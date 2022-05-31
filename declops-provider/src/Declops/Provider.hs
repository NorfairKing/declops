{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider
  ( module Declops.Provider,
    liftIO,
    module Control.Monad.Logger,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Exception (AsyncException)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Functor.Identity
import Data.String
import Data.Validity
import Declops.Provider.ProviderName
import GHC.Generics (Generic)
import Path
import System.Exit
import UnliftIO

type JSONProvider = Provider JSON.Value JSON.Value JSON.Value

toJSONProvider ::
  ( FromJSON specification,
    FromJSON reference,
    ToJSON reference,
    FromJSON output,
    ToJSON output
  ) =>
  Provider specification reference output ->
  JSONProvider
toJSONProvider provider =
  let parseJSONOrErr :: FromJSON a => JSON.Value -> P a
      parseJSONOrErr value = case JSON.parseEither parseJSON value of
        Left err -> liftIO $ die err
        Right result -> pure result
   in Provider
        { providerName = providerName provider,
          providerQuery = \referenceJSON -> do
            reference <- parseJSONOrErr referenceJSON
            fmap toJSON <$> providerQuery provider reference,
          providerApply = \specificationJSON applyContextJSON -> do
            specification <- parseJSONOrErr specificationJSON
            applyContext <- bimapApplyContext parseJSONOrErr parseJSONOrErr applyContextJSON
            dimapApplyResult toJSON toJSON
              <$> providerApply provider specification applyContext,
          providerCheck = \specificationJSON referenceJSON -> do
            reference <- parseJSONOrErr referenceJSON
            specification <- parseJSONOrErr specificationJSON
            fmap toJSON <$> providerCheck provider specification reference,
          providerDestroy = \referenceJSON -> do
            reference <- parseJSONOrErr referenceJSON
            providerDestroy provider reference
        }

instance HasCodec (Path Rel Dir) where
  codec = bimapCodec (left show . parseRelDir) fromRelDir codec

instance HasCodec (Path Rel File) where
  codec = bimapCodec (left show . parseRelFile) fromRelFile codec

instance HasCodec (Path Abs Dir) where
  codec = bimapCodec (left show . parseAbsDir) fromAbsDir codec

instance HasCodec (Path Abs File) where
  codec = bimapCodec (left show . parseAbsFile) fromAbsFile codec

-- | A provider for a resource.
--
-- A provider has three type parameters:
--
-- * An specification type, to declaratively specify what the resource should look like.
-- * A reference type, to refer to the resource in the local declops database.
-- * An output type, to contain all the information about the remote resource.
--
-- In this context "local" means "wherever declops is run" and "remote" means "in reality".
--
-- A provider contains:
--
-- * A name, to reference it.
-- * A query function, to find out if the resource with the given local reference still exists remotely.
-- * An apply function, to apply the current specification to reality
-- * A check function, to find out if the remote resource still looks like what it should and works as it should.
-- * A destroy function, to destroy a resource
--
-- Each of these functions MUST be idempotent so that they can be retried.
-- Getting them all right is not an easy thing to do, which is why we provide a test suite.
data Provider specification reference output = Provider
  { providerName :: !ProviderName,
    providerQuery :: !(reference -> P (QueryResult output)),
    providerApply :: !(specification -> ApplyContext reference output -> P (ApplyResult reference output)),
    providerCheck :: !(specification -> reference -> P (CheckResult output)),
    providerDestroy :: !(reference -> P DestroyResult)
  }
  deriving (Generic)

runProviderQuery ::
  Provider specification reference output ->
  reference ->
  LoggingT IO (QueryResult output)
runProviderQuery provider reference =
  runPCatchingExceptionsWith QueryFailure $
    providerQuery provider reference

runProviderApply ::
  Provider specification reference output ->
  specification ->
  ApplyContext reference output ->
  LoggingT IO (ApplyResult reference output)
runProviderApply provider specification applyContext =
  runPCatchingExceptionsWith ApplyFailure $
    providerApply provider specification applyContext

runProviderCheck ::
  Provider specification reference output ->
  specification ->
  reference ->
  LoggingT IO (CheckResult output)
runProviderCheck provider specification reference =
  runPCatchingExceptionsWith CheckFailure $
    providerCheck provider specification reference

runProviderDestroy ::
  Provider specification reference output ->
  reference ->
  LoggingT IO DestroyResult
runProviderDestroy provider reference =
  runPCatchingExceptionsWith DestroyFailure $
    providerDestroy provider reference

newtype P a = P {unP :: LoggingT IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

throwP :: Exception e => e -> P a
throwP = P . liftIO . throwIO

instance MonadFail P where
  fail = throwP . ProviderException

runPCatchingExceptionsWith :: (ProviderException -> a) -> P a -> LoggingT IO a
runPCatchingExceptionsWith wrapper func =
  unP func `catches` exceptionHandlersWith wrapper

exceptionHandlersWith :: MonadIO m => (ProviderException -> a) -> [Handler m a]
exceptionHandlersWith wrapper =
  [ -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
    Handler (\e -> throwIO (e :: AsyncException)),
    -- Catch ProviderExceptions specifically
    Handler (\e -> return (wrapper e)),
    -- Catch all the rest as well
    Handler (\e -> return $ wrapper (ProviderException (displayException (e :: SomeException))))
  ]

instance Validity (Provider specification reference output) where
  validate = trivialValidation

data LocalState reference
  = DoesNotExistLocally
  | ExistsLocally !reference
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

data RemoteState output
  = DoesNotExistRemotely
  | ExistsRemotely !output
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

type JSONQueryResult = QueryResult JSON.Value

data QueryResult output
  = QuerySuccess !(RemoteState output)
  | QueryFailure !ProviderException
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

type JSONApplyContext = ApplyContext JSON.Value JSON.Value

data ApplyContext reference output
  = DoesNotExistLocallyNorRemotely
  | ExistsLocallyButNotRemotely !reference
  | ExistsLocallyAndRemotely !reference !output
  deriving (Show, Eq, Generic)

dimapApplyContext ::
  (reference1 -> reference2) ->
  (output1 -> output2) ->
  ApplyContext reference1 output1 ->
  ApplyContext reference2 output2
dimapApplyContext referenceFunc outputFunc = runIdentity . bimapApplyContext (pure . referenceFunc) (pure . outputFunc)

bimapApplyContext ::
  Applicative f =>
  (reference1 -> f reference2) ->
  (output1 -> f output2) ->
  ApplyContext reference1 output1 ->
  f (ApplyContext reference2 output2)
bimapApplyContext referenceFunc outputFunc = \case
  DoesNotExistLocallyNorRemotely -> pure DoesNotExistLocallyNorRemotely
  ExistsLocallyButNotRemotely reference -> ExistsLocallyButNotRemotely <$> referenceFunc reference
  ExistsLocallyAndRemotely reference output -> ExistsLocallyAndRemotely <$> referenceFunc reference <*> outputFunc output

type JSONApplyResult = ApplyResult JSON.Value JSON.Value

data ApplyResult reference output
  = ApplySuccess !reference !output
  | ApplyFailure !ProviderException
  deriving (Show, Eq, Generic)

dimapApplyResult ::
  (reference1 -> reference2) ->
  (output1 -> output2) ->
  ApplyResult reference1 output1 ->
  ApplyResult reference2 output2
dimapApplyResult referenceFunc outputFunc = runIdentity . bimapApplyResult (pure . referenceFunc) (pure . outputFunc)

bimapApplyResult ::
  Applicative f =>
  (reference1 -> f reference2) ->
  (output1 -> f output2) ->
  ApplyResult reference1 output1 ->
  f (ApplyResult reference2 output2)
bimapApplyResult referenceFunc outputFunc = \case
  ApplySuccess reference output -> ApplySuccess <$> referenceFunc reference <*> outputFunc output
  ApplyFailure err -> pure $ ApplyFailure err

applyFailed :: ApplyResult reference output -> Bool
applyFailed = \case
  ApplySuccess _ _ -> False
  ApplyFailure _ -> True

type JSONCheckResult = CheckResult JSON.Value

data CheckResult output
  = CheckSuccess !output
  | CheckFailure !ProviderException
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

checkFailed :: CheckResult output -> Bool
checkFailed = \case
  CheckSuccess _ -> False
  CheckFailure _ -> True

data DestroyResult
  = DestroySuccess
  | DestroyFailure !ProviderException
  deriving (Show, Eq, Generic)

destroyFailed :: DestroyResult -> Bool
destroyFailed = \case
  DestroySuccess -> False
  DestroyFailure _ -> True

newtype ProviderException = ProviderException {unProviderException :: String}
  deriving stock (Generic)
  deriving newtype (Show, Read, Eq, IsString)

instance Exception ProviderException where
  displayException = unProviderException
