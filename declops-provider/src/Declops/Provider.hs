{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider
  ( module Declops.Provider,
    module Declops.Provider.ProviderName,
    module Declops.Provider.ResourceName,
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
import Data.String
import Data.Validity
import Declops.Provider.ProviderName
import Declops.Provider.ResourceName
import GHC.Generics (Generic)
import Path
import System.Exit
import UnliftIO

type JSONProvider = Provider JSON.Value JSON.Value

toJSONProvider ::
  ( FromJSON specification,
    FromJSON output,
    ToJSON output
  ) =>
  Provider specification output ->
  JSONProvider
toJSONProvider provider =
  let parseJSONOrErr :: FromJSON a => JSON.Value -> P a
      parseJSONOrErr value = case JSON.parseEither parseJSON value of
        Left err -> liftIO $ die err
        Right result -> pure result
   in Provider
        { providerName = providerName provider,
          providerQuery = \resourceName -> do
            fmap toJSON <$> providerQuery provider resourceName,
          providerApply = \resourceName specificationJSON -> do
            specification <- parseJSONOrErr specificationJSON
            fmap toJSON <$> providerApply provider resourceName specification,
          providerCheck = \resourceName specificationJSON -> do
            specification <- parseJSONOrErr specificationJSON
            fmap toJSON <$> providerCheck provider resourceName specification,
          providerDestroy = \resourceName -> do
            providerDestroy provider resourceName
        }

instance HasCodec () where
  codec = Autodocodec.object "Unit" $ pure ()

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
-- A provider has two type parameters:
--
-- * An specification type, to declaratively specify what the resource should look like.
-- * An output type, to contain all the information about the remote resource.
--
-- In this context "local" means "wherever declops is run" and "remote" means "in reality".
--
-- A provider contains:
--
-- * A name, to reference it.
-- * A query function, to find out if the resource with the given resource still exists remotely.
-- * An apply function, to apply the current specification to reality
-- * A check function, to find out if the remote resource still looks like what it should and works as it should.
-- * A destroy function, to destroy a resource
--
-- Each of these functions MUST be idempotent so that they can be retried.
-- Getting them all right is not an easy thing to do, which is why we provide a test suite.
data Provider specification output = Provider
  { providerName :: !ProviderName,
    providerQuery :: !(ResourceName -> P (QueryResult output)),
    providerApply :: !(ResourceName -> specification -> P (ApplyResult output)),
    providerCheck :: !(ResourceName -> specification -> P (CheckResult output)),
    providerDestroy :: !(ResourceName -> P DestroyResult)
  }
  deriving (Generic)

runProviderQuery ::
  Provider specification output ->
  ResourceName ->
  LoggingT IO (QueryResult output)
runProviderQuery provider resourceName =
  runPCatchingExceptionsWith QueryFailure $
    providerQuery provider resourceName

runProviderApply ::
  Provider specification output ->
  ResourceName ->
  specification ->
  LoggingT IO (ApplyResult output)
runProviderApply provider resourceName specification =
  runPCatchingExceptionsWith ApplyFailure $
    providerApply provider resourceName specification

runProviderCheck ::
  Provider specification output ->
  ResourceName ->
  specification ->
  LoggingT IO (CheckResult output)
runProviderCheck provider resourceName specification =
  runPCatchingExceptionsWith CheckFailure $
    providerCheck provider resourceName specification

runProviderDestroy ::
  Provider specification output ->
  ResourceName ->
  LoggingT IO DestroyResult
runProviderDestroy provider resourceName =
  runPCatchingExceptionsWith DestroyFailure $
    providerDestroy provider resourceName

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

instance Validity (Provider specification output) where
  validate = trivialValidation

data RemoteState output
  = DoesNotExistRemotely
  | ExistsRemotely !output
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

type JSONQueryResult = QueryResult JSON.Value

data QueryResult output
  = QuerySuccess !(RemoteState output)
  | QueryFailure !ProviderException
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

type JSONApplyResult = ApplyResult JSON.Value

data ApplyResult output
  = ApplySuccess !output
  | ApplyFailure !ProviderException
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

applyFailed :: ApplyResult output -> Bool
applyFailed = \case
  ApplySuccess _ -> False
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
