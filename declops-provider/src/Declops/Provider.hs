{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider
  ( module Declops.Provider,
    liftIO,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Functor.Identity
import Data.Validity
import Declops.Provider.ProviderName
import GHC.Generics (Generic)
import Path
import System.Exit

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
    providerQuery :: !(reference -> P (RemoteState output)),
    providerApply :: !(specification -> ApplyContext reference output -> P (ApplyResult reference output)),
    providerCheck :: !(specification -> reference -> P (CheckResult output)),
    providerDestroy :: !(reference -> P DestroyResult)
  }
  deriving (Generic)

newtype P a = P {unP :: LoggingT IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

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
  | ApplyFailure !String
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
  | CheckFailure !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

checkFailed :: CheckResult output -> Bool
checkFailed = \case
  CheckSuccess _ -> False
  CheckFailure _ -> True

data DestroyResult
  = DestroySuccess
  | DestroyFailure !String
  deriving (Show, Eq, Generic)

destroyFailed :: DestroyResult -> Bool
destroyFailed = \case
  DestroySuccess -> False
  DestroyFailure _ -> True
