{-# LANGUAGE DeriveGeneric #-}

module Declops.Provider where

import Data.Text (Text)
import GHC.Generics (Generic)

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
  { providerName :: !Text,
    providerQuery :: !(reference -> IO (RemoteState output)),
    providerApply :: !(specification -> ApplyContext reference output -> IO (ApplyResult reference output)),
    providerCheck :: !(specification -> output -> IO CheckResult),
    providerDestroy :: !(reference -> RemoteState output -> IO DestroyResult)
  }
  deriving (Generic)

data LocalState reference
  = DoesNotExistLocally
  | ExistsLocally !reference
  deriving (Show, Eq, Generic)

data RemoteState output
  = DoesNotExistRemotely
  | ExistsRemotely !output
  deriving (Show, Eq, Generic)

data ApplyContext reference output
  = DoesNotExistLocallyNorRemotely
  | ExistsLocallyButNotRemotely !reference
  | ExistsLocallyAndRemotely !reference !output
  deriving (Show, Eq, Generic)

data ApplyResult reference output
  = ApplySuccess !reference !output
  | ApplyFailure !String
  deriving (Show, Eq, Generic)

data CheckResult
  = CheckSuccess
  | CheckFailure !String
  deriving (Show, Eq, Generic)

data DestroyResult
  = DestroySuccess
  | DestroyFailure !String
  deriving (Show, Eq, Generic)
