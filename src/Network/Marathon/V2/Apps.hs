{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Marathon.V2.Apps where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import Servant.API

newtype AppId = AppId Text deriving (Show, Read, Eq)

instance FromJSON AppId where
  parseJSON (String s) = return . AppId $ s
  parseJSON _ = fail "Expecting a JSON string"

newtype AppEnv = AppEnv (HMap.HashMap Text Text) deriving (Show, Read, Eq)

instance FromJSON AppEnv where
  -- TODO: Handle Values other than string
  parseJSON (Object o) = return . AppEnv $ HMap.map (\(String s) -> s) o
  parseJSON _ = fail "Expecting a JSON object"

-- TODO: Better Haskell representation
newtype AppConstraint = AppConstraint [Text] deriving (Show, Read, Eq)

data App = App
  { appId          :: AppId
  , appCmd         :: Text
  , appArgs        :: Maybe Text
  , appUser        :: Maybe Text
  , appEnv         :: AppEnv
  , appInstances   :: Int
  , appCPUs        :: Double
  , appMem         :: Int
  , appDisk        :: Int
  , appExecutor    :: Text
  , appConstraints :: [Text]
  , appURIs        :: [Text]
  , appStoreURLs   :: [Text]
  , appPorts       :: [Int]
  , appRequirePorts :: Bool
  , appBackoffSeconds :: Int
  , appBackoffFactor :: Double
  , appMaxLaunchDelaySecs :: Int
  , appContainer :: Maybe Text -- TODO: Add Container data type
  , appHeathChecks :: [Text] -- TODO: Add HeathCheck data type
  , appDeps :: [Text] -- TODO: Add Deps data type
  , appUpgradeStrategy :: Text -- TODO: Add UpgradeStrategy data type
  , appLabels :: [Text] -- TODO: Store using a HashMap
  , appAcceptedResourceRoles :: Maybe Text
  , appVersion :: Text -- TODO: Make it a shared version type based on UTCTime
  , appVersionInfo :: Text -- TODO: add version info type
  , appTasksStaged :: Int
  , appTasksRunning :: Int
  , appTasksHealthy :: Int
  , appTasksUnhealthy :: Int
  , appDeployments :: [Text] -- TODO: Needs its own type
  } deriving (Show, Read, Eq)

instance FromJSON App where
  parseJSON (Object o) = App
    <$> o .: "id"
    <*> o .: "cmd"
    <*> o .:? "args"
    <*> o .:? "user"
    <*> o .: "env"
    <*> o .: "instances"
    <*> o .: "cpus"
    <*> o .: "mem"
    <*> o .: "disk"
    <*> o .: "executor"
    <*> o .: "constraints"
    <*> o .: "uris"
    <*> o .: "storeUrls"
    <*> o .: "ports"
    <*> o .: "requirePorts"
    <*> o .: "backoffSeconds"
    <*> o .: "backoffFactor"
    <*> o .: "maxLaunchDelaySeconds"
    <*> o .:? "container"
    <*> o .: "healthChecks"
    <*> o .: "dependencies"
    <*> o .: "upgradeStrategy"
    <*> o .: "labels"
    <*> o .:? "acceptedResourceRoles"
    <*> o .: "version"
    <*> o .: "versionInfo"
    <*> o .: "tasksStaged"
    <*> o .: "tasksRunning"
    <*> o .: "tasksHealthy"
    <*> o .: "tasksUnhealthy"
    <*> o .: "deployments"

newtype Apps = Apps [App]
    deriving (Show, Read, Eq)

instance FromJSON Apps where
  parseJSON (Object o) = Apps <$> (o .: "apps")
  parseJSON _ = mzero

type Api = Get '[JSON] Apps
