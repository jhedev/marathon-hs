{-# LANGUAGE OverloadedStrings #-}
module Network.Marathon.V2.Apps where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)

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

instance FromJSON AppConstraint where
  parseJSON o = parseJSON o >>= return . AppConstraint

data ContainerVolume = ContainerVolume
  { volContainerPath :: Text
  , volHostPath :: Text
  , volMode :: Text -- TODO: Better typing
  } deriving (Show, Read, Eq)

instance FromJSON ContainerVolume where
  parseJSON (Object o) = ContainerVolume
    <$> o .: "containerPath"
    <*> o .: "hostPath"
    <*> o .: "mode"
  parseJSON _ = fail "Expecting a JSON object"

data ContainerType = Docker deriving (Show, Read, Eq)

instance FromJSON ContainerType where
  parseJSON (String "Docker") = return Docker
  parseJSON (String _) = fail "Unsupported container type"
  parseJSON _ = fail "Expecting a JSON String for container type"

data Container = Container -- TODO: Currently incomplete
  { containerType :: ContainerType -- TODO: Better typing
  , containerVolumens :: [ContainerVolume]
  } deriving (Show, Read, Eq)

instance FromJSON Container where
  parseJSON (Object o) = Container
    <$> o .: "type"
    <*> o .: "volumes"

data HealthCheck = HealthCheck -- TODO: check which attributes are optional
  { healthCheckCommand                :: Text -- TODO: Incomplete type
  , healthCheckGracePeriodSeconds     :: Int
  , healthCheckIgnoreHttp1xx          :: Bool
  , healthCheckIntervalSeconds        :: Int
  , healthCheckMaxConsecutiveFailures :: Int
  , healthCheckPath                   :: Text
  , healthCheckProtocol               :: Text
  , healthCheckTimoutSeconds          :: Text
  } deriving (Show, Read, Eq)

instance FromJSON HealthCheck where
  parseJSON (Object o) = HealthCheck
    <$> o .: "command"
    <*> o .: "gracePeriodSeconds"
    <*> o .: "ignoreHttp1xx"
    <*> o .: "intervalSeconds"
    <*> o .: "maxConsecutiveFailures"
    <*> o .: "path"
    <*> o .: "protocol"
    <*> o .: "timeoutSeconds"

newtype Dependency = Dependency Text deriving (Show, Read, Eq)

instance FromJSON Dependency where
  parseJSON (String s) = return . Dependency $ s
  parseJSON _ = fail "Expecting a JSON string for app dependencies"

data UpgradeStrategy = UpgradeStrategy
  { stratMaximumOverCapacity   :: Double
  , stratMinimumHealthCapacity :: Double
  } deriving (Show, Read, Eq)

instance FromJSON UpgradeStrategy where
  parseJSON (Object o) = UpgradeStrategy
    <$> o .: "maximumOverCapacity"
    <*> o .: "minimumHealthCapacity"

newtype Labels = Labels (HMap.HashMap Text Text) deriving (Show, Read, Eq)

instance FromJSON Labels where
  parseJSON (Object o) = return . Labels $
    HMap.map (\(String s) -> s) o
  parseJSON _ = fail "Expecting a JSON object for Labels"

data VersionInfo = VersionInfo
  { versionInfoLastConfigChangeAt :: Text
  , versionInfoLastScalingAt      :: Text
  } deriving (Show, Read, Eq)

instance FromJSON VersionInfo where
  parseJSON (Object o) = VersionInfo
    <$> o .: "lastconfigChangeAt"
    <*> o .: "lastScalingAt"
  parseJSON _ = fail "Expecting a JSON object for version info"

data Deployment = Deployment
  { deploymentId :: Text
  } deriving (Show, Read, Eq)

instance FromJSON Deployment where
  parseJSON (Object o) = Deployment
    <$> o .: "id"
  parseJSON _ = fail "Expecting a JSON object for deployment"

data StatsGroup = StatsGroup
  { statsGrpCountsStaged       :: Int
  , statsGrpCountsRunning      :: Int
  , statsGrpCountsHealthy      :: Int
  , statsGrpCountsUnhealthy    :: Int
  , statsGrpLifeTimeAvgSecs    :: Double
  , statsGrpLifeTimeMedianSecs :: Double
  } deriving (Show, Read, Eq)

instance FromJSON StatsGroup where
  parseJSON (Object stats) = do
    o <- stats .: "stats"
    counts   <- o .: "counts"
    lifeTime <- o .: "lifeTime"
    StatsGroup
      <$> counts   .: "staged"
      <*> counts   .: "running"
      <*> counts   .: "healthy"
      <*> counts   .: "unhealthy"
      <*> lifeTime .: "averageSeconds"
      <*> lifeTime .: "medianSeconds"
  parseJSON _ = fail "Expecting a JSON object for stats groups in taskStats"

data TaskStats = TaskStats
  { statsWithLatestConfig        :: Maybe StatsGroup
  , statsStartedAfterLastScaling :: Maybe StatsGroup
  , statsWithOutdatedConfig      :: Maybe StatsGroup
  , statsTotalSummary            :: Maybe StatsGroup
  } deriving (Show, Read, Eq)

instance FromJSON TaskStats where
  parseJSON (Object o) = TaskStats
    <$> o .:? "startedAfterLastScaling"
    <*> o .:? "withLatestConfig"
    <*> o .:? "withOutdatedConfig"
    <*> o .:? "totalSummary"

data App = App
  { appId                    :: AppId
  , appAcceptedResourceRoles :: Maybe [Text]
  , appArgs                  :: Maybe Text
  , appBackoffFactor         :: Double
  , appBackoffSeconds        :: Int
  , appCmd                   :: Text
  , appConstraints           :: [AppConstraint]
  , appContainer             :: Maybe Container
  , appCPUs                  :: Double
  , appDeployments           :: Maybe [Deployment]
  , appDeps                  :: [Dependency]
  , appDisk                  :: Int
  , appEnv                   :: AppEnv
  , appExecutor              :: Text
  , appHeathChecks           :: [HealthCheck]
  , appInstances             :: Int
  , appLabels                :: Labels
  , appLastTaskFailure       :: Maybe Text -- TODO: Create new data type
  , appMaxLaunchDelaySecs    :: Int
  , appMem                   :: Int
  , appPorts                 :: [Int]
  , appRequirePorts          :: Bool
  , appStoreURLs             :: [Text]
  , appTasksHealthy          :: Maybe Int
  , appTasksRunning          :: Maybe Int
  , appTasksStaged           :: Maybe Int
  , appTaskStats             :: Maybe TaskStats
  , appTasksUnhealthy        :: Maybe Int
  , appUpgradeStrategy       :: UpgradeStrategy
  , appURIs                  :: [Text]
  , appUser                  :: Maybe Text
  , appVersion               :: Text -- TODO: Make it a shared version type based on UTCTime
  , appVersionInfo           :: VersionInfo -- TODO: add version info type
  } deriving (Show, Read, Eq)

instance FromJSON App where
  parseJSON (Object o) = App
    <$> o .: "id"
    <*> o .:? "acceptedResourceRoles"
    <*> o .:? "args"
    <*> o .: "backoffFactor"
    <*> o .: "backoffSeconds"
    <*> o .: "cmd"
    <*> o .: "constraints"
    <*> o .:? "container"
    <*> o .: "cpus"
    <*> o .:? "deployments"
    <*> o .: "dependencies"
    <*> o .: "disk"
    <*> o .: "env"
    <*> o .: "executor"
    <*> o .: "healthChecks"
    <*> o .: "instances"
    <*> o .: "labels"
    <*> o .:? "lastTaskFailure"
    <*> o .: "maxLaunchDelaySeconds"
    <*> o .: "mem"
    <*> o .: "ports"
    <*> o .: "requirePorts"
    <*> o .: "storeUrls"
    <*> o .:? "tasksHealthy"
    <*> o .:? "tasksRunning"
    <*> o .:? "tasksStaged"
    <*> o .:? "taskStats"
    <*> o .:? "tasksUnhealthy"
    <*> o .: "upgradeStrategy"
    <*> o .: "uris"
    <*> o .:? "user"
    <*> o .: "version"
    <*> o .: "versionInfo"

newtype Apps = Apps [App]
    deriving (Show, Read, Eq)

instance FromJSON Apps where
  parseJSON (Object o) = Apps <$> (o .: "apps")
  parseJSON _ = mzero

newtype CmdFilter = CmdFilter Text deriving (Show, Read, Eq)
newtype IdFilter = IdFilter Text deriving (Show, Read, Eq)
newtype LabelSelector = LabelSelector Text deriving (Show, Read, Eq)

data Embed = EmbedTasks
           | EmbedCounts
           | EmbedDeployments
           | EmbedLastTaskFailure
           | EmbedTaskStats
          deriving (Show, Read, Eq)
