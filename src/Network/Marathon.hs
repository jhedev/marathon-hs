{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Marathon ( marathonApi
                        , MarathonApi
                        , module X
                        ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.API

import Network.Marathon.V2.Apps as X
import Network.Marathon.V2.Deployments as X
import Network.Marathon.V2.Groups as X
import Network.Marathon.V2.Tasks as X
import Network.Marathon.V2.Artifacts as X
import Network.Marathon.V2.Events as X
import Network.Marathon.V2.EventsSub as X
import Network.Marathon.V2.Info as X
import Network.Marathon.V2.Leader as X
import Network.Marathon.V2.Queue as X


-- Apps API
--
-- Action on all apps
type AllApps = Post '[JSON] App -- Create a new app
   :<|> QueryParam "id" IdFilter :> QueryParam "label" LabelSelector :>
        QueryParam "cmd" CmdFilter :> Get '[JSON] Apps -- Get all apps
-- Actions with app id
type AppById = Get '[JSON] App
   :<|> "versions" :> Capture "version" Text :> Get '[JSON] App
   :<|> Put '[JSON] App
   :<|> Post '[] () -- TODO: Get back json object with deployment id and version
   :<|> Delete '[] () -- TODO: same as above

--   :<|> "tasks" :> TaskApi
--type TaskApi = Get '[JSON] [Task]
--   :<|> QueryParam "host" Text :> QueryParam "scale" Bool :> Delete '[] [Task]
--   :<|> Capture "taskId" Text :> QueryParam "scale" Bool :> Delete '[] Task
type AppsApi = QueryParams "embed" Embed :> AllApps
  :<|> QueryParams "embed" :> Capture "appId" Text :> AppById


-- Deployments API
type DeploymentsApi = Get '[JSON] Deployments
      :<|> Capture "deployment_id" DeploymentId :> Delete '[JSON] Text

-- Groups API
type GroupsApi = Get '[JSON] Text

-- Tasks API
type TasksApi = Get '[JSON] Text

-- Artifacts API
type ArtifactsApi = Get '[JSON] Text

-- Events API
type EventsApi = Get '[JSON] Text

-- EventSubscriptions API
type EventSubsApi = Get '[JSON] Text

-- Info API
type InfoApi = Get '[JSON] Text

-- Leader API
type LeaderApi = Get '[JSON] Text

-- Queue API
type QueueApi = Get '[JSON] Text

type MarathonApiV2 = "apps" :> AppsApi
                :<|> "deployments" :> DeploymentsApi
                :<|> "groups" :> GroupsApi
                :<|> "tasks" :> TasksApi
                :<|> "artifacts" :> ArtifactsApi
                :<|> "events" :> EventsApi
                :<|> "eventSubscriptions" :> EventSubsApi
                :<|> "info" :> InfoApi
                :<|> "leader" :> LeaderApi
                :<|> "queue" :> QueueApi

type MarathonApi = "v2" :> MarathonApiV2
              :<|> "metrics" :> Get '[JSON] Text

marathonApi :: Proxy MarathonApi
marathonApi = Proxy
