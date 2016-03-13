{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Marathon ( marathonApi
                        , MarathonApi
                        , module Apps
                        , module Deployments
                        , module Groups
                        , module Tasks
                        , module Artifacts
                        , module Events
                        , module EventsSub
                        , module Info
                        , module Leader
                        , module Queue
                        ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.API

import Network.Marathon.V2.Apps as Apps
import Network.Marathon.V2.Deployments as Deployments
import Network.Marathon.V2.Groups as Groups
import Network.Marathon.V2.Tasks as Tasks
import Network.Marathon.V2.Artifacts as Artifacts
import Network.Marathon.V2.Events as Events
import Network.Marathon.V2.EventsSub as EventsSub
import Network.Marathon.V2.Info as Info
import Network.Marathon.V2.Leader as Leader
import Network.Marathon.V2.Queue as Queue

type MarathonApiV2 = "apps" :> Apps.Api
                :<|> "deployments" :> Deployments.Api
                :<|> "groups" :> Groups.Api
                :<|> "tasks" :> Tasks.Api
                :<|> "artifacts" :> Artifacts.Api
                :<|> "events" :> Events.Api
                :<|> "eventSubscriptions" :> EventsSub.Api
                :<|> "info" :> Info.Api
                :<|> "leader" :> Leader.Api
                :<|> "queue" :> Queue.Api

type MarathonApi = "v2" :> MarathonApiV2
              :<|> "metrics" :> Get '[JSON] Text

marathonApi :: Proxy MarathonApi
marathonApi = Proxy
