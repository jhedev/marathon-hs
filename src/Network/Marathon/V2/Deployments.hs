{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Marathon.V2.Deployments where

import Data.Aeson
import Data.Text (Text)
import Servant.API

-- Deployments API

data Step = Step
  { stepAction :: Text
  , stepApp :: Text
  } deriving (Show, Read, Eq)

instance FromJSON Step where
  parseJSON (Object o) = Step
    <$> o .: "action"
    <*> o .: "app"
  parseJSON _ = fail "Expecting a JSON object"

newtype DeploymentId = DeploymentId Text deriving (Show, Read, Eq)

instance FromJSON DeploymentId where
  parseJSON (String s) = return $ DeploymentId s
  parseJSON _ = fail "Expecting a string"

data Deployment = Deployment
  { depId :: DeploymentId
  , depVersion :: Text
  , depAffectedApps :: [Text]
  , depSteps :: [Step]
  , debCurActions :: [Step]
  , debCurrentStep :: Int
  , debTotalSteps :: Int
  } deriving (Show, Read, Eq)

instance FromJSON Deployment where
  parseJSON (Object o) = Deployment
    <$> o .: "id"
    <*> o .: "version"
    <*> o .: "affectedApps"
    <*> o .: "steps"
    <*> o .: "currentActions"
    <*> o .: "currentStep"
    <*> o .: "totalSteps"
  parseJSON _ = fail "Expecting a JSON object"

newtype Deployments = Deployments [Deployment]
  deriving (Show, Read, Eq)

instance FromJSON Deployments where
  parseJSON v = parseJSON v >>= return . Deployments

type Api = Get '[JSON] Deployments
      :<|> Capture "deployment_id" DeploymentId :> Delete '[JSON] Text
