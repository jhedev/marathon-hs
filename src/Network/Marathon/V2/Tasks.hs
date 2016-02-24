{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Marathon.V2.Tasks where

import Data.Aeson
import Data.Text (Text)
import Servant.API

type Api = Get '[JSON] Text
