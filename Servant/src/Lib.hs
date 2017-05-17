{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Text
import Data.String
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import GitHub
import GitHub.Data.Repos
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers


type API = "crawl" :> Capture "user" String :> Get '[JSON] String


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = crawlGithub


crawlGithub :: String -> Handler String
crawlGithub user = liftIO $ do
  -- repos <- getRepos (fromString user)
  userinfo <- getUserInfo (fromString user)
  --addUser user
  return userinfo


getUserInfo :: Text -> IO User
getUserInfo name = do
    let user = GitHub.mkUserName name
    request <- GitHubUsers.userInfoFor user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return result