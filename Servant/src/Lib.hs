{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings      #-}

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
import Database.Bolt
import Data.Map

config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}

type API = "crawl" :> Capture "user" String :> Get '[JSON] String

type API = "crawlUser" :> Capture "user" String :> Get '[JSON] String
        :<|> "crawlCompany" :> Capture "company" String :> Get '[JSON] String
        :<|> "crawlRepository" :> Capture "repository" String :> Get '[JSON] String

startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = crawlGithubUser 
    :<|> crawlGithubCompany
    :<|> crawlGithubRepository


crawlGithub :: String -> Handler String
crawlGithub user = liftIO $ do
  -- repos <- getRepos (fromString user)
  userinfo <- getUserInfo (fromString user)
  addUser user
  return user


getUserInfo :: Text -> IO User
getUserInfo name = do
    let user = GitHub.mkUserName name
    request <- GitHubUsers.userInfoFor user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return result
