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
import GitHub.Auth
import Data.Vector
import GitHub.Data.Repos
import GitHub.Data.Definitions
import qualified GitHub.Endpoints.Repos as GitHubRepos hiding(query)
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers
import Database.Bolt
import Data.Map
import GithubService
import qualified Data.ByteString.Char8 as BS


config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}

--API section
type API = "crawlUser" :> Capture "user" String :> Capture "authentication" String :> Get '[JSON] String
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

crawlGithubUser :: String -> String -> Handler String
crawlGithubUser user authentication = liftIO $ do
  result <- crawlUser (fromString user) authentication
  return user

crawlGithubCompany :: String -> Handler String
crawlGithubCompany company = liftIO $ do
  --crawl company
  return company

crawlGithubRepository :: String -> Handler String
crawlGithubRepository repository = liftIO $ do
  --crawl repository
  return repository
