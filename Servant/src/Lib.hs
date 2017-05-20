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
import qualified Data.ByteString.Char8 as BS


config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}

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

crawlGithubUser :: String -> Handler String
crawlGithubUser user = liftIO $ do
  return user

crawlGithubCompany :: String -> Handler String
crawlGithubCompany company = liftIO $ do
  return company

crawlGithubRepository :: String -> Handler String
crawlGithubRepository repository = liftIO $ do
  return repository

crawlGithub :: String -> Handler String
crawlGithub user = liftIO $ do
  -- repos <- getRepos (fromString user)
  userinfo <- getUserInfo (fromString user)
  --addUser user
  return user


getUserInfo :: Text -> IO User
getUserInfo name = do
    let user = GitHub.mkUserName name
    request <- GitHubUsers.userInfoFor user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return result


crawlUser :: Text -> String -> IO (Vector (String, String))
crawlUser user token = do
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ token
    repos <- getUserRepos user auth
    --result <- Data.Vector.mapM addRepo repos
    --result_two <- Data.Vector.mapM (crawlRepo auth) repos
    return repos

getUserRepos :: Text -> Maybe Auth -> IO (Vector (String, String))
getUserRepos name auth = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos' auth owner RepoPublicityPublic
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result


formatRepo :: Repo -> (String, String)
formatRepo repo = do
    let owner = GitHubRepos.repoOwner repo
    let owner_name = untagName $ simpleOwnerLogin owner
    let repo_name = untagName $ GitHubRepos.repoName repo
    (Data.Text.unpack owner_name, Data.Text.unpack repo_name)
