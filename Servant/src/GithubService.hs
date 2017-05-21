{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings      #-}

module GithubService where

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
import DbService

crawlGithubForUserData :: Text -> String -> IO (Vector (String, String))
crawlGithubForUserData user authentication = do
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ authentication
    repos <- getRepos user auth
    --result <- Data.Vector.mapM addRepo repos
    --result_two <- Data.Vector.mapM (crawlRepo auth) repos
    return repos

getRepos :: Text -> Maybe Auth -> IO (Vector (String, String))
getRepos name auth = do
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
