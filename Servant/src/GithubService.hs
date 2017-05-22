{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings      #-}

module GithubService where

import Data.Text
import Data.Vector
import Data.String
import GitHub
import GitHub.Auth
import GitHub.Data
import qualified Data.ByteString.Char8 as BS
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowersS
import DbService


crawlGithubForUserData :: Text -> String -> IO (Vector (String, String))
crawlGithubForUserData user authentication = do
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ authentication
    repos <- crawlGithubForReposOfUser user auth
    result <- Data.Vector.mapM (insertRepo "User") repos
    result_two <- Data.Vector.mapM (crawlGithubForRepoContributors auth) repos
    result_three <- Data.Vector.mapM (crawlGitHubForRepoForks auth) repos
    return repos


--Repositories
crawlGithubForReposOfUser :: Text -> Maybe Auth -> IO (Vector (String, String))
crawlGithubForReposOfUser name auth = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos' auth owner RepoPublicityPublic
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result


--Contributors
crawlGithubForRepoContributors :: Maybe Auth -> (String, String) -> IO (Vector String)
crawlGithubForRepoContributors auth (owner, repo) = do
    logMsg ["Crawling repo: ", owner, "/", repo, "\n"]
    contributors <- crawlRepoContributorsByOwnerAndRepo (owner, repo) auth
    result <- Data.Vector.mapM (insertContributor (owner, repo)) contributors
    return contributors


crawlRepoContributorsByOwnerAndRepo :: (String, String) -> Maybe Auth -> IO (Vector String)
crawlRepoContributorsByOwnerAndRepo (owner, repo) auth = do
    let github_owner = GitHub.mkOwnerName $ fromString owner
    let github_repo = GitHub.mkRepoName $ fromString repo
    request <- GitHubRepos.contributors' auth github_owner github_repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatContributor (Data.Vector.take 25 result)

formatContributor :: Contributor -> String
formatContributor (KnownContributor contributions avatarUrl name url uid gravatar) = unpack $ untagName name

formatRepo :: Repo -> (String, String)
formatRepo repo = do
    let owner = GitHubRepos.repoOwner repo
    let owner_name = untagName $ simpleOwnerLogin owner
    let repo_name = untagName $ GitHubRepos.repoName repo
    (Data.Text.unpack owner_name, Data.Text.unpack repo_name)

--Watchers
crawlGithubForRepoWatchers :: Maybe Auth -> (String, String) -> IO (Vector String)
crawlGithubForRepoWatchers auth (owner, repo) = do
    logMsg ["Crawling repo: ", owner, "/", repo, "\n"]
    watchers <- crawlRepoWatchers (owner, repo) auth
    result <- Data.Vector.mapM (insertWatcher (owner, repo)) watchers
    return contributors

crawlRepoWatchers :: (String, String) -> Maybe Auth -> IO (Vector String)
crawlRepoWatchers (owner, repo) auth = do
    let github_owner = GitHub.mkOwnerName $ fromString owner
    let github_repo = GitHub.mkRepoName $ fromString repo
    request <- GitHub.repoWatchers auth github_owner github_repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatWatcher (Data.Vector.take 25 result)

formatWatcher :: GitHub.Owner -> String
formatWatcher user = untagName $ user


