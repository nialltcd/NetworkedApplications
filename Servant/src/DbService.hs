{-# LANGUAGE OverloadedStrings      #-}

module DbService where

import Database.Bolt hiding(pack, unpack)
import Data.String
import Data.Text
import Data.Map
import GitHub hiding(query)
import GitHub.Data.Repos
import GitHub.Data.Definitions
import qualified GitHub.Endpoints.Repos as GitHubRepos hiding(query)

-- Neo4J Config
config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}

-- CREATE (n:Person { name: 'Andres', title: 'Developer' })
insertUser :: String -> IO()
insertUser user = do
    logMsg ["Adding User: ", user, "\n"]
    pipe <- connect config
    result <- Database.Bolt.run pipe $ queryP "CREATE (n:User {name: {name}})" 
                              (fromList [("name", T (fromString user))])
    putStrLn $ show result
    close pipe


insertCompany :: String -> IO()
insertCompany company = do
    pipe <- connect config
    result <- run pipe $ query $ pack $ "MERGE (a:" ++ (getCompanyDb company) ++ ")"
    close pipe

insertRepo :: String -> (String, String) -> IO()
insertRepo node_type (owner_name, repo_name) = do
    logMsg ["Adding Repo: ", owner_name, "/", repo_name, "\n"]
    pipe <- connect config
    result <- run pipe $ query $ pack $ "MERGE (a:" ++ (getRepoNode owner_name repo_name) ++ ")"
    result <- run pipe $ query $ pack $ addLink (getNode node_type owner_name) (getRepoNode owner_name repo_name) "OWNER"
    close pipe

getRepoNode :: String -> String -> String
getRepoNode owner name = "Repo {owner: '" ++ owner ++ "', name: '" ++ name ++ "'}"

getNode :: String -> String -> String
getNode node_type name = node_type ++ " {name: '" ++ name ++ "'}"

addLink :: String -> String -> String -> String
addLink a b link = "MATCH (a:" ++ a ++ ") MATCH (b:" ++ b ++ ") MERGE (a)-[l:" ++ link ++ "]->(b)"

-- Utility Functions
logMsg :: [String] -> IO()
logMsg = mapM_ putStr

getCompanyDb :: String -> String
getCompanyDb name = "Org {name: '" ++ name ++ "'}"
