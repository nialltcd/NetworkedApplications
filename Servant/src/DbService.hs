module DbService where

import Database.Bolt
import Data.Map

-- Neo4J Config
config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}

-- CREATE (n:Person { name: 'Andres', title: 'Developer' })
insertUser :: String -> IO()
insertUser user = do
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


getCompanyDb :: String -> String
getCompanyDb name = "Org {name: '" ++ name ++ "'}"
