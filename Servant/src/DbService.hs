import Database.Bolt
import Data.Map

-- Neo4J Config
config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}

addUser :: String -> IO()
addUser user = do
    pipe <- connect config
    result <- Database.Bolt.run pipe $ queryP "CREATE (n:User {name: {name}})" 
                              (fromList [("name", T (fromString user))])
    putStrLn $ show result
    close pipe
