module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data Tool = Tool
  { toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int }
 
data User = User
  { userId :: Int
  , userName :: String }
  
instance Show User where
  show user = mconcat [ show $ userId user
                      , ".)  "
                      , userName user]

instance Show Tool where
  show tool = mconcat [ show $ toolId tool
                      , ".) "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n"]

databaseName :: String
databaseName = "tools.db"

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn

addUser :: String -> IO ()
addUser userName = withConn databaseName $
  \conn -> do
    execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
    print "add user"

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn databaseName $
  \conn -> do
    execute conn "INSERT INTO checkout (user_id, tool_id) VALUE (?, ?)" (userId, toolId)

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

printUsers :: IO ()
printUsers = withConn databaseName $
  \conn -> do
    users <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print users

printToolQuery :: Query -> IO ()
printToolQuery q =  withConn databaseName $
  \conn ->  do
    tools <- query_ conn q :: IO [Tool]
    mapM_ print tools

printTools :: IO ()
printTools =  printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $
  mconcat [ "select * from tools "
          , "where id not in "
          , "(select tool_id from checkedout);" ]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
  mconcat [ "select * from tools "
          , "where id in "
          , "(select tool_id from checkedout);" ]

main :: IO ()
main = print "SQL"
