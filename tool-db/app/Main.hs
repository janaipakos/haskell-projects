module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import qualified System.Process as SP --for system function
import System.Exit -- for the IO ExitCode


{-  1. Create DB with sqlite3
    2. Model tables in this file
    3. Create instances of show
    4. Create way to add resources with addResource
    5. Create way to print users 
-}

--model tool and user after db fields
data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

data User = User
    { userId :: Int
    , userName :: String
    }

--get info from db
instance FromRow User where 
    fromRow = User <$> field 
                   <*> field
                   
instance FromRow Tool where 
    fromRow = Tool <$> field 
                   <*> field
                   <*> field 
                   <*> field 
                   <*> field

--instance of both as strings
instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user
                        ]

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
                        , "\n"
                        ]

--automatically handles opening and closing database
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn

--execute runs a SQL command, and the (?)'s match the String values
addUser :: String -> IO ()
addUser userName = withConn "tools.db" $
                   \conn -> do
                     execute conn "INSERT INTO users (username) VALUES (?)"
                       (Only userName)
                     print "user added"

addTool :: String -> String -> String -> Int -> IO ()
addTool name description lastReturned timesBorrowed = withConn "tools.db" $
                \conn -> do
                  execute conn "INSERT INTO tools (name,description,lastReturned,timesBorrowed) VALUES (?,?,?,?)"
                    (name, description, lastReturned, timesBorrowed)
                  print "tool added"

--give id of user and the tool id they checkout
checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                            \conn -> do
                                execute conn
                                  "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
                                  (userId,toolId)

printUsers :: IO ()
printUsers = withConn "tools.db" $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM users;" :: IO [User]
               mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q =  withConn "tools.db" $
                         \conn ->  do
                           resp <- query_ conn q :: IO [Tool]
                           mapM_ print resp


printTools :: IO ()
printTools =  printToolQuery "SELECT * FROM tools;" 

--what tool is available
printAvailable :: IO ()
printAvailable = printToolQuery $
                 mconcat ["select * from tools "
                         ,"where id not in "
                         ,"(select tool_id from checkedout);"]

--what tool is checked out
printCheckedout :: IO ()
printCheckedout = printToolQuery $
                  mconcat ["select * from tools "
                          ,"where id in "
                          ,"(select tool_id from checkedout);"]

--DB operations are I/O and Maybe catches incorrect ids
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn
            "SELECT * FROM tools where id = (?)"
            (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp

--returns the id of tool from above
firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = Tool
    { toolId = toolId tool
    , name = name tool
    , description = description tool
    , lastReturned = date
    , timesBorrowed = 1 + (timesBorrowed tool)
    }

--update tool or show Nothing if invalid
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
                            \conn -> do
                                let q = mconcat ["UPDATE TOOLS SET "
                                                ,"lastReturned = ?,"
                                                ," timesBorrowed = ? "
                                                ,"WHERE ID = ?;"]
                                execute conn q ( lastReturned tool
                                               , timesBorrowed tool
                                               , toolId tool) 
                                print "tool updated"

--send a tool back to table and update info with checkout/date
updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                           \conn -> do
                             tool <- selectTool conn toolId
                             currentDay <- utctDay <$> getCurrentTime
                             let updatedTool = updateTool <$> tool
                                                          <*> pure currentDay
                             updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
                            \conn -> do
                              execute conn
                                "DELETE FROM checkedout WHERE tool_id = (?);"
                                (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

promptAndAddUser :: IO () 
promptAndAddUser = do
    print "Enter new user name" 
    userName <- getLine
    addUser userName

promptAndCheckout :: IO () 
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- pure read <*> getLine
    print "Enter the id of the tool"
    toolId <- pure read <*> getLine 
    checkout userId toolId

promptAndCheckin :: IO () 
promptAndCheckin = do
    print "enter the id of tool" 
    toolId <- pure read <*> getLine 
    checkinAndUpdate toolId

performCommand :: String -> IO () 
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedout >> main
    | command == "quit" = print "bye!"
    | otherwise = print "Sorry command not found" >> main

--Rebuild database!!!!!!!!
build :: IO ExitCode
build = do
    print "This will reset the DB, type yes if you're sure"
    answer <- getLine
    if answer == "yes"
    then SP.system "sqlite3 tools.db < build_db.sql"
    else SP.system "Goodbye"

main :: IO ()
main = do
    print "Enter a command" 
    command <- getLine 
    performCommand command
