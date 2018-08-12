{-# LANGUAGE OverloadedStrings #-}
{-| The version of MySQL-Haskell 0.8.3.0 only works properly with MySQL server 5.5-}
module MySQLConnector where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Int
import ModelTypes
import qualified Data.Text as T
import Data.List


selectAllQuery = "SELECT * FROM haskell_users"
selectByIdQuery = "SELECT * FROM haskell_users WHERE userId=(?)"
insertUserQuery = "INSERT INTO mysql.haskell_users (userId,userName) VALUES(?,?)"

getAllUsers :: IO ()
getAllUsers = do
    conn <- createConnection
    s <- prepareStmt conn selectAllQuery
    (defs, is) <- queryStmt conn s [MySQLInt32U 18]
    print =<< Streams.toList is

{-| For select we use [query] operator followed by the connection, query and a QueryParam-}
getUserById :: Int -> IO (Maybe User)
getUserById id = let userId = id in do
            conn <- createConnection
            (columnDef, inputStream) <- query conn selectByIdQuery [One $ MySQLInt32 (intToInt32 userId)]
            maybeMySQLValue <- Streams.read inputStream
            user <- do return (transformToUser <$> maybeMySQLValue)
            return user

transformToUser :: [MySQLValue] -> User
transformToUser maybeMySQLValue = User 1 "We need to get the real use data"

extractMaybeUser :: Maybe User -> User
extractMaybeUser maybeUser = case maybeUser of
                                            Just value -> value
                                            Nothing -> User 1 "default User"

--extractUserId :: [MySQLValue] -> MySQLInt32
extractUserId mySQLValue = mySQLValue getTextField [mySQLTypeLong]

{-| For insert we use [execute] operator followed by the connection, query and an array of QueryParam-}
insertUser :: User -> IO User
insertUser _user = let user = _user in do
        conn <- createConnection
        status <- execute conn insertUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user)]
        return user

{-| Transform from Int to Int32 format-}
intToInt32 :: Int -> Int32
intToInt32 userId = fromIntegral (userId :: Int) :: Int32

{-| WE use [connect] operator together with [defaultConnectInfo] with the info to connect to the MySQL Server-}
createConnection :: IO MySQLConn
createConnection = connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "mysql"}

