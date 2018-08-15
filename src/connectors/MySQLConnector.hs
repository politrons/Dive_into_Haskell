{-# LANGUAGE OverloadedStrings #-}
{-| Connector working in top of library MySQL-Haskell https://github.com/winterland1989/mysql-haskell
    The version of MySQL-Haskell 0.8.3.0 only works properly with MySQL server 5.5-}
module MySQLConnector where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Int
import ModelTypes
import qualified Data.Text as T
import Data.List
import Control.Monad.IO.Class

selectAllQuery = "SELECT * FROM haskell_users"
selectByIdQuery = "SELECT * FROM haskell_users WHERE userId=(?)"
deleteByIdQuery = "DELETE FROM haskell_users WHERE userId=(?)"
insertUserQuery = "INSERT INTO mysql.haskell_users (userId,userName) VALUES(?,?)"
updateUserQuery = "UPDATE mysql.haskell_users SET userId=(?),userName=(?) WHERE userId=(?)"

-- | MySQL CRUD
-- -------------
getAllUsers :: IO [[MySQLValue]]
getAllUsers = do
    conn <- createConnection
    s <- prepareStmt conn selectAllQuery
    (defs, inputStream) <- queryStmt conn s [MySQLInt32U 18]
    maybeUsers <- do return (Streams.toList inputStream)
    users <- liftIO $ transformMySQLValueArrayToUsers <$> maybeUsers
    return users

transformMySQLValueArrayToUsers :: [[MySQLValue]] -> [[MySQLValue]]
transformMySQLValueArrayToUsers array = map (\user -> user)array

{-| For select we use [query] operator followed by the connection, query and a QueryParam-}
getUserById :: Int -> IO User
getUserById id = let userId = id in do
            conn <- createConnection
            (columnDef, inputStream) <- query conn selectByIdQuery [One $ MySQLInt32 (intToInt32 userId)]
            maybeMySQLValue <- Streams.read inputStream
            user <- do return (extractMaybeUser maybeMySQLValue)
            return user

{-| For insert we use [execute] operator followed by the connection, query and an array of QueryParam-}
insertUser :: User -> IO OK
insertUser _user = let user = _user in do
            conn <- createConnection
            status <- execute conn insertUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user)]
            return status

{-| For select we use [query] operator followed by the connection, query and a QueryParam-}
deleteUserById :: Int -> IO OK
deleteUserById id = let userId = id in do
              conn <- createConnection
              status <- execute conn  deleteByIdQuery [One $ MySQLInt32 (intToInt32 userId)]
              return status

{-| For update we use [execute] operator followed by the connection,update query and an array of QueryParam with data to update and filter-}
updateUserById :: User -> IO OK
updateUserById _user = let user = _user in do
            conn <- createConnection
            status <- execute conn  updateUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user),MySQLInt32 (intToInt32 $ getUserId user)]
            return status

{-| Function to extract the MySQLValue from Maybe and transform into User calling another function-}
extractMaybeUser :: Maybe [MySQLValue] -> User
extractMaybeUser maybeMySQLValue = case maybeMySQLValue of
                                            Just mysqlValue -> transformToUser mysqlValue
                                            Nothing -> User 0 "default User"

{-| Function to receive the row [MySQLValue] and we define the fields of the row to be extracted, and after change
    format of types using some utils functions we create the User instance.
    In order to transform from Text to String we just need to use the operator [unpack] to extract the String -}
transformToUser :: [MySQLValue] -> User
transformToUser [MySQLInt32 row_userId, MySQLText row_userName] = User (int32ToInt row_userId) (T.unpack row_userName)

{-| Transform from Int to Int32 format-}
intToInt32 :: Int -> Int32
intToInt32 userId = fromIntegral (userId :: Int) :: Int32

int32ToInt :: Int32 -> Int
int32ToInt userId = fromIntegral (userId :: Int32) :: Int

{-| WE use [connect] operator together with [defaultConnectInfo] with the info to connect to the MySQL Server-}
createConnection :: IO MySQLConn
createConnection = connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "mysql"}

