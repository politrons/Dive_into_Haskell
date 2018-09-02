{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import System.IO.Streams (InputStream)
import Control.Concurrent (myThreadId,newEmptyMVar,forkIO,threadDelay,putMVar,takeMVar)

selectAllQuery = "SELECT * FROM haskell_users"
selectByIdQuery = "SELECT * FROM haskell_users WHERE userId=(?)"
selectByNameQuery = "SELECT * FROM haskell_users WHERE userName=(?)"
deleteByIdQuery = "DELETE FROM haskell_users WHERE userId=(?)"
insertUserQuery = "INSERT INTO mysql.haskell_users (userId,userName) VALUES(?,?)"
updateUserQuery = "UPDATE mysql.haskell_users SET userId=(?),userName=(?) WHERE userId=(?)"
insertAddressQuery = "INSERT INTO mysql.haskell_address (id,number,street) VALUES(?,?,?)"
selectByAddressIdQuery = "SELECT * FROM haskell_address WHERE id=(?)"


-- | MySQL CRUD
-- -------------

-- | User
-- ---------

{-| Function for select all. We select we use [queryStmt] operator followed by the statement created previously.
    For the prepare statement we use the [prepareStmt] operator followed by the connection and query.
    For all this operations since access to database is blocking, we use green threads by [ForkIO] -}
getAllUsers :: IO [User]
getAllUsers = do
            emptyVar <- newEmptyMVar
            forkIO $ do
                conn <- createConnection
                liftIO $ putStrLn ("Preparing connection:")
                s <- prepareStmt conn selectAllQuery
                (_, inputStream) <- queryStmt conn s [MySQLInt32U 18]
                maybeUsers <- return (Streams.toList inputStream)
                users <- liftIO $ transformMySQLValueArrayToUsers <$> maybeUsers
                putMVar emptyVar users
            users <- takeMVar emptyVar
            return users

{-| Function for select. We select we use [query] operator followed by the connection, query and a QueryParam-}
getUserById :: Int -> IO (Either UserNotFound User)
getUserById id = let userId = id in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                  conn <- createConnection
                  maybeMySQLValue <- executeQuery (toUserId userId) conn
                  putMVar emptyVar maybeMySQLValue
              maybeMySQLValue <- takeMVar emptyVar
              return $ transformMaybeMySQLValueToUser maybeMySQLValue

{-| Function for select. We use [query] operator followed by the connection, query and a QueryParam-}
getUserByUserName :: String -> IO (Either UserNotFound User)
getUserByUserName _name = let name = _name in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                  conn <- createConnection
                  maybeMySQLValue <- executeQuery (toUserName name) conn
                  putMVar emptyVar maybeMySQLValue
              maybeMySQLValue <- takeMVar emptyVar
              return $ transformMaybeMySQLValueToUser maybeMySQLValue

{-|Function for insert. We use [execute] operator followed by the connection, query and an array of QueryParam-}
insertUser :: User -> IO OK
insertUser _user = let user = _user in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                    conn <- createConnection
                    status <- executeCommand user conn
                    putMVar emptyVar status
              status <- takeMVar emptyVar
              return status

{-| Function for delete. We use [query] operator followed by the connection, query and a QueryParam-}
deleteMySQLUserById :: Int -> IO OK
deleteMySQLUserById id = let userId = id in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                  conn <- createConnection
                  status <- executeCommand (toUserId userId) conn
                  putMVar emptyVar status
              status <- takeMVar emptyVar
              return status

{-| Function for update. we use [execute] operator followed by the connection,update query and an array of QueryParam with data to update and filter-}
updateUserById :: User -> IO OK
updateUserById _user = let user = _user in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                  conn <- createConnection
                  status <- executeUpdateQuery user conn
                  putMVar emptyVar status
              status <- takeMVar emptyVar
              return status

-- | Address
-- ---------

insertAddress :: Address -> IO OK
insertAddress _address = let address = _address in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                    conn <- createConnection
                    status <- executeCommand address conn
                    putMVar emptyVar status
              status <- takeMVar emptyVar
              return status

getAddressById :: Int -> IO Address
getAddressById id = let addressId = id in do
              emptyVar <- newEmptyMVar
              forkIO $ do
                  conn <- createConnection
                  maybeMySQLValue <- executeQuery (toAddressId addressId) conn
                  putMVar emptyVar maybeMySQLValue
              maybeMySQLValue <- takeMVar emptyVar
              return $ transformMaybeMySQLValueToAddress maybeMySQLValue


-- | Type classes
-- ----------------

-- | Commands

{-| Type class definition for commands-}
class Commands x y z where
    executeCommand :: x -> y -> IO z

{-| Type class implementation to Execute the insert user query-}
instance Commands User MySQLConn OK where
    executeCommand user conn = execute conn insertUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user)]

{-| Type class implementation to Execute the insert Address query-}
instance Commands Address MySQLConn OK where
    executeCommand address conn = execute conn insertAddressQuery [MySQLInt32 (intToInt32 $ getAddressId address),
                                                                MySQLInt32(intToInt32 $ getAddressNumber address),
                                                                MySQLText (T.pack $ getAddressStreet address)]

{-| Type class implementation to Execute the delete query-}
instance Commands UserId MySQLConn OK where
    executeCommand userId conn = execute conn  deleteByIdQuery [One $ MySQLInt32 (intToInt32 $ fromUserId userId)]

-- | Queries

{-| Type class definition for queries-}
class Queries x y z k where
    executeQuery :: x -> y -> IO (z [k])

{-| Type class implementation to Execute the select By Id query-}
instance Queries UserId MySQLConn Maybe MySQLValue where
  executeQuery userId conn = do (columnDef, inputStream) <- query conn selectByIdQuery [One $ MySQLInt32 (intToInt32 (fromUserId userId))]
                                maybe <- (Streams.read inputStream)
                                return maybe

{-| Type class implementation to Execute the select By Address query-}
instance Queries AddressId MySQLConn Maybe MySQLValue where
  executeQuery addressId conn = do (columnDef, inputStream) <- query conn selectByAddressIdQuery [One $ MySQLInt32 (intToInt32 (fromAddressId addressId))]
                                   maybe <- (Streams.read inputStream)
                                   return maybe

{-| Type class implementation to Execute the select By name query-}
instance Queries Username MySQLConn Maybe MySQLValue where
  executeQuery userName conn = do (columnDef, inputStream) <- query conn selectByNameQuery [One $ MySQLText (T.pack $ fromUserName userName)]
                                  maybe <- (Streams.read inputStream)
                                  return maybe

{-| Function to  Execute the update query-}
executeUpdateQuery :: User -> MySQLConn -> IO OK
executeUpdateQuery user  conn = execute conn  updateUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user),MySQLInt32 (intToInt32 $ getUserId user)]

-- | Transformation utils
-- -------------------------

{-| Function to extract the MySQLValue from Maybe and transform into User calling another function-}
transformMaybeMySQLValueToUser :: Maybe [MySQLValue] -> Either UserNotFound User
transformMaybeMySQLValueToUser maybeMySQLValue = case maybeMySQLValue of
                                            Just mysqlValue -> Right $ transformToUser mysqlValue
                                            Nothing -> Left $ UserNotFound "User not found"

{-| Function to extract the MySQLValue from Maybe and transform into Address calling another function-}
transformMaybeMySQLValueToAddress :: Maybe [MySQLValue] -> Address
transformMaybeMySQLValueToAddress maybeMySQLValue = case maybeMySQLValue of
                                            Just mysqlValue -> transformToAddress mysqlValue
                                            Nothing -> Address 0  0 "default address"

{-| Function that take an array of [[MySQLValue]] and transform every element into [User]-}
transformMySQLValueArrayToUsers :: [[MySQLValue]] -> [User]
transformMySQLValueArrayToUsers mysqlValues = map (\mysqlValue -> transformToUser mysqlValue) mysqlValues

{-| Function to receive the row [MySQLValue] and we define the fields of the row to be extracted, and after change
    format of types using some utils functions we create the User instance.
    In order to transform from Text to String we just need to use the operator [unpack] to extract the String -}
transformToUser :: [MySQLValue] -> User
transformToUser [MySQLInt32 row_userId, MySQLText row_userName] = User (int32ToInt row_userId) (T.unpack row_userName)

transformToAddress :: [MySQLValue] -> Address
transformToAddress [MySQLInt32 row_id, MySQLInt32 row_number, MySQLText row_street] = Address (int32ToInt row_id)(int32ToInt row_number) (T.unpack row_street)

{-| We use [connect] operator together with [defaultConnectInfo] with the info to connect to the MySQL Server-}
createConnection :: IO MySQLConn
createConnection = connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "mysql"}

{-| Using mysql-haskell [Streams.read] operator we able to transform the inputStream into Maybe[Type]-}
readInputStream :: InputStream a -> IO (Maybe a)
readInputStream inputStream = Streams.read inputStream