{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ConnectorManager where

import CassandraConnector
import MySQLConnector
import ModelTypes

import Data.Configurator
import Data.Configurator.Types (Value(String))
import Data.Text (unpack,pack)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32,Int)
import Database.MySQL.Base

-- | Connector manager
-- --------------------
{-| Module responsible to choose between the different connectors to persist, read or delete elements in back ends. -}

selectAllUsers :: IO (Either UserNotFound [User])
selectAllUsers =  do
                 connectorType <- readConfiguration "connector"
                 result <- case connectorType of
                                String  "cassandra" -> searchAllCassandraUsers
                                String  "mysql" -> searchAllMySQLUsers
                                _ -> return  $ Left $ UserNotFound "No connector found"
                 return result

selectUserById :: Int -> IO (Either UserNotFound User)
selectUserById id =  do
                 connectorType <- readConfiguration "connector"
                 result <- case connectorType of
                                String  "cassandra" -> searchCassandraUserById id
                                String  "mysql" -> searchMySQLUserById id
                                _ -> return  $ Left $ UserNotFound "No connector found"
                 return result

createUser ::  User -> IO ConnectorStatus
createUser user = do result <- genericCommand $ Right user
                     return result

deleteUserById :: Int -> IO ConnectorStatus
deleteUserById id = do result <- genericCommand $ Left id
                       return result

{-| Generic command for create and delete user using [Either] as a type to determine one action or another-}
genericCommand :: (Either Int User) -> IO ConnectorStatus
genericCommand either = do connectorType <- readConfiguration "connector"
                           result <- case connectorType of
                                   String  "cassandra" -> case either of
                                                               Left id -> deleteCassandraById id
                                                               Right user -> insertCassandraUser user

                                   String  "mysql" -> case either of
                                                               Left id -> deleteMySQLById id
                                                               Right user -> insertMySQLUser user
                                   _ -> return  $ ConnectorStatus "No connector found"
                           return result

-- | Interact with connectors
-- ---------------------------
{-| Here we interact with the final connectors where we adapt the specific response from the connectors
    into the generic one for our consumers-}

searchAllCassandraUsers:: IO (Either UserNotFound [User])
searchAllCassandraUsers = do result <- selectAllCassandraUser
                             return $ Right result

searchAllMySQLUsers:: IO (Either UserNotFound [User])
searchAllMySQLUsers = do result <- getAllUsers
                         return $ Right result

searchCassandraUserById:: Int -> IO (Either UserNotFound User)
searchCassandraUserById id = do result <- selectCassandraUserById $ intToInt32 id
                                return result

searchMySQLUserById:: Int -> IO (Either UserNotFound User)
searchMySQLUserById id = do result <- getUserById id
                            return result

insertMySQLUser :: User -> IO ConnectorStatus
insertMySQLUser user = do status <- insertUser user
                          return $ ConnectorStatus "MySQL user persisted"

insertCassandraUser :: User -> IO ConnectorStatus
insertCassandraUser user = do status <- createCassandraUser user
                              return $ ConnectorStatus "Cassandra user persisted"

deleteMySQLById :: Int -> IO ConnectorStatus
deleteMySQLById id = do status <- deleteMySQLUserById id
                        return $ ConnectorStatus "MySQL user deleted"

deleteCassandraById :: Int -> IO ConnectorStatus
deleteCassandraById id = do status <- deleteCassandraUserById (intToInt32 id)
                            return $ ConnectorStatus "Cassandra user deleted"

-- | Utils
-- ---------
{-| We use the [configurator] library to read a cnf file an extract properties in the file.
    We use the operator [load] to read the config file and return a [Config] type,
    and then we use [require] operator to extract the specific property
    The function return a monad of type Value which contains the property in the config file-}
readConfiguration :: String -> IO Value
readConfiguration param = do
                   cfg <- load [Required "$(HOME)/Development/Dive_into_Haskell/connectorManager.cfg"]
                   cnfProperty <- require cfg (pack param) :: IO Value
                   return cnfProperty