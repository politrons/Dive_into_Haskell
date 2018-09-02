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
import Data.Int (Int32)
import Database.MySQL.Base


-- | Connector manager
-- --------------------
{-| Module responsible to choose between the diferenmt connectors to persist, read or delete elements in back ends. -}

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
createUser user = do connectorType <- readConfiguration "connector"
                     result <- case connectorType of
                               String  "cassandra" -> insertCassandraUser user
                               String  "mysql" -> insertMySQLUser user
                               _ -> return  $ ConnectorStatus "No connector found"
                     return result

deleteUserById :: Int -> IO ConnectorStatus
deleteUserById id = do connectorType <- readConfiguration "connector"
                       result <- case connectorType of
                                   String  "cassandra" -> deleteCassandraById id
                                   String  "mysql" -> deleteMySQLById id
                                   _ -> return  $ ConnectorStatus "No connector found"
                       return result



-- | Interact with connectors
-- ---------------------------

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
{-| We use the [configurator] library to read a cnf file an extract properties in the file-}
readConfiguration :: String -> IO Value
readConfiguration param = do
                   cfg <- load [Required "$(HOME)/Development/Dive_into_Haskell/connectorManager.cfg"]
                   cnfProperty <- require cfg (pack param) :: IO Value
                   return cnfProperty