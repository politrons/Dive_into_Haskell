{-# LANGUAGE OverloadedStrings #-}
module ConnectorManager where

import CassandraConnector
import MySQLConnector
import ModelTypes

import Data.Configurator
import Data.Configurator.Types (Value(String))
import Data.Text (unpack,pack)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)

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

-- | Utils
-- ---------

readConfiguration :: String -> IO Value
readConfiguration param = do
                   cfg <- load [Required "$(HOME)/Development/Dive_into_Haskell/connectorManager.cfg"]
                   connectorName <- require cfg (pack param) :: IO Value
                   return connectorName