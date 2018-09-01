{-# LANGUAGE OverloadedStrings #-}
module ConnectorManager where

import CassandraConnector
import MySQLConnector
import ModelTypes

import Data.Configurator
import Data.Configurator.Types (Value(String))
import Data.Text (unpack,pack)
import Control.Monad.IO.Class (liftIO)


selectAllUsers :: IO (Either UserNotFound [User])
selectAllUsers =  do
                 connectorType <- readConfiguration
                 result <- case connectorType of
                                String  "cassandra" -> searchAllCassandraUsers
                                String  "mysql" -> searchAllMySQLUsers
                                _ -> return  $ Left $ UserNotFound "No connector found"
                 return result

searchAllCassandraUsers:: IO (Either UserNotFound [User])
searchAllCassandraUsers = do result <- selectAllCassandraUser
                             return $ Right result

searchAllMySQLUsers:: IO (Either UserNotFound [User])
searchAllMySQLUsers = do result <- getAllUsers
                         return $ Right result

-- | Utils
-- ---------

readConfiguration :: IO Value
readConfiguration = do
                   cfg <- load [Required "$(HOME)/Development/Dive_into_Haskell/connectorManager.cfg"]
                   connectorName <- require cfg  "connector" :: IO Value
                   return connectorName