{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CassandraConnector where

import Data.Text (Text,pack,unpack)
import Data.Functor.Identity
import Database.CQL.IO as Client
import qualified System.Logger as Logger
import Control.Monad.IO.Class (liftIO)
import System.Logger (eval)
import Database.CQL.Protocol( ColumnType( IntColumn ) )
import Database.CQL.Protocol( ColumnType( VarCharColumn ) )
import Data.Int (Int64,Int32)
import ModelTypes

-- | User
-- -------------

getVersion:: IO [Identity Text]
getVersion = do
                logger <- Logger.new Logger.defSettings
                conn <- Client.init logger createConnectionSettings
                let versionQuery = "SELECT cql_version from system.local" :: QueryString R () (Identity Text)
                let queryParam = defQueryParams One ()
                runClient conn (query versionQuery queryParam)

selectAllUser :: IO [(Int32, Text)]
selectAllUser = do
                  logger <- Logger.new Logger.defSettings
                  conn <- Client.init logger createConnectionSettings
                  let selectAllQuery = "SELECT * from haskell_cassandra.haskell_users;" :: QueryString R () ((Int32, Text))
                  let queryParam = defQueryParams One ()
                  runClient conn (query selectAllQuery queryParam)

selectUserById :: Int32 -> IO User
selectUserById userId = do
                  logger <- Logger.new Logger.defSettings
                  conn <- Client.init logger createConnectionSettings
                  let selectQuery = "SELECT * from haskell_cassandra.haskell_users  WHERE userid=?" :: PrepQuery R (Identity Int32) ((Int32, Text))
                  let queryParam = defQueryParams One (Identity userId)
                  do maybe <- runClient conn (query1 selectQuery queryParam)
                     response <- transformTupleToUser maybe
                     return response


-- | Utils
-- -------------

transformTupleToUser :: Maybe((Int32, Text)) -> IO User
transformTupleToUser maybe = case maybe of
                               Just value -> return $ User (getFirstElement value) (getLastElement value)
                               Nothing -> return $ User 0 "User not found"

getFirstElement ::(Int32, Text) -> Int
getFirstElement tuple = int32ToInt(fst tuple)

getLastElement ::(Int32, Text) -> String
getLastElement tuple = unpack(snd tuple)

-- | Connection
-- -------------
createConnectionSettings :: Settings
createConnectionSettings = addRetryStrategy $
                           addMaxTimeout $
                           addMaxConnections $
                           addPortNumber defSettings

addPortNumber :: Settings -> Settings
addPortNumber settings =  (setPortNumber 9042) settings

addMaxConnections :: Settings -> Settings
addMaxConnections settings = (setMaxConnections 100) settings

addMaxTimeout :: Settings -> Settings
addMaxTimeout settings = (setMaxTimeouts 10000) settings

addRetryStrategy :: Settings -> Settings
addRetryStrategy settings = (setRetrySettings retryForever) settings



--insertUser :: Client ()
--insertUser = do
--             let user = ( 4835637638, "hello world")
--             write ins1 (params a)
--             where
--                 ins1 :: PrepQuery W Ty1 ()
--                 ins1 = [r| insert into cqltest.test1 (a,b) values (?,?) |]