{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CassandraConnector where

import Data.Text (Text)
import Data.Functor.Identity
import Database.CQL.IO as Client
import qualified System.Logger as Logger
import Control.Monad.IO.Class (liftIO)
import System.Logger (eval)
import Database.CQL.Protocol( ColumnType( IntColumn ) )
import Database.CQL.Protocol( ColumnType( VarCharColumn ) )
import Data.Int (Int64,Int32)


cassandraConnector:: IO [Identity Text]
cassandraConnector = do
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

selectUserById :: IO (Identity Text)
selectUserById = do
                  logger <- Logger.new Logger.defSettings
                  conn <- Client.init logger createConnectionSettings
                  let selectQuery = "SELECT * from haskell_cassandra.haskell_users" :: QueryString R () (Identity Text)
                  let queryParam = defQueryParams One ()
                  do maybe <- runClient conn (query1 selectQuery queryParam)
                     response <- getResponse maybe
                     return response

--selectRows :: ClientState -> IO [(Int, String)]
--selectRows conn = runClient conn (query (defQueryParams Quorum ()) cql)
--  where
--    cql :: QueryString R () (Int, String)
--    cql = "SELECT userid, username from haskell_cassandra.haskell_users "


--insertUser :: Client ()
--insertUser = do
--             let user = ( 4835637638, "hello world")
--             write ins1 (params a)
--             where
--                 ins1 :: PrepQuery W Ty1 ()
--                 ins1 = [r| insert into cqltest.test1 (a,b) values (?,?) |]





createRequest :: QueryString R () (Identity Text, Text)
createRequest = "SELECT * from haskell_cassandra.haskell_users"

getResponse :: Maybe(Identity Text) -> IO (Identity Text)
getResponse maybe = case maybe of
                               Just value -> return value
                               Nothing -> return "No records found"

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