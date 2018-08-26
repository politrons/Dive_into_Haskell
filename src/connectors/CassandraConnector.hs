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


cassandraConnector:: IO [Identity Text]
cassandraConnector = do
                    logger <- Logger.new Logger.defSettings
                    conn <- Client.init logger createConnectionSettings
                    let selectQuery = "SLECT cql_version from system.local" :: QueryString R () (Identity Text)
                    let queryParam = defQueryParams One ()
                    runClient conn (query selectQuery queryParam)



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