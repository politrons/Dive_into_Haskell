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
                    g <- Logger.new Logger.defSettings
                    c <- Client.init g defSettings
                    let q = "SLECT cql_version from system.local" :: QueryString R () (Identity Text)
                    let p = defQueryParams One ()
                    runClient c (query q p)




