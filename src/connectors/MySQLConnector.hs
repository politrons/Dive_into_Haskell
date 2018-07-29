{-# LANGUAGE OverloadedStrings #-}
module MySQLConnector where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams

{-| The version of MySQL-Haskell 0.8.3.0 only works properly with MySQL server 5.5-}
mysqlIO :: IO ()
mysqlIO = do
    conn <- connect
        defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "mysql"}
    (defs, is) <- query_ conn "SELECT * FROM haskell_users"
    print =<< Streams.toList is