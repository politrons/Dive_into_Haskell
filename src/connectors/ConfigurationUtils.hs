{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigurationUtils where

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
import Network.Socket (PortNumber (..),PortNumber)
import Data.Configurator
import Data.Configurator.Types (Value(String))
import Text.Read (readMaybe)

{-| Generic function which receive config file path and param and return the value of that property-}
getConfigParam :: String -> String -> IO String
getConfigParam filePath param = do
                   cfg <- load [Required filePath]
                   configParam <- require cfg $ pack param :: IO Value
                   configValue <- case configParam of
                                       String value -> return value
                                       _ -> return "No config property found"
                   return $ unpack configValue

