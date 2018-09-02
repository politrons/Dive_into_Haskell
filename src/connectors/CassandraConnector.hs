{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This connector is build on top of awesome libraries as https://hackage.haskell.org/package/cql
     and cql-io http://hackage.haskell.org/package/cql-io-}
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
import Network.Socket (PortNumber (..),PortNumber)
import Data.Configurator
import Data.Configurator.Types (Value(String))
import Text.Read (readMaybe)

-- | Queries
-- -------------
{- | In cql-io we have [QueryString] function which first element is the type of action
      R - Read
      W - Write
      S - Schema
    After that we define in case we have arguments in the query the tuple of types
    Finally we define another tuple for the output of the data.
-}
versionQuery = "SELECT cql_version from system.local" :: QueryString R () (Identity Text)
allUsersQuery = "SELECT * from haskell_cassandra.haskell_users;" :: QueryString R () ((Int32, Text))
userByIdQuery = "SELECT * from haskell_cassandra.haskell_users  WHERE userid=?" :: QueryString R (Identity Int32) ((Int32, Text))
insertQuery = "INSERT INTO haskell_cassandra.haskell_users(userid,username) VALUES (?,?)" :: QueryString W ((Int32, Text)) ()
deleteByIdQuery = "DELETE FROM haskell_cassandra.haskell_users WHERE userid=?" :: QueryString W (Identity Int32) ()

-- | CRUD
-- -------------
{-| cql-io  provide [runClient] function which receive the next arguments
      conn :: ClientState -> Is the connection to the backend
      client: Client created from query/query1/write functions passing arguments QueryString/PrepQuery and QueryParam

    As request in case you have a query with inputs you define a tuple in the QueryString and is automatically replace by the ?
    your query.
    As response, in case you define in your QueryString that you expect to receive a tuple, the response is a tuple of type as you
    defined.
    -}
getVersion:: IO [Identity Text]
getVersion = do
                let queryParam = createQueryParam ()
                runQuery versionQuery queryParam

{-| We create and use the type class [CustomQueryRunner] and we use the function runQuery-}
selectAllCassandraUser :: IO [User]
selectAllCassandraUser = do
                  let queryParam = createQueryParam ()
                  array <- runQuery allUsersQuery queryParam
                  users <- transformArrayToUsers array
                  return users

{-| In case of find by Id a user we need to control the effect that maybe the user is not present in the database.
    In order to have that possible effect we define the type [UserNotFound] and we use [Either] monad which like
    in other language can contains two types, in this case [UserNotFound] or [User] -}
selectCassandraUserById :: Int32 -> IO (Either UserNotFound User)
selectCassandraUserById userId = do
                  let queryParam = createQueryParam (Identity userId)
                  do maybe <- runQuery userByIdQuery queryParam
                     either <- transformMaybeTupleToUser maybe
                     return either

createCassandraUser:: User -> IO ()
createCassandraUser user = do
                 let queryParam = createQueryParam (intToInt32(getUserId user), pack $ getUserName user)
                 runQuery insertQuery queryParam

deleteCassandraUserById :: Int32 -> IO ()
deleteCassandraUserById userId = do
                  let queryParam = createQueryParam (Identity userId)
                  runQuery deleteByIdQuery queryParam


-- | Type classes
-- --------------------

-- | Query params
-- --------------------
{- | We use Type classes to reuse the same signature method and with multiple implementations depending the types-}
class CustomQueryParam x where
  createQueryParam :: x -> QueryParams x

instance CustomQueryParam (Identity Int32) where
   createQueryParam x = defQueryParams One x

instance CustomQueryParam (Int32, Text) where
   createQueryParam x = defQueryParams One x

instance CustomQueryParam () where
   createQueryParam x = defQueryParams One x

-- | Run Client
-- --------------------
{-| For this current and futures queries we define this Type classes to reuse runClient queries in a generic way for
    the specific input and output types.-}
class CustomQueryRunner queryString queryParam output where
   runQuery :: queryString -> queryParam -> output

instance CustomQueryRunner (QueryString R () (Identity Text)) (QueryParams ()) (IO[Identity Text])  where
   runQuery x y = do conn <- getConnection
                     runClient conn $ query x y

instance CustomQueryRunner (QueryString R () ((Int32, Text))) (QueryParams ()) (IO[(Int32, Text)])  where
   runQuery x y = do conn <- getConnection
                     runClient conn $ query x y

instance CustomQueryRunner (QueryString R (Identity Int32) ((Int32, Text))) (QueryParams (Identity Int32)) (IO (Maybe(Int32, Text)))  where
   runQuery x y = do conn <- getConnection
                     runClient conn $ query1 x y

instance CustomQueryRunner (QueryString W (Int32, Text) ()) (QueryParams (Int32, Text)) (IO())  where
   runQuery x y = do conn <- getConnection
                     runClient conn $ write x y

instance CustomQueryRunner (QueryString W (Identity Int32) ()) (QueryParams (Identity Int32)) (IO())  where
   runQuery x y = do  conn <- getConnection
                      runClient conn $ write x y

getConnection :: IO ClientState
getConnection = do  logger <- Logger.new Logger.defSettings
                    conn <- createConnection logger
                    return conn

-- | Utils
-- -------------
{- | As usual using [map] operator we transform the Tuple into User data type-}
transformArrayToUsers :: [(Int32, Text)] -> IO [User]
transformArrayToUsers array = return $ map (\tuple -> User (getFirstElement tuple) (getLastElement tuple)) array

{- | Using [Either] operator we define the possibility that we have two possible effects. We can return a User
     in case the id is correct, or if is not we will return an UserNotFound.-}
transformMaybeTupleToUser :: Maybe((Int32, Text)) -> IO (Either UserNotFound User)
transformMaybeTupleToUser maybe = case maybe of
                               Just value -> return $ Right $ User (getFirstElement value) (getLastElement value)
                               Nothing -> return $ Left $ UserNotFound "User not found"

{- | Using [fst] operator we are able to get first element in a Tuple -}
getFirstElement ::(Int32, Text) -> Int
getFirstElement tuple = int32ToInt(fst tuple)

{- | Using [snd] operator we are able to get last element in a Tuple -}
getLastElement ::(Int32, Text) -> String
getLastElement tuple = unpack(snd tuple)

-- | Connection
-- -------------
{-| In here we use the cql-io API to create the [ClientState] data type which contains the connection to the backend -}
createConnection :: Logger.Logger -> IO ClientState
createConnection logger = do connectionSettings <- getConnectorSettings
                             Client.init logger connectionSettings

{-| Monad to compose a Cassandra Settings connector config-}
getConnectorSettings :: IO Settings
getConnectorSettings = do
                          portCnf <- getConfigParam "portNumber"
                          portNumber <- return $ getPortNumber portCnf
                          maxTimeoutCnf <- getConfigParam "maxTimeout"
                          maxTimeout <- return $ stringToInt $ unpack maxTimeoutCnf
                          maxConnectionsCnf <- getConfigParam "maxConnections"
                          maxConnections <- return $ stringToInt $ unpack maxConnectionsCnf
                          return $ createConnectionSettings portNumber maxConnections maxTimeout

{-| THis function it creates the [Settings] type which is used for Client.init function to create the ClientState which
    is the open connection to the cassandra backend.
    This function is a composition of functions each configuring a particular part of the Cassandra connection option-}
createConnectionSettings :: PortNumber -> Int -> Int -> Settings
createConnectionSettings portNumber maxConnection maxTimeout= addRetryStrategy retryForever $
                           addMaxTimeout maxTimeout $
                           addMaxConnections maxConnection $
                           addPortNumber portNumber defSettings

addPortNumber :: PortNumber -> Settings -> Settings
addPortNumber port settings =  (setPortNumber port) settings

addMaxConnections :: Int -> Settings -> Settings
addMaxConnections maxConnection settings = (setMaxConnections maxConnection) settings

addMaxTimeout :: Int -> Settings -> Settings
addMaxTimeout maxTimeout settings = (setMaxTimeouts maxTimeout) settings

addRetryStrategy :: RetrySettings -> Settings -> Settings
addRetryStrategy strategy settings = (setRetrySettings strategy) settings

getConfigParam :: String -> IO Text
getConfigParam param = do
                   cfg <- load [Required "$(HOME)/Development/Dive_into_Haskell/cassandraConnector.cfg"]
                   configParam <- require cfg $ pack param :: IO Value
                   configValue <- case configParam of
                                       String value -> return value
                                       _ -> return "No config property found"
                   return configValue

getPortNumber :: Text -> PortNumber
getPortNumber s = case (fromInteger <$> readMaybe (unpack s)) of
                    Just value -> value
                    Nothing -> 9042 -- Default port
