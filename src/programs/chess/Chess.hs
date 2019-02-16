{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess where

import           Control.Monad.IO.Class    (liftIO)
import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           Data.List
import           Data.List.Split
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Text.Lazy            (Text)
import           Data.Text.Lazy            (pack)
import           Data.Text.Lazy            (replace)
import qualified Data.Text.Lazy            as LazyText
import           GHC.Generics
import           Text.RawString.QQ

--Server
import           Web.Scotty
import           Web.Scotty                (ActionM, ScottyM, get, scotty, text)
import           Web.Scotty.Internal.Types (ActionT, File, Options, Param,
                                            RoutePattern, ScottyT)

servicePort = 3700 :: Int

{-| ----------------------------------------------}
{-|                    SERVER                    -}
{-| ----------------------------------------------}
{-| Using [scotty] passing [port] and [routes] we define the http server.
    We also keep the state of the player using [IORef] so we can move that state [AdventureInfo] around the program-}
chessServer :: IO ()
chessServer = do
  print ("Starting Adventure Server at port " ++ show servicePort)
  chessInfo <- newIORef $ ChessInfo []
  scotty servicePort (routes chessInfo)

routes :: IORef ChessInfo -> ScottyM ()
routes chessInfoRef = do
  get "/service" responseService
  get "/chess/register/:player" $ registerPlayer chessInfoRef
  get "/chess/players" $ getPlayersInGame chessInfoRef
  get "/chess/:player/:move" undefined -- $ processAction adventureInfoRef

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = do
  let version = "1.0"
  html $ mconcat ["<h1>Chess Haskell server ", version, "</h1>"]

registerPlayer :: IORef ChessInfo -> ActionM ()
registerPlayer chessInfoRef = do
  chessInfo <- liftIO $ readIORef chessInfoRef
  page <- toActionM $ readFile "src/programs/chess/table/table.hrml"
  let playersList = players chessInfo
  if length playersList == 2
    then do page <- undefined -- return $ replace "#name" "Max number of users reach" page
            html $ mconcat ["", page, ""]
    else html $ ""

{-| Function to return the board chess page with the number of users that are playing-}
getPlayersInGame :: IORef ChessInfo -> ActionM ()
getPlayersInGame chessInfoRef = do
  chessInfo <- liftIO $ readIORef chessInfoRef
  page <- toActionM $ replacePlayersInChessBoard $ players chessInfo
  html $ mconcat ["", page, ""]

{-| Function to read the html page and transform in Text, also we use fmap name players to iterate the players collection
    and return a new one with only the attribute specify, in here it would be a list of names::String.
    We use the unwords function to extract the String in the list and concatenate |-}
replacePlayersInChessBoard :: [PlayerInfo] -> IO Text
replacePlayersInChessBoard players = do
  page <- readFile "src/programs/chess/table/table.hrml"
  let playerNamesList = fmap name players
  let playersName = unwords playerNamesList
  let pageWithName = replace "#name" (pack playersName) (pack page)
  return pageWithName

{-| Sugar syntax function where we expect any IO value and we use the Scotty [liftAndCatchIO] function to transform to [ActionM] monad-}
toActionM :: IO any -> ActionM any
toActionM = liftAndCatchIO

newtype PlayerInfo = PlayerInfo
  { name :: String
  }

newtype ChessInfo = ChessInfo
  { players :: [PlayerInfo]
  }
