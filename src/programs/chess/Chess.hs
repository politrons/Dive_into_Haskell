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
import Data.Maybe (fromMaybe)

servicePort = 3700 :: Int
chessBoardPath = "src/programs/chess/table/table.html" :: String
playersTag = "#Players" :: Text

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
  get "/chess/register/:playerName" $ registerPlayer chessInfoRef
  get "/chess/players" $ getPlayersInGame chessInfoRef
  get "/chess/:player/:move" undefined -- $ processAction adventureInfoRef

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = do
  let version = "1.0"
  html $ mconcat ["<h1>Chess Haskell server ", version, "</h1>"]

{-| Function to register the new PlayerInfo in case we not reach the max number of players already-}
registerPlayer :: IORef ChessInfo -> ActionM ()
registerPlayer chessInfoRef = do
  playerName <- extractUriParam "playerName"
  chessInfo <- liftIO $ readIORef chessInfoRef
  page <- toActionM $ readFile chessBoardPath
  let playersList = players chessInfo
  if length playersList == 2
    then do
      let replacePage = replace playersTag "Max number of users reach" (pack page)
      html $ mconcat ["", replacePage, ""]
    else do
      let playerInfoList = players chessInfo ++ [PlayerInfo playerName]
      newChessInfo <- toActionM $ writeChessInfoInIORef chessInfoRef playerInfoList
      let replacePage = replaceNameInPage page playerName
      html $ mconcat ["", replacePage, ""]

{-| Function to return the board chess page with the number of users that are playing-}
getPlayersInGame :: IORef ChessInfo -> ActionM ()
getPlayersInGame chessInfoRef = do
  chessInfo <- liftIO $ readIORef chessInfoRef
  page <- toActionM $ replacePlayersInChessBoard $ players chessInfo
  html $ mconcat ["", page, ""]

{-| ----------------------------------------------}
{-|                    GAME UTILS                -}
{-| ----------------------------------------------}
writeChessInfoInIORef :: IORef ChessInfo -> [PlayerInfo] -> IO ()
writeChessInfoInIORef chessInfoRef playerInfoList = writeIORef chessInfoRef $ ChessInfo playerInfoList

replaceNameInPage :: String -> String -> Text
replaceNameInPage page name = replace playersTag (pack name) (pack page)

{-| Function to read the html page and transform in Text, also we use fmap name players to iterate the players collection
    and return a new one with only the attribute specify, in here it would be a list of names::String.
    We use the unwords function to extract the String in the list and concatenate.
    We also use [fromMaybe] sugar syntax function to extract the value of the maybe or else return default value.|-}
replacePlayersInChessBoard :: [PlayerInfo] -> IO Text
replacePlayersInChessBoard players = do
  page <- readFile chessBoardPath
  let playerNamesList = fmap name players
  let maybePlayersName = Just $ unwords playerNamesList
  let playersName = fromMaybe "No players register" maybePlayersName
  let pageWithName = replace playersTag (pack playersName) (pack page)
  return pageWithName

{-| Function to extract uri params by name-}
extractUriParam :: LazyText.Text -> ActionM String
extractUriParam = Web.Scotty.param

{-| Sugar syntax function where we expect any IO value and we use the Scotty [liftAndCatchIO] function to transform to [ActionM] monad
    Thanks to "eta reduction" sugar syntax we can skip function arguments. f a = x a  -> f = x-}
toActionM :: IO any -> ActionM any
toActionM = liftAndCatchIO

newtype PlayerInfo = PlayerInfo
  { name :: String
  }

newtype ChessInfo = ChessInfo
  { players :: [PlayerInfo]
  }
