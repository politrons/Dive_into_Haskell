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
import           Data.Text.Lazy            (Text, unpack)
import           Data.Text.Lazy            (pack)
import           Data.Text.Lazy            (replace)
import qualified Data.Text.Lazy            as LazyText
import           GHC.Generics
import           Text.RawString.QQ

import           Data.Maybe                (fromMaybe)

import           System.IO.Streams.Process (system)

--Server
import           Web.Scotty
import           Web.Scotty                (ActionM, ScottyM, get, scotty, text)
import           Web.Scotty.Internal.Types (ActionT, File, Options, Param,
                                            RoutePattern, ScottyT)

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
  chessInfo <- newIORef $ ChessInfo [] initBoardGame
  scotty servicePort (routes chessInfo)

routes :: IORef ChessInfo -> ScottyM ()
routes chessInfoRef = do
  get "/service" responseService
  get "/chess/register/:playerName" $ registerPlayer chessInfoRef
  get "/chess/players" $ getPlayersInGame chessInfoRef
  get "/chess/:player/:from/:to" undefined -- $ processAction adventureInfoRef

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
  page <- toActionM $ prepareBoard chessInfo
  let playersList = players chessInfo
  if length playersList == 2
    then do
      let replacePage = replace playersTag "Max number of users reach" page
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
  page <- toActionM $ prepareBoard chessInfo
  page <- toActionM $ replacePlayersInChessBoard page (players chessInfo)
  html $ mconcat ["", page, ""]

makeMoveInGame :: IORef ChessInfo -> ActionM ()
makeMoveInGame chessInfoRef = do
  playerName <- extractUriParam "player"
  from <- extractUriParam "from"
  to <- extractUriParam "to"
  return undefined

{-| ----------------------------------------------}
{-|                    GAME UTILS                -}
{-| ----------------------------------------------}
prepareBoard ::ChessInfo ->  IO Text
prepareBoard chessInfo = do
  page <- readFile chessBoardPath
  return $ replacePiecesInBoard (pack page) (Map.toList (boardGame chessInfo))

{-| Using [foldl] foldLeft function we can do the tail recursive calls just, together with eta reduce we can receive in
    any invocation of the function the page which is propagated in every recursive call together with the next iteration of
     the list of tuples.
    Also to get the first element of the tuple we use [fst] function and to get the second just [snd]-}
replacePiecesInBoard :: Text -> [(String, String)] -> Text
replacePiecesInBoard = foldl (\page tuple -> replace (pack ("#" ++ fst tuple)) (pack (snd tuple)) page)

{-| Classic no sugar syntax to do a fold over the list of tuples and propagate the applied changes over the page-}
--replacePage page (tuple:listOfTuples) = replacePage (replace (pack ("#" ++ fst tuple)) (pack (snd tuple)) page) listOfTuples
--replacePage page [] = page
writeChessInfoInIORef :: IORef ChessInfo -> [PlayerInfo] -> IO ()
writeChessInfoInIORef chessInfoRef playerInfoList = writeIORef chessInfoRef $ ChessInfo playerInfoList initBoardGame

replaceNameInPage :: Text -> String -> Text
replaceNameInPage page name = replace playersTag (pack name) page

{-| Function to read the html page and transform in Text, also we use fmap name players to iterate the players collection
    and return a new one with only the attribute specify, in here it would be a list of names::String.
    We use the unwords function to extract the String in the list and concatenate.
    We also use [fromMaybe] sugar syntax function to extract the value of the maybe or else return default value.|-}
replacePlayersInChessBoard :: Text -> [PlayerInfo] -> IO Text
replacePlayersInChessBoard page players = do
  let playerNamesList = fmap name players
  let maybePlayersName = Just $ unwords playerNamesList
  let playersName = fromMaybe "No players register" maybePlayersName
  let pageWithName = replace playersTag (pack playersName) page
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

data ChessInfo = ChessInfo
  { players   :: [PlayerInfo]
  , boardGame :: Map String String
  }

initBoardGame =
  Map.fromList
    [ ("a1", "<div class='white'>&#9820;</div>")
    , ("b1", "<div class='black'>&#9822;</div>")
    , ("c1", "<div class='white'>&#9821;</div>")
    , ("d1", "<div class='black'>&#9819;</div>")
    , ("e1", "<div class='white'>&#9818;</div>")
    , ("f1", "<div class='black'>&#9821;</div>")
    , ("g1", "<div class='white'>&#9822;</div>")
    , ("h1", "<div class='black'>&#9820;</div>")
    , ("a2", "<div class='black'>&#9821;</div>")
    , ("b2", "<div class='white'>&#9821;</div>")
    , ("c2", "<div class='black'>&#9821;</div>")
    , ("d2", "<div class='white'>&#9821;</div>")
    , ("e2", "<div class='black'>&#9821;</div>")
    , ("f2", "<div class='white'>&#9821;</div>")
    , ("g2", "<div class='black'>&#9821;</div>")
    , ("h2", "<div class='white'>&#9821;</div>")
    , ("a3", "<div class='white'></div>")
    , ("b3", "<div class='black'></div>")
    , ("c3", "<div class='white'></div>")
    , ("d3", "<div class='black'></div>")
    , ("e3", "<div class='white'></div>")
    , ("f3", "<div class='black'></div>")
    , ("g3", "<div class='white'></div>")
    , ("h3", "<div class='black'></div>")
    , ("a4", "<div class='black'></div>")
    , ("b4", "<div class='white'></div>")
    , ("c4", "<div class='black'></div>")
    , ("d4", "<div class='white'></div>")
    , ("e4", "<div class='black'></div>")
    , ("f4", "<div class='white'></div>")
    , ("g4", "<div class='black'></div>")
    , ("h4", "<div class='white'></div>")
    , ("a5", "<div class='white'></div>")
    , ("b5", "<div class='black'></div>")
    , ("c5", "<div class='white'></div>")
    , ("d5", "<div class='black'></div>")
    , ("e5", "<div class='white'></div>")
    , ("f5", "<div class='black'></div>")
    , ("g5", "<div class='white'></div>")
    , ("h5", "<div class='black'></div>")
    , ("a6", "<div class='black'></div>")
    , ("b6", "<div class='white'></div>")
    , ("c6", "<div class='black'></div>")
    , ("d6", "<div class='white'></div>")
    , ("e6", "<div class='black'></div>")
    , ("f6", "<div class='white'></div>")
    , ("g6", "<div class='black'></div>")
    , ("h6", "<div class='white'></div>")
    , ("a7", "<div class='white'>&#9817;</div>")
    , ("b7", "<div class='black'>&#9817;</div>")
    , ("c7", "<div class='white'>&#9817;</div>")
    , ("d7", "<div class='black'>&#9817;</div>")
    , ("e7", "<div class='white'>&#9817;</div>")
    , ("f7", "<div class='black'>&#9817;</div>")
    , ("g7", "<div class='white'>&#9817;</div>")
    , ("h7", "<div class='black'>&#9817;</div>")
    , ("a8", "<div class='black'>&#9814;</div>")
    , ("b8", "<div class='white'>&#9816;</div>")
    , ("c8", "<div class='black'>&#9815;</div>")
    , ("d8", "<div class='white'>&#9813;</div>")
    , ("e8", "<div class='black'>&#9812;</div>")
    , ("f8", "<div class='white'>&#9815;</div>")
    , ("g8", "<div class='black'>&#9816;</div>")
    , ("h8", "<div class='white'>&#9814;</div>")
    ] :: Map String String
