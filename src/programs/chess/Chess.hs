{-# LANGUAGE OverloadedStrings #-}

module Chess where

import           Control.Monad.IO.Class    (liftIO)
import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           Data.List
import           Data.List.Split
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text.Lazy            (Text, unpack)
import           Data.Text.Lazy            (pack)
import           Data.Text.Lazy            (replace)
import qualified Data.Text.Lazy            as LazyText
import           GHC.Generics
import           System.IO.Streams.Process (system)
import           Text.RawString.QQ

--Server
import           Web.Scotty
import           Web.Scotty                (ActionM, ScottyM, get, scotty, text)
import           Web.Scotty.Internal.Types (ActionT, File, Options, Param,
                                            RoutePattern, ScottyT)

--Board
import           ChessBoard

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
  chessInfo <- newIORef $ ChessInfo Map.empty initBoardGame
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
  let playersMap = players chessInfo
  if length playersMap == 2
    then do
      let replacePage = replace playersTag "Max number of users reach" page
      html $ mconcat ["", replacePage, ""]
    else do
      let playerPieces = getPlayerPieces (Map.elems playersMap)
      let playerInfoList =  Map.insert playerName (PlayerInfo playerName playerPieces) playersMap
      newChessInfo <- toActionM $ writeChessInfoInIORef chessInfoRef playerInfoList initBoardGame
      let replacePage = replaceNameInPage page playerName
      html $ mconcat ["", replacePage, ""]

{-| Function to return the board chess page with the number of users that are playing-}
getPlayersInGame :: IORef ChessInfo -> ActionM ()
getPlayersInGame chessInfoRef = do
  chessInfo <- liftIO $ readIORef chessInfoRef
  prepareBoardPage chessInfo

makeMoveInGame :: IORef ChessInfo -> ActionM ()
makeMoveInGame chessInfoRef = do
  playerName <- extractUriParam "player"
  from <- extractUriParam "from"
  to <- extractUriParam "to"
  chessInfo <- liftIO $ readIORef chessInfoRef
  chessInfo <- toActionM $ changeBoardPieces chessInfo playerName from to
  prepareBoardPage chessInfo

prepareBoardPage :: ChessInfo -> ActionM ()
prepareBoardPage chessInfo = do
  page <- toActionM $ prepareBoard chessInfo
  page <- toActionM $ replacePlayersInChessBoard page (Map.elems $ players chessInfo)
  html $ mconcat ["", page, ""]

{-| ----------------------------------------------}
{-|                    GAME UTILS                -}
{-| ----------------------------------------------}
changeBoardPieces :: ChessInfo -> String -> String -> String -> IO ChessInfo
changeBoardPieces chessInfo playerName from to = do
  let player = fromMaybe (PlayerInfo "" Map.empty) $ Map.lookup playerName (players chessInfo)
  newPlayerMovements <- replacePlayerMovements (movements player) from to
  let playerInfo = PlayerInfo (name player) newPlayerMovements
  --update chess in IORef
  return undefined


replacePlayerMovements :: Map String String -> String -> String -> IO (Map String String)
replacePlayerMovements playerMovements from to = do
  let fromPiece = fromMaybe "" $ Map.lookup from playerMovements
  let mapNewValue = Map.filter (/= from) $ Map.filter (/= to) playerMovements
  return $ Map.insert from "" $ Map.insert to fromPiece mapNewValue


{-| Function which having the chessInfo data type with the current game, we get the chess board page and
    we do the replacements of the pieces in the board-}
prepareBoard :: ChessInfo -> IO Text
prepareBoard chessInfo = do
  page <- readFile chessBoardPath
  boardGameWithMovements <- replaceMovementsInBoard chessInfo
  return $ replacePiecesInPhysicalBoard (pack page) (Map.toList boardGameWithMovements)

{-| Function that receive the [ChessInfo] and we extract the players info to replace the movements of player into the map board
    To achieve this we use [foldl] to iterate over the tuple of board movements and extract the player movements and set into
    a new Map-}
replaceMovementsInBoard :: ChessInfo -> IO (Map String String)
replaceMovementsInBoard chessInfo = do
  let player1Pieces =
        if null (players chessInfo)
          then initPlayerPieces1
          else movements (head (Map.elems $ players chessInfo))
  let player2Pieces =
        if isNullOrHasOnePlayer chessInfo
          then initPlayerPieces2
          else movements (last (Map.elems $ players chessInfo))
  let playersMovementsInBoard =
        foldl
          (\previousMap tuple ->
             Map.insert (fst tuple) (replacePlayerPiecesInBoard tuple player1Pieces player2Pieces) previousMap)
          Map.empty
          (Map.toList (boardGame chessInfo))
  return playersMovementsInBoard

{-| Function to replace the memory board [#movement] by the Player-}
replacePlayerPiecesInBoard :: (String, String) -> Map String String -> Map String String -> String
replacePlayerPiecesInBoard (key, boardPosition) player1Pieces player2Pieces =
  unpack $
  replace
    "#movement"
    (pack (fromMaybe (fromMaybe "" $ Map.lookup key player2Pieces) $ Map.lookup key player1Pieces))
    (pack boardPosition)

{-| Using [foldl] foldLeft function we can do the tail recursive calls just, together with eta reduce we can receive in
    any invocation of the function the page which is propagated in every recursive call together with the next iteration of
     the list of tuples.
    Also to get the first element of the tuple we use [fst] function and to get the second just [snd]-}
replacePiecesInPhysicalBoard :: Text -> [(String, String)] -> Text
replacePiecesInPhysicalBoard = foldl (\page tuple -> replace (pack ("#" ++ fst tuple)) (pack (snd tuple)) page)

{-| Classic no sugar syntax to do a fold over the list of tuples and propagate the applied changes over the page-}
--replacePage page (tuple:listOfTuples) = replacePage (replace (pack ("#" ++ fst tuple)) (pack (snd tuple)) page) listOfTuples
--replacePage page [] = page
writeChessInfoInIORef :: IORef ChessInfo -> Map String PlayerInfo -> Map String String -> IO ()
writeChessInfoInIORef chessInfoRef playerInfoList initBoardGame =
  writeIORef chessInfoRef $ ChessInfo playerInfoList initBoardGame

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

{-| Function to return the map of movements of player1 or player2-}
getPlayerPieces :: [PlayerInfo] -> Map String String
getPlayerPieces playersList =
  if null playersList
    then initPlayerPieces1
    else initPlayerPieces2

isNullOrHasOnePlayer :: ChessInfo -> Bool
isNullOrHasOnePlayer chessInfo = null (players chessInfo) || length (players chessInfo) == 1

{-| Function to extract uri params by name-}
extractUriParam :: LazyText.Text -> ActionM String
extractUriParam = Web.Scotty.param

{-| Sugar syntax function where we expect any IO value and we use the Scotty [liftAndCatchIO] function to transform to [ActionM] monad
    Thanks to "eta reduction" sugar syntax we can skip function arguments. f a = x a  -> f = x-}
toActionM :: IO any -> ActionM any
toActionM = liftAndCatchIO

data PlayerInfo = PlayerInfo
  { name          :: String
  , movements :: Map String String
  }

data ChessInfo = ChessInfo
  { players   :: Map String PlayerInfo
  , boardGame :: Map String String
  }

-------------------
--testReplacePlayerPieceInBoard :: IO ()
--testReplacePlayerPieceInBoard = do
--  response <-
--    replaceMovementsInBoard $
--    ChessInfo ([PlayerInfo "pol" initPlayerPieces1] ++ [PlayerInfo "esther" initPlayerPieces2]) initBoardGame
--  print response

--recursiveList :: Text -> [(String, String)] -> IO Text
--recursiveList value (tuple:xs) = do
--  let to = "c1"
--  let from = "a1"
--  let fromPiece = fromMaybe "" $ Map.lookup from playerPieces1
--  let mapAfter =
--        if fst tuple == to
--          then fromPiece
--          else snd tuple
--  return $ pack mapAfter
--recursiveList value [] = return value
