{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module Adventure where
--Server
import Web.Scotty
import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)
import Web.Scotty (ScottyM,scotty,ActionM,get,text)
import qualified Data.Text.Lazy as LazyText
import Text.RawString.QQ
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy (replace)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split

servicePort = 3500 :: Int

{-| ----------------------------------------------}
{-|                    SERVER                    -}
{-| ----------------------------------------------}

{-| Using [scotty] passing [port] and [routes] we define the http server -}
adventureServer :: IO ()
adventureServer = do
                    print ("Starting Adventure Server at port " ++ show servicePort)
                    timelineRef <- newIORef $ AdventureInfo (PlayerInfo "" (Race "")) (TimeLine 0)
                    scotty servicePort (routes timelineRef)

routes :: IORef AdventureInfo -> ScottyM()
routes adventureInfoRef = do get "/service" responseService
                             get "/adventure/" $ startAdventure adventureInfoRef
                             get "/adventure/name/:name/race/:race" $ createCharacter adventureInfoRef
                             get "/adventure/:action" $ processAction adventureInfoRef

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = do
                  let version = "3.0"
                  html $ mconcat ["<h1>Adventure Haskell server ",version,"</h1>"]

startAdventure :: IORef AdventureInfo -> ActionM ()
startAdventure adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                     story <- toActionM $ readStoryTimeLine ("story" ++ show(state (timeline adventureInfo)) ++ ".html")
                                     html $ mconcat ["",story,""]

createCharacter :: IORef AdventureInfo -> ActionM ()
createCharacter adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                      name <- extractUriParam "name"
                                      race <- extractUriParam "race"
                                      raceMaybe <- toActionM $ extractRace race
                                      story <- toActionM $ updatePlayerInfo name raceMaybe adventureInfoRef
                                      html $ mconcat ["",story,""]

processAction :: IORef AdventureInfo -> ActionM ()
processAction adventureInfoRef = do action <- extractUriParam "action"
                                    playerActions <- return $ splitOn " " action
                                    timeline <- toActionM $ getTimeLineState adventureInfoRef
                                    puzzleResult <- return $ findActionsInChapterActions timeline playerActions
                                    storyPage <- toActionM $ case puzzleResult of
                                                    True -> do page <- getStoryPage adventureInfoRef
                                                               return page
                                                    False -> do errorPage <- getStoryErrorPage adventureInfoRef
                                                                return errorPage
                                    story <- toActionM $ readStoryTimeLine storyPage
                                    html $ mconcat ["",story,""]

{-| ----------------------------------------------}
{-|                    GAME LOGIC                -}
{-| ----------------------------------------------}

actionsPerChapter :: Map(Int)[String]
actionsPerChapter = Map.fromList [(1,["run","chase","race", "speed", "rush", "dash", "hurry", "career", "barrel"])]


findActionsInChapterActions ::TimeLine -> [String] -> Bool
findActionsInChapterActions timeline playerActions = case maybeActions of
                                                            Just actions -> (length list) > 0
                                                                 where
                                                                  list = playerActions >>= \playerAction -> filter(\chapterAction -> playerAction == chapterAction) actions
                                                            Nothing -> False
                                                            where
                                                             maybeActions = Map.lookup (state timeline) actionsPerChapter

{-| ----------------------------------------------}
{-|                    GAME UTILS                -}
{-| ----------------------------------------------}

extractRace :: String -> IO (Maybe Race)
extractRace race = case race of
                        "Elf" -> return $ Just $ Race "Elf"
                        "Human" -> return $ Just $ Race "Human"
                        "Dwarf" -> return $ Just $ Race "Dwarf"
                        "Wizard" -> return $ Just $ Race "Wizard"
                        _ -> return Nothing

{-| Function to get the maybe race and return the html page with success or error
    To replace some text from the html pages we use [replace] function -}
updatePlayerInfo :: String -> Maybe Race -> IORef AdventureInfo -> IO Text
updatePlayerInfo name raceMaybe adventureInfoRef = case raceMaybe of
                                                   Just race -> do newPlayerInfo <- writeIORef adventureInfoRef (AdventureInfo (PlayerInfo name race)(TimeLine 1))
                                                                   story <- readStoryTimeLine "playerCreated.html"
                                                                   story <- return $ replace "#name" (pack name) story
                                                                   story <- return $ replace "#race" (pack (raceName race)) story
                                                                   return story
                                                   Nothing -> do story <- readStoryTimeLine "error.html"
                                                                 return story

{-| Function to extract uri params by name-}
extractUriParam :: LazyText.Text -> ActionM String
extractUriParam param = Web.Scotty.param param

{-| Function to find the next page in the timeline of your adventure-}
getStoryPage :: IORef AdventureInfo -> IO String
getStoryPage adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                   return $ case Map.lookup (state (timeline adventureInfo)) chapterPages of
                                              Just page -> page
                                              Nothing -> "error.html"

{-| Function to find the next error page in the timeline of your adventure-}
getStoryErrorPage :: IORef AdventureInfo -> IO String
getStoryErrorPage adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                        return $ case Map.lookup (state (timeline adventureInfo)) chapterErrorPages of
                                                  Just page -> page
                                                  Nothing -> "error.html"

{-| Function to find the timeline of your adventure-}
getTimeLineState :: IORef AdventureInfo -> IO TimeLine
getTimeLineState adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                       return $ timeline adventureInfo

{-| Function to read the html page and transform in Text|-}
readStoryTimeLine :: String -> IO Text
readStoryTimeLine page = do fileContent <- readFile $ "src/programs/adventure/story/" ++ page
                            return $ pack fileContent

{-| Sugar syntax function where we expect any IO value and we use the Scotty [liftAndCatchIO] function to transform to [ActionM] monad-}
toActionM :: IO any -> ActionM any
toActionM any = liftAndCatchIO any

{-| ----------------------------------------------}
{-|                  GAME CHAPTERS               -}
{-| ----------------------------------------------}

chapterPages :: Map(Int)(String)
chapterPages = Map.fromList [(0,"story0.html"),(1,"story1.html"),(2,"story2.html"),(3,"story3.html")]

chapterErrorPages :: Map(Int)(String)
chapterErrorPages = Map.fromList [(0,"storyError0.html"),(1,"storyError1.html"),(2,"storyError2.html"),(3,"storyError3.html")]

{-| ----------------------------------------------}
{-|                    MODEL                     -}
{-| ----------------------------------------------}
data Race = Race {raceName::String}

data TimeLine = TimeLine {state::Int}

data PlayerInfo = PlayerInfo {name::String, race::Race}

data AdventureInfo = AdventureInfo {playerInfo :: PlayerInfo, timeline :: TimeLine}