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

{-| Using [scotty] passing [port] and [routes] we define the http server.
    We also keep the state of the player using [IORef] so we can move that state [AdventureInfo] around the program-}
adventureServer :: IO ()
adventureServer = do
                    print ("Starting Adventure Server at port " ++ show servicePort)
                    timelineRef <- newIORef $ AdventureInfo (PlayerInfo "" (Race "")) (TimeLine 0 (Attempts 0))
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

{-| Function to create the adventure and point our hero in the first chapter-}
startAdventure :: IORef AdventureInfo -> ActionM ()
startAdventure adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                     story <- toActionM $ readStoryTimeLine ("story" ++ show(state (timeline adventureInfo)) ++ ".html")
                                     html $ mconcat ["",story,""]

{-| Function to create the character with the information provided by the client-}
createCharacter :: IORef AdventureInfo -> ActionM ()
createCharacter adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                      name <- extractUriParam "name"
                                      race <- extractUriParam "race"
                                      raceMaybe <- toActionM $ extractRace race
                                      story <- toActionM $ updatePlayerInfo name raceMaybe adventureInfoRef
                                      html $ mconcat ["",story,""]

{- | Main function of the game where we will process all chapters of the game-}
processAction :: IORef AdventureInfo -> ActionM ()
processAction adventureInfoRef = do action <- extractUriParam "action"
                                    playerActions <- return $ splitOn " " action
                                    timeline <- toActionM $ getTimeLineState adventureInfoRef
                                    puzzleResult <- return $ processActionsInChapterActions timeline playerActions
                                    storyPage <- toActionM $ processChapterResult puzzleResult adventureInfoRef
                                    story <- toActionM $ readStoryTimeLine storyPage
                                    html $ mconcat ["",story,""]

{-| ----------------------------------------------}
{-|                    GAME LOGIC                -}
{-| ----------------------------------------------}

{-| Collection with all possible good actions for a puzzle-}
actionsPerChapter :: Map(Int)[String]
actionsPerChapter = Map.fromList [(1,["run","chase","race", "speed", "rush", "dash", "hurry", "career", "barrel"]),
                                  (2,["fight","duel","battle","attack","action","rumble"]),
                                  (3,["see","wall","watch","torch","look"]),
                                  (4,["nothing"])]



{-| Function to process the action and return true/false-}
processActionsInChapterActions ::TimeLine -> [String] -> Bool
processActionsInChapterActions timeline playerActions = case maybeActions of
                                                            Just actions -> (length list) > 0
                                                                 where
                                                                  list = playerActions >>= \playerAction -> filter(\chapterAction -> playerAction == chapterAction) actions
                                                            Nothing -> False
                                                            where
                                                             maybeActions = Map.lookup (state timeline) actionsPerChapter

{-| Function to get the result and in case of error we increase the number of attempts and ultimately end the game-}
processChapterResult :: Bool -> IORef AdventureInfo -> IO String
processChapterResult puzzleResult adventureInfoRef = case puzzleResult of
                                                      True -> do adventureInfoRef <- updatePlayerToNextLevel adventureInfoRef
                                                                 page <- getStoryPage adventureInfoRef chapterPages
                                                                 return page
                                                      False -> do adventureInfoRef <- updatePlayerToError adventureInfoRef
                                                                  maxAttemptsReached <- processNumberOfErrors adventureInfoRef
                                                                  page <- case maxAttemptsReached of
                                                                            True -> do errorPage <- getStoryPage adventureInfoRef chapterGameOverPages
                                                                                       return errorPage
                                                                            False -> do errorPage <- getStoryPage adventureInfoRef chapterErrorPages
                                                                                        return errorPage
                                                                  return page

{-| Function just to check the max number of errors of the player in one chapter-}
processNumberOfErrors :: IORef AdventureInfo -> IO Bool
processNumberOfErrors adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                            return (number (attempts (timeline adventureInfo)) == 3)

{-| Function to get the maybe race and return the html page with success or error
    To replace some text from the html pages we use [replace] function -}
updatePlayerInfo :: String -> Maybe Race -> IORef AdventureInfo -> IO Text
updatePlayerInfo name raceMaybe adventureInfoRef = case raceMaybe of
                                                   Just race -> do _ <- writeIORef adventureInfoRef (AdventureInfo (PlayerInfo name race)(TimeLine 1 (Attempts 0)))
                                                                   story <- readStoryTimeLine "playerCreated.html"
                                                                   story <- return $ replace "#name" (pack name) story
                                                                   story <- return $ replace "#race" (pack (raceName race)) story
                                                                   story <- return $ replaceRaceNameByImageInStory story (raceName race)
                                                                   return story
                                                   Nothing -> do story <- readStoryTimeLine "error.html"
                                                                 return story

{-| Function to create a new AdventureInfo with the new information for the next level-}
updatePlayerToNextLevel :: IORef AdventureInfo -> IO (IORef AdventureInfo)
updatePlayerToNextLevel adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                              _ <- writeIORef adventureInfoRef (AdventureInfo (playerInfo adventureInfo)(TimeLine (state (timeline adventureInfo) + 1) (Attempts 0)))
                                              return adventureInfoRef

{-| Function to create a new AdventureInfo with increasing the number of errors per chapter-}
updatePlayerToError :: IORef AdventureInfo -> IO (IORef AdventureInfo)
updatePlayerToError adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                          _ <- writeIORef adventureInfoRef(AdventureInfo(playerInfo adventureInfo)(TimeLine (state (timeline adventureInfo)) (Attempts (number (attempts (timeline adventureInfo)) + 1))))
                                          return adventureInfoRef

{-| ----------------------------------------------}
{-|                    GAME UTILS                -}
{-| ----------------------------------------------}

{-| Function to add in the story the race image using the raceName-}
replaceRaceNameByImageInStory :: Text -> String -> Text
replaceRaceNameByImageInStory story raceName = case raceName of
                                      "Human" -> replace "#playerImage" humanImage story
                                      "Elf" -> replace "#playerImage" elfImage story
                                      "Dwarf" -> replace "#playerImage" elfImage story
                                      "Wizard" -> replace "#playerImage" wizardImage story

{-| Function to transform the input race from the customer in the Game Race type-}
extractRace :: String -> IO (Maybe Race)
extractRace race = case race of
                        "Elf" -> return $ Just $ Race "Elf"
                        "Human" -> return $ Just $ Race "Human"
                        "Dwarf" -> return $ Just $ Race "Dwarf"
                        "Wizard" -> return $ Just $ Race "Wizard"
                        _ -> return Nothing


{-| Function to extract uri params by name-}
extractUriParam :: LazyText.Text -> ActionM String
extractUriParam param = Web.Scotty.param param

{-| Function to find the next page in the timeline of your adventure-}
getStoryPage :: IORef AdventureInfo -> Map(Int)(String) -> IO String
getStoryPage adventureInfoRef collectionPage = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                                  return $ case Map.lookup (state (timeline adventureInfo)) collectionPage of
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
{-| Collections of chapter, error and game over pages of the game-}

chapterPages :: Map(Int)(String)
chapterPages = Map.fromList [(0,"story0.html"),(1,"story1.html"),(2,"story2.html"),(3,"story3.html"),(4,"story4.html"),(5,"story5.html")]

chapterErrorPages :: Map(Int)(String)
chapterErrorPages = Map.fromList [(1,"storyError1.html"),(2,"storyError2.html"),(3,"storyError3.html"),(4,"storyError4.html")]

chapterGameOverPages :: Map(Int)(String)
chapterGameOverPages = Map.fromList [(1,"storyOver1.html"),(2,"storyOver2.html"),(3,"storyOver3.html"),(4,"storyOver4.html")]

{-| ----------------------------------------------}
{-|                    MODEL                     -}
{-| ----------------------------------------------}
data Attempts = Attempts {number::Int}

data Race = Race {raceName::String}

data TimeLine = TimeLine {state::Int, attempts :: Attempts}

data PlayerInfo = PlayerInfo {name::String, race::Race}

data AdventureInfo = AdventureInfo {playerInfo :: PlayerInfo, timeline :: TimeLine}

humanImage= "https://static.squarespace.com/static/51b3dc8ee4b051b96ceb10de/51ce6099e4b0d911b4489b79/51ce619ce4b0d911b449a17f/1316719790027/1000w/ranger_and_gondorian_by_otisframpton-d3crwha.jpeg"
wizardImage="https://static.squarespace.com/static/51b3dc8ee4b051b96ceb10de/51ce6099e4b0d911b4489b79/51ce619ce4b0d911b449a17e/1316719755098/1000w/wizard_and_hobbit_by_otisframpton-d3crwd0.jpeg"
elfImage="https://static.squarespace.com/static/51b3dc8ee4b051b96ceb10de/51ce6099e4b0d911b4489b79/51ce619ce4b0d911b449a180/1316719808523/1000w/elf_and_dwarf_by_otisframpton-d3crwmm.jpeg"
