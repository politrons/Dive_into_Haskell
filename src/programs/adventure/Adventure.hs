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
import Data.IORef (IORef,newIORef,readIORef)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy (pack)

servicePort = 3500 :: Int

{-| ----------------------------------------------}
{-|                    SERVER                    -}
{-| ----------------------------------------------}

{-| Using [scotty] passing [port] and [routes] we define the http server -}
adventureServer :: IO ()
adventureServer = do
                    print ("Starting Adventure Server at port " ++ show servicePort)
                    timelineRef <- newIORef $ AdventureInfo (PlayerInfo "" Human) (TimeLine 0)
                    scotty servicePort (routes timelineRef)

routes :: IORef AdventureInfo -> ScottyM()
routes timelineRef = do get "/service" responseService
                        get "/adventure/" $ startAdventure timelineRef
                        get "/adventure/name/:name/race/:race" processAction
                        get "/adventure/:action" processAction

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = do
                  let version = "3.0"
                  html $ mconcat ["<h1>Adventure Haskell server ",version,"</h1>"]

startAdventure :: IORef AdventureInfo -> ActionM ()
startAdventure adventureInfoRef = do adventureInfo <- liftIO $ readIORef adventureInfoRef
                                     story <- toActionM $ readStoryTimeLine (timeline adventureInfo)
                                     html $ mconcat ["",story,""]

processAction :: ActionM ()
processAction = do product <- extractUriParam "action"
--                   products <- toActionM $ findProduct ioRefManager product
                   json product

{-| Function to extract uri params by name-}
extractUriParam :: LazyText.Text -> ActionM String
extractUriParam param = Web.Scotty.param param

readStoryTimeLine :: TimeLine -> IO Text
readStoryTimeLine timeline = do fileContent <- readFile $ "src/programs/adventure/story/story" ++ show(state timeline) ++ ".html"
                                return $ pack fileContent

{-| Sugar syntax function where we expect any IO value and we use the Scotty [liftAndCatchIO] function to transform to [ActionM] monad-}
toActionM :: IO any -> ActionM any
toActionM any = liftAndCatchIO any

{-| ----------------------------------------------}
{-|                    MODEL                    -}
{-| ----------------------------------------------}
data Race = Human | Elf | Wizard | Dwarf

data TimeLine = TimeLine {state::Int}

data PlayerInfo = PlayerInfo {name::String, race::Race}

data AdventureInfo = AdventureInfo {playerInfo :: PlayerInfo, timeline :: TimeLine}