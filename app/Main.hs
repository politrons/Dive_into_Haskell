module Main where

import ArithmeticFunction
import FunctionsFeature
import HighOrderFunctionsFeature
import ConditionsFunction
import CollectionFunctions
import PipelineFunctions
import TuplesFunctions
import DataTypeFunctions
import MaybeFunctions
import FirstProgram
import ClassesAndInstances
import AsyncFunctions
import CategoryTheoryFunctions
import HttpServer
import ScottyHttpServer
import MySQLConnector
import ModelTypes
import CassandraConnector
import ConnectorManager
import KafkaConsumer
import KafkaProducer
import Control.Concurrent (threadDelay)
import ErrorHandling
import RestConnector
import CircuitBreaker
import Generics
import StateMachine
import EventSourcingPattern
import OmdbAPI
import MessageSystem
import LetAndWhere
import FileTransferSystem
import AppleAPI
import Adventure
import Chess

-- | This is the main entry program for Haskell, just like static void main or Java
--   You can only have one main declaration, so you have to point to one output or another

main :: IO ()


--main = chessServer

-- | Adventure Game
-- ------------------------

main = adventureServer

-- | Apple API
-- ------------------------

--main = appleServer

-- | File transfer program
-- ------------------------

--main = mainFileTransferProgram
--main = fileSystem
--main = receiverClient
--main = senderClient "somefile.txt"

-- | Let and Where
-- ---------------------

--main = letDeclaration
--main = highOrderFunction

-- | Event sourcing
-- ---------------------

--main = eventSourcingProgram

-- | Circuit breaker
-- ---------------------

--main  = do state <- selectAllCassandraUserCB $ Close [] 0
--           print state
--           state <- selectAllCassandraUserCB state
--           print state
--           state <- selectAllCassandraUserCB state
--           print state
--           state <- selectAllCassandraUserCB state
--           print state
--           state <- selectAllCassandraUserCB state
--           print state
--           state <- selectAllCassandraUserCB state
--           print state


-- | State machine
-- ---------------------

--main = stateMachine
--main = stateMachineFalse
--main = goShoppingWithMoney
--main = goShoppingWithCard

-- | Generics
-- --------------------

--main = genericFeature $ G1 (Car "bmw")
--main = genericFeature $ G2 (Truck "Volvo")

-- | Rest connector
-- ---------------------

--main = simpleGetRequest "http://httpbin.org/get"

-- | Error Handler
-- ---------------------

--main = bracketFeature
--main = bracketWithErrorHandler
--main = tryFeature
--main = catchFeature

-- |    Kafka
-- ---------------------

--main = startConsumer
--main = startProducer "Hello Kafka producer world"
--main = do _ <- startProducer "Hello Kafka world"
--          threadDelay 5000000
--          _ <- startConsumer
--          return ()

-- | Connector manager
-- ---------------------

--main = do result <- selectAllUsers
--          print result

-- | MySQL Connector
-- ------------------

--main = mysqlIO
--main = do user <- getUserById 1000
--          print  user
--main = do user <- insertUser User { userId = 1002, userName = "Paul2" }
--          print  user
--main = do address <- insertAddress $ Address 1 2 ""
--          print address
--main = do address <- getAddressById 1981
--          print address

-- | Cassandra Connector
-- -----------------------

--main = do result <- getConfigParam "portNumber"
--          print result
--main = do c <- getVersion
--          print c
--main  = do response <- selectAllCassandraUser
--           print response
--main  = do response <- selectCassandraUserById 1981
--           print response
--main  = createCassandraUser $ User 2000 "Paul"
--main  = do response <- deleteCassandraUserById 2000
--           print response

-- | Http Server
-- --------------

--main = myServer
--main = scottyServer

-- | Programs
-- --------------

--main = startServer
--main = messageSystem

-- | Monads
-- ------------

--main = composeMonads
--main = sumCompositionMonads
--main = print outputSumCombinations
--main = doubleXOutput
--main = multiplyCompositionMonads
--main = functorComposition
--main = justFunctor
--main = monadMaybeNumber
--main = monadMaybeString
--main = monadFlatMap3
--main = monoidString
--main = monoidMaybe
--main = applicativeMaybeIO
--main = do list <- return $ findInTwoListSameValues ["a","b","c"] ["f","g","c","e","a"]
--          print $ show list

-- | Async
-- ------------

--main = concurrentOutput
--main = raceOutput
--main = forkIOThreads
--main = asyncResponse
--main = multipleAsyncResponse
--main = combiningAsyncResponse
--main = combiningAsyncResponse1
--main = forkOSThreads
--main = fmapNumberAsync
--main = fmapSentenceAsync
--main = sync 5

-- | Functions
-- ------------

--main = putStrLn welcomeSentence
--main = print func1Output
--main = print func2Output
--main = functorDollar
--main = do value <- forkIOThreadsCallback
--          print value

-- | High order function
-- ----------------------

--main = putStrLn outputSentence
--main = print outputSentenceSeparator
--main = print outputFunc

-- | Pipeline
-- -------------

--main = print pipeline

-- | Conditions
-- -------------

--main = print (outputValue "Paul")
--main = putStrLn (messageFunc "Paul" "Perez")
--main = putStrLn (messageFunc "John" "Perez")
--main = putStrLn (messageFunc "John" "Smith")
--main = putStrLn (messageCaseFunc "Paul" "Perez")
--main = putStrLn (messageCaseFunc "John" "Perez")
--main = putStrLn (messageCaseFunc "John" "Smith")
--main = print outputPatternMatching

-- | Arithmetic
-- -------------

--main = numericOutput
--main = print subtractResponse
--main = print overloadIntOutput
--main = print overloadLongOutput

-- | Tuple
--  --------

--main  = print sorterNameOutput
--main  = print olderOutput

-- | Collections
--  -------------
-- List

--main = print multiValueList
--main = putStrLn ("The length of the list is " ++ (show (length multiValueList)))
--main = print firstElementOutput
--main = print addValueListOutput
--main = print reverseListOutput
--main = print firstListOutput
--main = print lastListOutput
--main = print findElementByIndex
--main = print joinListOutput
--main = print mapListOutput
--main = print filterListOutput
--main = print isElementThere
--main = print outputFind
--main = print outputListFunc
--main  = print $ sumAllElements 0 [1,2,3,4,5]

-- | Map

--main = print singletonMap
--main = print fromListMap
--main = print mapSize
--main = print getMapValueByKeyOutput
--main = print newAppendMap
--main = print isMemberPresentOutput
--main = print findWithDefaultFuncOutput
--main = print newDeletedMap
--main = print transformValuesToUpperCase
--main = print transformToUpperCaseByKey
--main = print transformToUpperCaseByKey
--main = print mapFoundByKeyAndLength
--main = print mapNotFoundByKeyAndLength

-- | Zip

--main = print zipList
--main = print zipWithOutput
--main = print unzipOutput


-- | Data types
-- ------------

--main = print outputMenu
--main = print outputOlderThan30
--main = print womenOutput

-- | Types classes
-- ----------------

--main = print output
--main = print sumIntArithmeticTypeClass
--main = print multiplyIntArithmeticTypeClass
--main = print sumDoubleArithmeticTypeClass
--main = print multiplyDoubleArithmeticTypeClass
--main = print compareIntOutput
--main = print compareDoubleOutput

-- | Maybe
--  --------

--main  = print withValue
--main  = print emptyValue
--main = print outputFindJust
--main = print outputFindNoting
--main = print valueOfMaybeFilled
--main = print valueOfMaybeNothing


-- | Program
--  --------
--main = outputProgram
