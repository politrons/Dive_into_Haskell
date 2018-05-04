module HighOrderFunctionsFeature
    ( highOrderOutput
    ) where

import Data.Char
import Data.List

-- |In Haskell You have two ways to define functions. With and without lambdas, if you go with without
-- approach you have to define all input arguments before the = and then you can use it in your function
--      function param1 param2 = "hello" ++ param1 ++ " Haskell rocks" ++ param2r

-- | We create a lambda functions that receive variable String name and return String
highOrderFunc :: String -> (String -> String)
highOrderFunc = \name -> \sentence -> name ++ " " ++sentence ++ " Combining"
--highOrderFunc name sentence = name ++ " " ++sentence ++ " Combining high order functions"

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- if the first list is contained, wholly and intact,anywhere within the second.
containsFunc :: String -> String
containsFunc = \sentence -> if(isInfixOf "Combining" sentence ) then sentence ++ " high order functions" else "Nothing"

-- |Function that receive a String param and response String
upperCaseFunc :: String -> String
--upperCaseFunction = \sentence -> map toUpper sentence ++ "!!!"
upperCaseFunc sentence = map toUpper sentence

-- |Here we compose two functions
outputSentence = upperCaseFunc (containsFunc (highOrderFunc "Hello" "Paul"))

-- |Creating and defining an Action IO
highOrderOutput :: IO ()

-- |Running the expression
highOrderOutput = putStrLn outputSentence

