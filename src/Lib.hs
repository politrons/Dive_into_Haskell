module Lib
    ( myFirstFunction
    ) where

--WE create a lambda function that receive variable String name and return String
lambdaFunc :: String -> String
--Function that receive a String param and response String
lambdaFunc = (\name -> "Hello " ++name++ ", Welcome to Haskell world at last!")
welcomeSentence = lambdaFunc "Paul"
--Creating and defining an Action IO
myFirstFunction :: IO ()
--Running the expression
myFirstFunction = putStrLn welcomeSentence
