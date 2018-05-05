module FunctionsFeature
    (
    output
    ) where


-- |We create a lambda functions that receive variable String name and return String
sentenceFunc :: String -> String
-- |Function that receive a String param and response String
nameFunc = (\name -> "Hello " ++name) :: String -> String
sentenceFunc = \sentence -> sentence ++ " Welcome to Haskell world at last!"
-- |Here we compose two functions
welcomeSentence = sentenceFunc (nameFunc "Paul")
-- |Creating and defining an Action IO
output :: IO ()
-- |Running the expression
output = putStrLn welcomeSentence
