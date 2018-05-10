module FunctionsFeature where


-- |We create a lambda functions that receive variable String name and return String
sentenceFunc :: String -> String
-- |Function that receive a String param and response String
nameFunc = (\name -> "Hello " ++name) :: String -> String
sentenceFunc = \sentence -> sentence ++ " Welcome to Haskell world at last!"
-- |Here we compose two functions
welcomeSentence = sentenceFunc (nameFunc "Paul")

-- | Haskell allow define multiple functions using same function description
func1, func2 :: Int -> Int -> Int
func1 a b = a + b
func2 a b = a * b

func1Output = func1 5 6
func2Output = func2 6 7


