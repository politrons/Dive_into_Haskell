module ArithmeticFunction
    (
     numericOutput
    ) where

-- |We create a lambda functions that receive variable String name and return String
arithmeticFunction :: Int -> Int
-- |You have two ways to define functions. With and without lambdas
--arithmeticFunction number = number * 100 --The same than the line below
arithmeticFunction = \number -> number * 100
-- |combining functions
responseValue = arithmeticFunction (arithmeticFunction 5)
-- |Here we define the IO action
numericOutput :: IO ()
-- |Running the expression. Using show operator we transform from Int -> String
numericOutput = putStrLn (show responseValue)
-- |print it will print in screen all types that implement show. Since Int is one of them, we're good!
--numericOutput = print responseValue