module LetAndWhere where

import Data.Char (toUpper)

-- | Let
-- ------
{-| The scope of the declarations is the expression and the right hand side of the declarations.
    Also if the declaration expect to receive an input value, it's consider as a high order function.
    [let] declaration is lazy by default and only when is invoked is when the declaration pass from
    lazy to eager.
-}
letDeclaration :: IO()
letDeclaration = do
              let letFunc z= 1+2+z
              value <- invokeLetDeclaration letFunc
              print value

invokeLetDeclaration :: (Integer -> Integer) -> IO Integer
invokeLetDeclaration letFunc =  return $ (letFunc 3)

{-| With [let] we can define also functions to be used in other functions as high order function.
    In this example we define a curried functions [curriedFunction] and is passed to the [highOrderFunc]
    as a second argument into the function, and then once we pass the last argument the function is finish.-}
highOrderFunction :: IO ()
highOrderFunction = do let curriedFunction = function "hello"
                       sentence <- highOrderFunc " world" curriedFunction
                       result <- return $ fmap toUpper sentence
                       print result

function :: String -> String -> String
function word word1 = word ++ word1 ++ "!!"

highOrderFunc :: String -> (String -> String) -> IO String
highOrderFunc word func = return $ func word

-- | Where
-- --------
--
--whereDeclararion :: IO ()
--whereDeclararion = do c <- a + b
--                      print c
--                      where
--                        a = 1
--                        b = 2
