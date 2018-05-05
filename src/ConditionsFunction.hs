module ConditionsFunction
    (
     messageFunc,
     isPaul,
     outputValue
    ) where

messageFunc :: String -> String -> String

-- | In this example we show how a simple nested if works
messageFunc name surname= if(name =="Paul")
                   then "Hey " ++ name ++ " how you doing!"
                   else if(surname == "Perez")
                        then surname ++".... Ah!, I know your family"
                   else "Do I know you?"

-- | In this example we show how if else works more similar to pattern matching.
messageCaseFunc name surname
             |  name == "Paul" = "Hey " ++ name ++ " how you doing!"
             |  surname == "Perez" = surname ++".... Ah!, I know your family"
             |  otherwise = "Do I know you?"

-- | Just like in Scala all if/else conditions return a value, so one more time Haskell dont allow mutability
isPaul = (\name -> name == "Paul") :: String -> Bool
outputValue = \name -> if(isPaul name)
                       then "Hey Paul"
                       else "Who are you"


