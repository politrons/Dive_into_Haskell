module ErrorHandling where

import Control.Exception (bracket,SomeException,evaluate,try,catch)
import Data.Char (toUpper,digitToInt)

{-| -------------
     Bracket
   --------------
 When you want to acquire a resource, do some work with it, and
 then release the resource, it is a good idea to use 'bracket',
 because 'bracket' will install the necessary exception handler to
 release the resource in the event that an exception is raised.

-- :: IO init            -- ^ computation to run first
-- -> (a -> IO last)     -- ^ computation to run last
-- -> (a -> IO between)  -- ^ computation to run in-between
-}
bracketFeature :: IO ()
bracketFeature = bracket init last between
                  where
                  init = do text <- return "Hello Bracket initial function"
                            print $ text ++ ":init"
                            return text
                  between param = print $ map toUpper $ (param ++ ":between function")
                  last param = print $ map toUpper $ (param ++ ":last function")
{-|
  Using [bracket] we can control the possible error in the between function  and run the last function.
  Just as like Try/catch/finally in imperative programing.
-}
bracketWithErrorHandler :: IO ()
bracketWithErrorHandler = bracket init last between
                  where
                  init = do text <- return "Hello Bracket with error handler"
                            print $ text ++ ":init"
                            return text
                  between param = print $ digitToInt 'h' -- This will provoke an error
                  last param = print $ map toUpper $ (param ++ ":last action")

{-| Bracket without where-}
bracketFeatureNoSugar :: IO ()
bracketFeatureNoSugar = bracket functionInit  functionLast functionInBetween

functionInit:: IO String
functionInit = do text <- return "Hello Bracket function execution"
                  print $ text ++ ":init"
                  return text

functionInBetween:: String -> IO()
functionInBetween param = print $ map toUpper $ (param ++ ":between action")

functionLast:: String -> IO()
functionLast param = print $ map toUpper $ (param ++ ":last action")

{-| -------------
        Try
   --------------
 Example that show how use try-evaluate functions with error and succeed result.
-}
tryFeature :: IO ()
tryFeature = do output1 <- tryWithParam 'h'
                output2 <- tryWithParam '1'
                print $ output1
                print $ output2

{-| With [try] function together with [evaluate] function we can control all effects in action
    it will return an Either of Exception or Type defined in the evaluate in the IO type. -}
tryWithParam :: Char -> IO String
tryWithParam param = do eitherResult <- try (evaluate (digitToInt param)) :: IO (Either SomeException Int)
                        case eitherResult of
                            Left ex  -> return $ "Exception handled: " ++ show ex
                            Right value -> return $ "The character as Int is: " ++ show value

{-| -------------
        Catch
   --------------
  [Catch] function together with [handler] allow execute an IO computation and in case there's an
  exception is controlled in the handler. -}
catchFeature :: IO ()
catchFeature = do output1 <- catchWithParam 'h'
                  output2 <- catchWithParam '1'
                  print ""

catchWithParam :: Char -> IO ()
catchWithParam param = catch (print $ digitToInt param) handler
                          where
                            handler :: SomeException -> IO ()
                            handler ex = putStrLn $ "Exception Catch!: " ++ show ex