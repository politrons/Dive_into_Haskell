module StateMachine where

import Control.Monad      (foldM)
import Data.List.NonEmpty
import Data.Text          (Text)
import Text.Printf        (printf)

{-| Types for the shopping-}
data Item = Item String Double deriving (Show, Eq)
data Card = Card String deriving (Show, Eq)
data Money = Money String deriving (Show, Eq)

{-| State Types for the state machine-}
data States
  = EmptyBasket
  | ItemInBasket (NonEmpty Item)
  | NoPayment (NonEmpty Item)
  | PaymentSelected (NonEmpty Item) Card
  | PaymentConfirmed (NonEmpty Item) Card
  deriving (Show, Eq)

{-| Actions Types for the state machine, that make the machine change from one state to another-}
data Actions
  = AddItem Item
  | RemoveItem Item
  | Checkout
  | AddPayment Card
  | Confirm
  deriving (Show, Eq)

{-| Alias Function for to avoid duplicate types in the function declaration.-}
type StateMachine state action =  state -> action -> IO state

{-|The function expect to receive two arguments the current state of the machine and the action,
   then it return a new state. -}
checkout:: StateMachine States Actions
-- First state we dont have items
checkout EmptyBasket (AddItem item) = return (ItemInBasket (item :| []))
--Possible second state add more items
checkout (ItemInBasket items) (AddItem item) = return (ItemInBasket (item <| items))
--Possible second state to the checkout
checkout (ItemInBasket items) Checkout =return (NoPayment items)
--Possible third state the select card to pay
checkout (NoPayment items) (AddPayment card) = return (PaymentSelected items card)
--Last state confirm card and pay
checkout (PaymentSelected items card) Confirm = return (PaymentConfirmed items card)

--Unhandled state
checkout state _ = return state

--To run the state machine
runFsm :: Foldable f => StateMachine state event -> state -> f event -> IO state
runFsm = foldM

--Function to print the state machine
withLogging :: (Show state, Show event )=> StateMachine state event  -> StateMachine state event
withLogging stateMachine state event = do newState <- stateMachine state event
                                          printf "- %s × %s → %s\n" (show state) (show event) (show newState)
                                          return newState
runStateMachine :: IO()
runStateMachine = do state <- runFsm
                               (withLogging checkout)
                               EmptyBasket
                               [ AddItem (Item "potatoes" 23.95)
                               , AddItem (Item "fish" 168.50)
                               , Checkout
                               , AddPayment (Card "0000-0000-0000-0000")
                               , Confirm
                               ]
                     print state


data State = S0 | S1 | S2

accepts :: State -> String -> Bool
accepts S0 ('a':xs) = accepts S1 xs
accepts S1 ('b':xs) = accepts S2 xs
accepts S2 ('c':xs) = True
accepts S2 _ = False

decide :: String -> Bool
decide = accepts S0


stateMachine :: IO()
stateMachine = print $ decide "abc"

stateMachineFalse :: IO()
stateMachineFalse = print $ decide "bca"