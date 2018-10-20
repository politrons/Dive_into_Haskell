module StateMachine where

import Control.Monad      (foldM)
import Data.List.NonEmpty
import Data.Text          (Text)
import Text.Printf        (printf)


-- | State machine Types
-- -------------------------------
{-| Types for the shopping-}
data Item = Item String Double deriving (Show, Eq)
data Card = Card String deriving (Show, Eq)
data Money = Money String deriving (Show, Eq)

{-| State Types for the state machine-}
data States
   = EmptyBasket
  | ItemsInBasket [Item]
  | NoPayment [Item]
  | PaymentSelected [Item] Card
  | PaymentConfirmed [Item] Card
  deriving (Show, Eq)

{-| Actions Types for the state machine, that make the machine change from one state to another-}
data Actions
  = AddItem Item
  | RemoveItem Item
  | Checkout
  | AddPayment Card
  | Confirm
  deriving (Show, Eq)


-- | State machine implementation
-- -------------------------------
{-| Alias Function for to avoid duplicate types in the function declaration.-}
type StateMachine state action =  state -> action -> IO state

{-|The function expect to receive two arguments the current state of the machine and the action,
   then it return a new state. -}
myBasket:: StateMachine States Actions
{-| First state we dont have items -}
myBasket EmptyBasket (AddItem item) = return (ItemsInBasket $ createBasket item)
{-| Possible second state add more items-}
myBasket (ItemsInBasket items) (AddItem item) = return (ItemsInBasket $ addItem item items)
{-|Possible second state to the checkout-}
myBasket (ItemsInBasket items) Checkout = return (NoPayment items)
{-|Possible third state the select card to pay-}
myBasket (NoPayment items) (AddPayment card) = return (PaymentSelected items card)
{-|Last state confirm card and pay-}
myBasket (PaymentSelected items card) Confirm = return (PaymentConfirmed items card)
{-|Unhandled state-}
myBasket state _ = return state

-- | Utils functions
-- -------------------------------
createBasket :: Item -> [Item]
createBasket item = item : []

addItem :: Item -> [Item] -> [Item]
addItem item items = item : items

-- | Runner
-- -----------
runStateMachine :: IO()
runStateMachine = do state <- myBasket EmptyBasket $ AddItem (Item "Coca-cola" 1.95)
                     print ("State:"++ show state)
                     state <- myBasket state $ AddItem (Item "Twix" 1.50)
                     print ("State:"++ show state)
                     state <- myBasket state $ AddItem (Item "Pizza" 9.50)
                     print ("State:"++ show state)
                     state <- myBasket state Checkout
                     print ("State:"++ show state)
                     state <- myBasket state $ AddPayment (Card "0000-0000-0000-0000")
                     print ("State:"++ show state)
                     state <- myBasket state Confirm
                     print ("State:"++ show state)





