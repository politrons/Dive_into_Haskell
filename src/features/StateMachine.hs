module StateMachine where

import Control.Monad      (foldM)
import Data.Text          (Text)
import Text.Printf        (printf)

-- | State machine implementation
-- -------------------------------
{-| Alias Function for to avoid duplicate types in the function declaration.-}
type StateMachine state action =  state -> action -> IO state

{-|Haskell has a very elegant way to do overload of functions, more than overload to catch invocation values
   to assign to a specific function implementation.
   In this case, depending of the State and Action arguments received for the function it will redirect to
   a specific implementation, and in case we cannot find any, it will go to the last implementation.
   You can think about this like a giant pattern matching.
   The function expect to receive two arguments the current state of the machine and the action,
   then it return a new state. -}
myBasket:: StateMachine States Actions
{-| First state we don't have items so we create the basket with one product -}
myBasket EmptyBasket (AddItem item) = return (ItemsInBasket $ createBasket item)
{-| Possible second state add more items-}
myBasket (ItemsInBasket items) (AddItem item) = return (ItemsInBasket $ addItem item items)
{-| Possible second state remove item from the basket-}
myBasket (ItemsInBasket items) (RemoveItem item) = return (ItemsInBasket $ removeItem item items)
{-| Possible second state to the checkout-}
myBasket (ItemsInBasket items) Checkout = return (NoPaymentSelected items)
{-| Possible third state the select card to pay-}
myBasket (NoPaymentSelected items) (AddPayment payment) = return (PaymentSelected items payment)
{-| Last state confirm card and pay-}
myBasket (PaymentSelected items payment) Confirm = return (Check items (Total $ sumAllPrices 0 items) payment)
{-| Unhandled state-}
myBasket state _ = return state

-- | Utils functions
-- -------------------------------
createBasket :: Item -> [Item]
createBasket item = item : []

addItem :: Item -> [Item] -> [Item]
addItem item items = item : items

removeItem :: Item -> [Item] -> [Item]
removeItem item items = filter (\element -> desc element /= desc item) items

{-|Fold Function that make a recursive call to sum an element of the list and call the function again
   with the new increase value and the list without that element that we just sum.
  (item:items) with collection is a cool feature of Haskell where when you have a collection you receive
  the item and the list of items without that item. Very handy for reduce.-}
sumAllPrices :: Double -> [Item] -> Double
sumAllPrices first (item:items) = sumAllPrices (first + (price item)) items
sumAllPrices first [] = first -- Last condition. When the list is empty we break the recursion

-- | State machine Types
-- -------------------------------
{-| Types for the shopping-}
data Item = Item {desc::String, price::Double} deriving (Show, Eq)
data Card = Card String deriving (Show, Eq)
data Money = Money Double deriving (Show, Eq)
data Total = Total Double deriving (Show, Eq)
data Basket = Basket [Item] Money

{-| State Types for the state machine-}
data States
   = EmptyBasket
  | ItemsInBasket [Item]
  | NoPaymentSelected {items::[Item]}
  | PaymentSelected [Item] (Either Card Money)
  | Check [Item] Total (Either Card Money)
  deriving (Show, Eq)

{-| Actions Types for the state machine, that make the machine change from one state to another-}
data Actions
  = AddItem Item
  | RemoveItem Item
  | Checkout
  | AddPayment (Either Card Money)
  | Confirm
  deriving (Show, Eq)

-- | Runner
-- -----------
goShoppingWithMoney :: IO()
goShoppingWithMoney = do state <- myBasket EmptyBasket $ AddItem (Item "Pepsi" 1.95)
                         print ("EmptyBasket: "++ show state)
                         state <- myBasket state $ AddItem (Item "Donuts" 1.50)
                         print ("AddItem: "++ show state)
                         state <- myBasket state $ AddItem (Item "Burgers" 4.50)
                         print ("AddItem: "++ show state)
                         state <- myBasket state $ RemoveItem (Item "Donuts" 1.50)
                         print ("RemoveItem: "++ show state)
                         state <- myBasket state $ AddItem (Item "Budbeiser pack" 6.00)
                         print ("AddItem: "++ show state)
                         state <- myBasket state Checkout
                         print ("Checkout: "++ show state)
                         state <- myBasket state $ AddPayment (Right $ Money $ sumAllPrices 0 (items state))
                         print ("AddPayment: "++ show state)
                         state <- myBasket state Confirm
                         print ("Confirm: "++ show state)

goShoppingWithCard :: IO()
goShoppingWithCard = do state <- myBasket EmptyBasket $ AddItem (Item "Coca-cola" 1.95)
                        print ("EmptyBasket: "++ show state)
                        state <- myBasket state $ AddItem (Item "Twix" 1.50)
                        print ("AddItem: "++ show state)
                        state <- myBasket state $ AddItem (Item "Pizza" 9.50)
                        print ("AddItem: "++ show state)
                        state <- myBasket state $ RemoveItem (Item "Twix" 1.50)
                        print ("RemoveItem: "++ show state)
                        state <- myBasket state Checkout
                        print ("Checkout: "++ show state)
                        state <- myBasket state $ AddPayment (Left $ Card "1234-5678-9123-5678")
                        print ("AddPayment: "++ show state)
                        state <- myBasket state Confirm
                        print ("Confirm: "++ show state)

