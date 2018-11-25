{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module EventSourcingPattern where

import Data.Text.Lazy (Text,pack)

{-| We embrace type system and Type class pattern that's why we type all primitive types of our system-}
data Product = Product {productVal::String}deriving (Show, Eq)
data Price = Price {priceVal::Double}deriving (Show, Eq)
data Discount = Discount { discountVal::Double }deriving (Show, Eq)

{-| To define this example we will use the typical grocery basket to be rehydrate -}
data Basket = Basket{products::[Product], totalPrice::Price, totalDiscount::Discount} deriving (Show, Eq)

{-| Data type to define all possible events-}
data Event =
   BasketCreated{ basket:: Basket}
  | ProductAdded{ product::Product, price::Price}
  | ProductRemoved{ product::Product, price::Price}
  | DiscountAdded{ discount::Discount }
  deriving (Show, Eq)

{-|  COMMANDS  -}
{-| ------------}
{-| In Event sourcing pattern [Commands] are the responsible to apply an action that lead into the creation of Events.
    Those events are the agents that can allow us to rehydrate an entity into a particular state in the future.-}

{-| Command function apply the command and create event [BasketCreated] to be rehydrate later on-}
createBasketCommand :: IO Event
createBasketCommand = return $ BasketCreated $ Basket [] (Price 0) (Discount 0)

{-| Command function apply the command and create event [ProductAdded] to be rehydrate later on-}
addProductCommand :: Basket -> Product -> Price -> IO Event
addProductCommand basket product price = return $ ProductAdded product price

{-| Command function apply the command and create event [ProductRemoved] to be rehydrate later on-}
removeProductCommand :: Basket -> Product -> Price -> IO Event
removeProductCommand basket product price = return $ ProductRemoved product price

{-| Command function apply the command and create event [DiscountAdded] to be rehydrate later on-}
addDiscountCommand :: Basket -> Discount -> IO Event
addDiscountCommand basket discount = return $ DiscountAdded discount

{-|  EVENTS    -}
{-| ------------}
{-| In Event sourcing pattern [Events] are the events/actions that after apply into an entity, it set in a particular state-}

{-| Declarative function for all Events implementations-}
applyEvent :: Basket -> Event -> Basket

{-| Pure function to create an empty basket-}
applyEvent basket (BasketCreated _basket) = basket

{-| Pure function to create a new Basket adding a product in basket-}
applyEvent basket (ProductAdded product price)= Basket (products basket ++ [product]) (increaseAmount basket price) (totalDiscount basket)

{-| Pure function to create a new Basket adding a discount in basket-}
applyEvent basket (DiscountAdded discount) = Basket (products basket)  (reduceAmount basket discount) (increaseAmount basket discount)

{-| Pure function to create a new Basket removing the product and price from previous basket-}
applyEvent basket (ProductRemoved product price) = Basket (removeProductFromBasket product (products basket)) (reduceAmount basket price) (totalDiscount basket)


{-|  PERSISTENCE/REHYDRATE -}
{-| ------------------------}

{-| Function that return a new [Event] appending the new event.
   This function It could be whatever backend (cassandra, couchbase, mongodb) we want to use to persist -}
appendEvent :: [Event] -> Event -> IO [Event]
appendEvent events event = return $ events ++ [event]

{-| Fold Function to receive the array of events and recursively apply the function [applyEvent] over the basket
    depending the type of Event is passed.-}
rehydrateByEvents :: Basket -> [Event] -> Basket
rehydrateByEvents basket (event:events) = rehydrateByEvents (applyEvent basket event) events
rehydrateByEvents basket [] = basket

{-| Utils  -}
{-| --------}
{-| Function to filter from the list of products the one we want to remove-}
removeProductFromBasket :: Product -> [Product] -> [Product]
removeProductFromBasket productToRemove products = filter (\product -> product /= productToRemove) products

{-| Type class pattern implementation to define two implementation with same signature for Price/Discount reducing a lot of boilerplate-}
class ReduceAmount basket moneyToReduce newMoney where
   reduceAmount:: basket -> moneyToReduce -> newMoney

instance ReduceAmount Basket Price Price where
   reduceAmount basket price = Price $ (priceVal (totalPrice basket)) - (priceVal price)

instance ReduceAmount Basket Discount Price where
   reduceAmount basket discount = Price $ (priceVal (totalPrice basket)) - (discountVal discount)

class IncreaseAmount basket amountToIncrease amount where
   increaseAmount:: basket -> amountToIncrease -> amount

instance IncreaseAmount Basket Price Price where
    increaseAmount basket price = Price $ (priceVal (totalPrice basket) + (priceVal price))

instance IncreaseAmount Basket Discount Discount where
    increaseAmount basket discount = Discount $ (discountVal (totalDiscount basket)) + (discountVal discount)

{-|   Program  -}
{-| ------------}

eventSourcingProgram :: IO()
eventSourcingProgram = do  print "############### PERSISTANCE COMMANDS #################"
                           event <- createBasketCommand
                           events <- appendEvent [] event
                           event <- addProductCommand (basket event) (Product "Coca-cola")  (Price 2.50)
                           events <- appendEvent events event
                           event <- addProductCommand (basket event) (Product "Buddbeiser") (Price 3.00)
                           events <- appendEvent events event
                           event <- addDiscountCommand (basket event) (Discount 0.50)
                           events <- appendEvent events event
                           event <- addProductCommand (basket event) (Product "Nachos") (Price 1.20)
                           events <- appendEvent events event
                           event <- addDiscountCommand (basket event) (Discount 0.20)
                           events <- appendEvent events event
                           event <- addProductCommand (basket event) (Product "Pepsi") (Price 2.40)
                           events <- appendEvent events event
                           event <- removeProductCommand (basket event) (Product "Coca-cola") (Price 2.50)
                           events <- appendEvent events event
                           mapM_ print events -- [mapM_] fold function to make print as println
                           print "############### REHYDRATE EVENTS #################"
                           basket <- return $ rehydrateByEvents (Basket [] (Price 0) (Discount 0)) events
                           printBasket basket

{-| Function to unbox primitive types from the Types to make it more readable for our consumers-}
printBasket :: Basket -> IO()
printBasket basket = do totalPrice <- return (priceVal (totalPrice basket))
                        print $ "TOTAL PRICE: " ++ (show totalPrice)
                        totalDiscount <- return (discountVal (totalDiscount basket))
                        print $ "TOTAL DISCOUNT: " ++ (show totalDiscount)
                        products <- return $ map (\product -> (productVal product)) (products basket)
                        print $ "PRODUCTS: " ++ (show products)

