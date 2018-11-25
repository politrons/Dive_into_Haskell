{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module EventSourcingPattern where

import Data.Text.Lazy (Text,pack)

type Product = String
type Price = Double

{-| To define this example we will use the typical grocery basket to be rehydrate -}
data Basket = Basket{products::[Product], totalPrice::Double, totalDiscount::Double} deriving (Show, Eq)

{-| Data type to define all possible events-}
data Event =
   BasketCreated{ basket:: Basket}
  | ProductAdded{ product::String, price::Double}
  | ProductRemoved{ product::String, price::Double}
  | DiscountAdded{ discount::Double }
  deriving (Show, Eq)


{-|  COMMANDS  -}
{-| ------------}
{-| Command function apply the command and create event [BasketCreated] to be rehydrate later on-}
createBasketCommand :: IO Event
createBasketCommand = return $ BasketCreated $ Basket [] 0 0

{-| Command function apply the command and create event [ProductAdded] to be rehydrate later on-}
addProductCommand :: Basket -> String -> Double -> IO Event
addProductCommand basket product price = return $ ProductAdded product price

{-| Command function apply the command and create event [ProductRemoved] to be rehydrate later on-}
removeProductCommand :: Basket -> String -> Double -> IO Event
removeProductCommand basket product price = return $ ProductRemoved product price

{-| Command function apply the command and create event [DiscountAdded] to be rehydrate later on-}
addDiscountCommand :: Basket -> Double -> IO Event
addDiscountCommand basket discount = return $ DiscountAdded discount

{-|  EVENTS    -}
{-| ------------}
{-| Declarative function for all Events implementations-}
applyEvent :: Basket -> Event -> Basket

{-| Pure function to create an empty basket-}
applyEvent basket (BasketCreated _basket) = basket

{-| Pure function to create a new Basket adding a product in basket-}
applyEvent basket (ProductAdded product price)= Basket (products basket ++ [product]) (totalPrice basket + price) (totalDiscount basket)

{-| Pure function to create a new Basket adding a discount in basket-}
applyEvent basket (DiscountAdded newDiscount) = Basket (products basket) (totalPrice basket - newDiscount) ((totalDiscount basket) + newDiscount )

{-| Pure function to create a new Basket removing the product and price from previous basket-}
applyEvent basket (ProductRemoved product price) = Basket (removeProductFromBasket product (products basket)) (reducePrice basket price) (totalDiscount basket)

{-| Utils  -}
{-| --------}
{-| Function to filter from the list of products the one we want to remove-}
removeProductFromBasket :: Product -> [Product] -> [Product]
removeProductFromBasket productToRemove products = filter (\product -> product /= productToRemove) products

reducePrice :: Basket -> Price -> Price
reducePrice basket price = totalPrice basket - price

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


{-|   Program  -}
{-| ------------}

persistEvents :: IO()
persistEvents = do print "############### PERSISTANCE COMMANDS #################"
                   event <- createBasketCommand
                   events <- appendEvent [] event
                   event <- addProductCommand (basket event) "Coca-cola" 2.50
                   events <- appendEvent events event
                   event <- addProductCommand (basket event) "Buddbeiser" 3.00
                   events <- appendEvent events event
                   event <- addDiscountCommand (basket event) 0.50
                   events <- appendEvent events event
                   event <- addProductCommand (basket event) "Nachos" 1.20
                   events <- appendEvent events event
                   event <- addDiscountCommand (basket event) 0.20
                   events <- appendEvent events event
                   event <- addProductCommand (basket event) "Pepsi" 2.40
                   events <- appendEvent events event
                   event <- removeProductCommand (basket event) "Coca-cola" 2.50
                   events <- appendEvent events event
                   print $ show events
                   print "############### REHYDRATE EVENTS #################"
                   basket <- return $ rehydrateByEvents (Basket [] 0 0) events
                   print basket


