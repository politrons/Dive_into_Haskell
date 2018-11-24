{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module EventSourcingPattern where

import Data.Text.Lazy (Text,pack)

data Basket = Basket{products::[Text], totalPrice::Double, totalDiscount::Double} deriving (Show, Eq)

data Event =
   BasketCreated{ basket:: Basket}
  | ProductAdded{ product::String, price::Double}
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

{-| Command function apply the command and create event [DiscountAdded] to be rehydrate later on-}
addDiscountCommand :: Basket -> Double -> IO Event
addDiscountCommand basket discount = return $ DiscountAdded discount

{-|  EVENTS    -}
{-| ------------}
{-| Declaration function for all Events implementations-}
applyEvent :: Basket -> Event -> Basket

{-| Pure function to a create an empty basket-}
applyEvent basket (BasketCreated _basket) = basket

{-| Pure function to a add a product in basket-}
applyEvent basket (ProductAdded product price)= Basket (products basket ++ [pack product]) (totalPrice basket) (totalDiscount basket)

{-| Pure function to a add a discount in basket-}
applyEvent basket (DiscountAdded newDiscount) = Basket (products basket) (totalPrice basket) ((totalDiscount basket) + newDiscount )


{-|  REHYDRATE/PERSISTENCE. It could be whatever backend (cassandra, couchbase, mongodb)  -}
{-| -----------------------------------------------------------------------------}

{-| Function that return a new Events type appending the new event-}
appendEvent :: [Event] -> Event -> IO [Event]
appendEvent events event = return $ events ++ [event]


rehydrateByEvents :: Basket -> [Event] -> Basket
rehydrateByEvents basket (event:events) = rehydrateByEvents (applyEvent basket event) events
rehydrateByEvents basket [] = basket


{-|   Program  -}
{-| ------------}

persistEvents :: IO()
persistEvents = do print "############### PERSISTANCE COMMANDS #################"
                   event <- createBasketCommand
                   events <- appendEvent [] event
                   event <- addProductCommand (basket event) "Coca-cola" 10
                   events <- appendEvent events event
                   event <- addDiscountCommand (basket event) 2
                   events <- appendEvent events event
                   print $ show events
                   print "############### REHYDRATE EVENTS #################"
                   events <- return $ [(BasketCreated $ Basket [] 0 0), (ProductAdded "coca-cola" 2.50), (DiscountAdded 0.50)]
                   basket <- return $ rehydrateByEvents (Basket [] 0 0) events
                   print basket


