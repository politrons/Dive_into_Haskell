module MaybeFunctions where

import Data.Char
import Data.List

-- | Maybe is just like Optional or Option from Scala and Java.
-- With Maybe we define the type [Maybe] follow by the type [T] that it will contain.

-- | In case we want to fill a Maybe with data we just need to add [Just] as Some in Scala just before the data "Hello effect"
someValue = Just "Hello effect" :: Maybe String

-- | In case we want to create an empty Maybe we just need to add [Nothing] as None in Scala
emptyValue = Nothing :: Maybe String

-- | The pure way to extract the value from a Maybe is using patter matching,
--   where we can just create the case for [Just value] or [Nothing], as we normally would do from Scala.

valueOfMaybeFilled = case someValue of
     Just value -> value
     Nothing -> "Default value"

valueOfMaybeNothing = case emptyValue of
     Just value -> value
     Nothing -> "Default value"

-- | In this next example we use the operator [find] which it return a [Mayvbe] type depending if is able or not
--   to find an element in the list.

people = ["Paul","Peter", "John", "Sussan"] :: [String]

outputFindJust = find (isGuy "John") people where isGuy name = \person -> person == name

outputFindNoting = find (isGuy "Heisemberg") people where isGuy name = \person -> person == name