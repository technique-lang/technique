{-# LANGUAGE OverloadedStrings #-}

module Technique.Quantity
(
      Quantity(..)
    , Symbol
    , Unit(..)
    , Group(..)
    , Scales(..)
    , units     
) where

import Core.Text.Rope
import Core.Data.Structures

data Quantity
    = Number Int                -- FIXME not Int
    | Quantity Int Unit


type Symbol = Rope

units :: Map Symbol Unit
units =
    foldr f emptyMap knownUnits
  where
    f unit m = insertKeyValue (unitSymbol unit) unit m

{-|
Whether Système International metric prefixes can be used, or (as is the case 
of time units) quantities should not be aggregated to other scales.
-}
data Group
    = Metric
    | Time
    | Scientific        -- probable collision with type from **base**
    | Engineering

knownUnits :: [Unit]
knownUnits =
    [ Unit "metre" "metres" "m" Metric
    , Unit "gram" "grams" "g" Metric
    , Unit "litre" "litres" "L" Metric
    , Unit "second" "seconds" "s" Time
    , Unit "hour" "hours" "h" Time
    , Unit "day" "days" "d" Time
    ]

data Scales
    = Peta
    | Tera
    | Giga
    | Mega
    | Kilo
    | Zero
    | Milli
    | Micro
    | Nano
    | Pico

data Unit = Unit
    { unitName :: Rope
    , unitPlural :: Rope
    , unitSymbol :: Rope
    , unitGroup :: Group
    }
