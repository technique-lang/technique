{-# LANGUAGE OverloadedStrings #-}

module Technique.Quantity
(
      Quantity(..)
    , Symbol
    , Unit(..)
    , Group(..)
    , Prefix(..)
    , units
    , prefixes
) where

import Core.Text.Rope
import Core.Data.Structures

data Quantity
    = None
    | Number Int                -- FIXME not Int
    | Quantity Int Unit
    | Text Rope

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
    = Metric            -- has prefixes
    | Time
    | Normal
    | Scientific        -- probable collision with type from **base**
    | Engineering

knownUnits :: [Unit]
knownUnits =
    [ Unit "metre" "metres" "m" Metric
    , Unit "gram" "grams" "g" Metric
    , Unit "litre" "litres" "L" Metric
    , Unit "second" "seconds" "sec" Time
    , Unit "minute" "minutes" "min" Time
    , Unit "hour" "hours" "hr" Time
    , Unit "day" "days" "d" Time
    , Unit "degree celsius" "degrees celsius" "°C" Normal
    , Unit "degree kelvin" "degrees kelvin" "K" Metric
    ]

prefixes :: Map Symbol Prefix
prefixes =
    foldr g emptyMap knownPrefixes
  where
    g prefix m = insertKeyValue (prefixSymbol prefix) prefix m

knownPrefixes :: [Prefix]
knownPrefixes =
    [ Prefix "peta" "P" 15
    , Prefix "tera" "T" 12
    , Prefix "giga" "G" 9
    , Prefix "mega" "M" 6
    , Prefix "kilo" "k" 3
    , Prefix "" "" 0
    , Prefix "milli" "m" (-3)
    , Prefix "micro" "μ" (-6)
    , Prefix "nano" "n" (-9)
    , Prefix "pico" "p" (-12)
    ]

data Prefix = Prefix
    { prefixName :: Rope
    , prefixSymbol :: Symbol
    , prefixScale :: Int        -- FIXME change this to a hard coded numerical constant?
    }

data Unit = Unit
    { unitName :: Rope
    , unitPlural :: Rope
    , unitSymbol :: Rope
    , unitGroup :: Group
    }
