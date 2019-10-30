{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Technique.Quantity
(
      Quantity(..)
    , Decimal(..)
    , Magnitude
    , decimalToRope
    , isZero
    , Symbol
    , Unit(..)
    , Group(..)
    , Prefix(..)
    , units
    , prefixes
) where

import Core.Text.Rope
import Core.Text.Utilities
import Core.Data.Structures
import Data.Int (Int8, Int64)

data Quantity
    = Number Int64
    | Quantity Decimal Decimal Magnitude Symbol
    deriving (Show, Eq)

type Symbol = Rope

type Magnitude = Int8

{-|
A decimal number with a fixed point resolution. The resolution (number of
decimal places) is arbitrary within the available range. This isn't really
for numerical analysis. It is for carrying information.

/Implementation note/

Internally this is a floating point where the mantissa is 19 characters
wide (the width of a 64-bit int in base 10). Thus the biggest number
representable is 9223372036854775807 and the smallest is
0.0000000000000000001. We could change this to Integer and be arbitrary
precision but meh.
-}
data Decimal = Decimal Int64 Int8
    deriving Eq

instance Show Decimal where
    show = show . decimalToRope

decimalToRope :: Decimal -> Rope
decimalToRope (Decimal number resolution)
    | resolution < 0  = error "resolution can't be negative"
    | resolution == 0 = intoRope (show number)
    | otherwise =
        let
            digits = intoRope (show (abs number))
            len = widthRope digits
            res = fromIntegral resolution
            pos = len - res
            result = if (pos <= 0)
                then "0." <> leftPadWith '0' res digits
                else let (whole,fraction) = splitRope pos digits in whole <> "." <> fraction
        in
            if number >= 0
                then result
                else "-" <> result

isZero :: Decimal -> Bool
isZero (Decimal number _) = if number == 0 then True else False

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
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Unit = Unit
    { unitName :: Rope
    , unitPlural :: Rope
    , unitSymbol :: Rope
    , unitGroup :: Group
    }
    deriving (Show, Eq)