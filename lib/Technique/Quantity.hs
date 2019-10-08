module Technique.Quantity where

data Quantity
    = Number Int                -- FIXME not Int
    | Quantity Int Units


-- this is Not Very Scalable to say the least. Units need to not be Haskell
-- host language sum type. They need to be declared inside the objective
-- langauage.
data Units
    = Grams
    | Metres
    | Seconds
    | Hours
    | Days
    | Teaspoon
    | Tablespoon
    | Litres

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

data Units = Units
    { unitsName :: Rope
    , unitsPlural :: Rope
    , unitsSymbol :: Rope
    }
