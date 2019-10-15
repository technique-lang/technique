{-# LANGUAGE GADTs #-}

module Technique.Evaluator where

-- TODO *not* Maybe! If we have two option types here then there are four
-- possibilities we need to cater for [(Nothing,Nothing), ... (Just,Just)]
-- which means there are four semantics for a given table row. Hm.

-- TODO there's an ambiguity here between the type of a table and the type
-- of an individual quantity. Which is "type"?

{-
data Value = Value
    { valueType :: Type 
    , valueLabel :: Maybe Label
    , valueData :: Maybe Quantity
    }

data Type = Type
     { typeName :: Rope
--   , typeInternal :: t
     }
-}


{-
data Expression b where
    Binding :: Variable b -> Expression a -> Expression b
    Comment :: Rope -> Expression ()
    Declaration :: (a -> b) -> Expression (a -> b)
    Application :: Expression (a -> b) -> Expression a -> Expression b 
    Attribute :: Role -> Expression a -> Expression a
-}


{-
-- aka apply
execute :: Procedure -> Value -> b
execute proc value =
  let
    body = procedureBlock proc
  in
    evaluate body

type Context = Map String Value -- ?


evaluate = undefined

evaluate :: Context -> Expression -> Value
evaluate e = case e of
    Comment _ -> ()
    _ -> undefined
-}
