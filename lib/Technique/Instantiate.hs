{-# LANGUAGE GADTs #-}

{-|
Given a Technique Procedure, transform it into an Instance that can be executed.
-}
module Technique.Evaluator where

import Core.Data
import Core.Text
import Data.UUID.Types (UUID)

import Technique.Language
import Technique.Quantity

-- Need names? Science names newly discovered creatures in Latin. I don't
-- speak Latin, but neither does anyone else so we can just make words up.
-- Yeay! (The lengths some people will go to in order to avoid qualified
-- imports is really impressive, isn't it?)
data Value
    = Unitus
    | Literali Rope
    | Quanticle Quantity
    | Tabularum [(Rope,Value)]
    | Parametri [Value]         -- TODO hmm

data Instance = Instance
    { instanceContext :: Context
    , instanceTitle :: Either Identifier Markdown
    , instanceSource :: Procedure -- necessary?
    , instanceRole :: Attribute
    , instanceSteps :: [Step]
    }

newtype Name = Name Rope -- ???

{-|
Names. Always needing names. These ones are from original work when we
envisioned technique as a shallow embedding of a domain specific language
implemented in Haskell. Comments describing constructors are taken from a
suggestion by Oleg Kiselyov on page 23 of his course "Typed Tagless Final
Interpreters" that the constructors of a simply typed lambda calculus in
this style could be considered a "minimal intuitionistic logic" which is
absolutely fabulous.
-}
data Step
    = Known Value                       -- axioms, aka literals
    | Blocking [Name]                   -- reference to a hypothesis denoted by a variable
    | Asynchronous Name Step            -- implication introduction, ie lambda, ie assignment?
    | Invocation Instance Step          -- implication elimination, ie function application

--
-- or maybe   | Depends [Step]
--
                                        -- assumption axiom?
                                        -- weakening?

{-|
In order to instantiate a Procedure into something that can in turn be
interpreted and thus run, we need to supply a Context: an identifier for
the event (collection of procedure calls) it is a part of, and the path
history we took to get here.
-}
data Context = Context
    { contextEvent :: UUID
    , contextPath :: Rope -- or a  list or a fingertree or...
    }

{-|
Take a static Procedure definition and spin it up into an Instance suitable
for interpretation. In other words, translate between the surface syntax types
and the abstract syntax we can feed to an evaluator.
-}
instantiate :: Context -> Procedure -> Instance
instantiate context procedure =
  let
    block = procedureBlock procedure
    statements = blockStatements block

    steps = foldr instantiateStatement [] statements
  in
    Instance
        { instanceContext = context
        , instanceProcedure = procedure
        }

