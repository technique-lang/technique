{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|

/Commentary/

We're optimizing for simplicity here. The language balances conventions
from other languages with choices to not overcomplicate things. Not
overloading operators, for example. Mostly we want to have "good error
messages" which is tough and subjective anyway. Not having multiline
anything, for example, might be a good choice, except that we also want to
be whitepsace insensitive.
-}
module Technique.Parser where

import Control.Monad
import Control.Monad.Combinators
import Core.Text.Rope
import Data.Foldable (foldl')
import Data.Int (Int8, Int64)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Technique.Language
import Technique.Quantity

type Parser = Parsec Void Text

__VERSION__ :: Int
__VERSION__ = 0

{-|
Skip /zero/ or more actual space characters. The __megaparsec__ function
@space@ etc consume all whitespace, not just ' '. That includes newlines,
which is very unhelpful.
-}
skipSpace :: Parser ()
skipSpace = void (hidden (many (char ' ')))

{-|
Skip at least /one/ actual space character.
-}
skipSpace1 :: Parser ()
skipSpace1 = void (hidden (some (char ' ')))

pMagicLine :: Parser Int
pMagicLine = do
    void (char '%') <?> "first line to begin with % character"
    void spaceChar <?> "a space character"
    void (string "technique")
    void spaceChar <?> "a space character"
    void (char 'v') <?> "the character v and then a number"
    v <- L.decimal <?> "the language version"
    void newline
    return v

pSpdxLine :: Parser (Rope,Maybe Rope)
pSpdxLine = do
    void (char '!') <?> "second line to begin with ! character"
    skipSpace

    -- I know we're supposed to use takeWhile1P in cases like this, but aren't
    -- we just duplicating the work of the parser combinators?
    license <- takeWhile1P
        (Just "software license description (ie an SPDX-Licence-Header value)")
        (\c -> not (c == ';' || c == '\n'))

    copyright <- optional $ do
        void (char ';') <?> "a semicolon"
        skipSpace
        void (char '©') <|> void (string "(c)")
        skipSpace
        takeWhile1P (Just "a copyright declaration") (/= '\n')
    void newline
    return (intoRope license,fmap intoRope copyright)

---------------------------------------------------------------------

pProcedureDeclaration :: Parser (Identifier,[Identifier],[Type],Type)
pProcedureDeclaration = do
    name <- pIdentifier
    skipSpace
    -- zero or more separated by comma
    params <- sepBy (pIdentifier <* skipSpace) (char ',' <* skipSpace)

    skipSpace
    void (char ':')
    skipSpace

    ins <- sepBy (pType <* skipSpace) (char ',' <* skipSpace)

    skipSpace
    void (string "->")
    skipSpace

    out <- pType
    return (name,params,ins,out)

identifierChar :: Parser Char
identifierChar = hidden (lowerChar <|> digitChar <|> char '_')


-- these do NOT consume trailing space. That's for pExpression to do.
pIdentifier :: Parser Identifier
pIdentifier = label "a valid identifier" $ do
    first <- lowerChar
    remainder <- many identifierChar
    return (Identifier (singletonRope first <> intoRope remainder))

typeChar :: Parser Char
typeChar = hidden (upperChar <|> lowerChar <|> digitChar)

pType :: Parser Type
pType = label "a valid type" $ do
    first <- upperChar
    remainder <- many typeChar
    return (Type (singletonRope first <> intoRope remainder))


---------------------------------------------------------------------

stringLiteral :: Parser Text
stringLiteral = label "a string literal" $ do
    void (char '\"')
    str <- many (do
        try (do
            void (char '\\')
            void (char '"')
            return '"')
        <|> try (do
            notFollowedBy (char '\"')
            printChar)
        )
    void (char '\"')
    return (T.pack str)

unitChar :: Parser Char
unitChar = hidden (upperChar <|> lowerChar <|> symbolChar)

unitLiteral :: Parser Rope
unitLiteral = label "a units symbol" $ do
    str <- some unitChar
    return (intoRope str)

-- FIXME change this to numbers with decimal points!
-- FIXME read? Really?
numberLiteral :: Parser Int64
numberLiteral = label "a number literal" $ do
    sign <- optional (char '-')
    digits <- some digitChar
    let number = read digits
    return (case sign of
        Just _ -> negate number
        Nothing -> number)

decimalLiteral :: Parser Decimal
decimalLiteral = label "a decimal literal" $ do
    digits1 <- some digitChar
    fraction <- optional (do
        void (char '.')
        some digitChar)

    return (case fraction of
        Nothing ->
          let
            number = read digits1
          in
            Decimal number 0
        Just digits2 ->
          let
            e = fromIntegral (length digits2)
            decimal = read digits1 * 10^e + read digits2
          in
            Decimal decimal e)

superscriptLiteral :: Parser Int8
superscriptLiteral = label "a superscript literal" $ do
    sign <- optional (char '⁻' <|> char '¯')    -- honestly not sure what the second of those is
    digits <- some (oneOf ['⁰','¹','²','³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'])
    let number = read (map toNumbers digits)
    return (case sign of
        Just _ -> negate number
        Nothing -> number)

toNumbers :: Char -> Char
toNumbers c = case c of
    '⁰' -> '0'
    '¹' -> '1'
    '²' -> '2'
    '³' -> '3'
    '⁴' -> '4'
    '⁵' -> '5'
    '⁶' -> '6'
    '⁷' -> '7'
    '⁸' -> '8'
    '⁹' -> '9'
    _   -> error "Invalid, superscript expected"

pQuantity :: Parser Quantity
pQuantity =
    try (do
        n <- pMantissa
        u <- pUncertainty
        m <- pMagnitude
        s <- pSymbol
        return (Quantity n u m s)) <|>
    try (do
        n <- pMantissa
        m <- pMagnitude
        s <- pSymbol
        return (Quantity n (Decimal 0 0) m s)) <|>
    try (do
        n <- pMantissa
        u <- pUncertainty
        s <- pSymbol
        return (Quantity n u 0 s)) <|>
    try (do
        n <- pMantissa
        s <- pSymbol
        return (Quantity n (Decimal 0 0) 0 s)) <|>
    try (do
        num <- numberLiteral
        return (Number num))
  where
    pMantissa = do
        decimalLiteral

    pUncertainty = do
        skipSpace1
        void (char '±') <|> void (string "+/-")
        skipSpace1
        decimalLiteral

    pMagnitude = do
        skipSpace1
        void (char '×') <|> void (char 'x') <|> void (char '*')
        skipSpace1
        void (string "10")
        number <- (do
            void (char '^')
            num <- numberLiteral
            pure (fromIntegral num))
            <|>
            superscriptLiteral
        return (fromIntegral number :: Int8)

    pSymbol = do
        skipSpace1
        unitLiteral


pOperator :: Parser Operator
pOperator =
    (char '&' *> return WaitBoth) <|>
    (char '|' *> return WaitEither) <|>
    (char '+' *> return Combine)

{-|
Parse a Tablet. This consumes trailing space around initial delimiter and
removes blank lines within the table (they're not syntactically meaningful)
but only cosnsumes a single newline after trailing delimeter, leaving
further consumption to pStatement.
-}
-- TODO this doesn't preserve alternate syntax if employed by user
pTablet :: Parser Tablet
pTablet = do
    void (char '[' <* space)

    bindings <- many
        (pBinding <* space)

    void (char ']' <* skipSpace <* optional newline <* skipSpace)

    return (Tablet bindings)
  where
    pBinding = do
        name <- stringLiteral
        skipSpace
        void (char '~')
        skipSpace
        subexpr <- pExpression

        -- handle alternate syntax here
        void (optional (char ','))
        return (Binding (intoRope name) subexpr)

pAttribute :: Parser Attribute
pAttribute =
    (do
        void (char '@')
        role <- pIdentifier <|> pAny
        return (Role role))
    <|>
    (do
        void (char '#')
        place <- pIdentifier <|> pAny
        return (Place place))
  where
    pAny = do
        void (char '*')
        return (Identifier (singletonRope '*'))

pExpression :: Parser Expression
pExpression = do
    expr1 <- pTerm
    skipSpace
    rest <- (optional (try pOperation2))
    skipSpace
    case rest of
        Just (oper,expr2)   -> return (Operation oper expr1 expr2)
        Nothing             -> return expr1
  where
    pTerm =
        try pNone <|>
        try pUndefined <|>
        try pRestriction <|>
        try pGrouping <|>
        try pObject <|>
        try pApplication <|>
        try pLiteral <|>
        try pVariable

    pNone = do
        void (string "()")
        return None

    pUndefined = do
        void (char '?')
        return Undefined

    pOperation2 = do                    -- 2 as in 2nd half
        operator <- pOperator
        skipSpace
        subexpr2 <- pExpression
        return (operator,subexpr2)

    pRestriction = do
        attr <- pAttribute
        space
        block <- pBlock
        return (Restriction attr block)

    pGrouping = do
        between (char '(' <* skipSpace) (char ')') $ do
            subexpr <- pExpression
            return (Grouping subexpr)

    pObject = do
        tablet <- pTablet
        return (Object tablet)

    pApplication = do
        name <- pIdentifier
        -- ie at least one space
        skipSpace1
        -- FIXME better do this manually, not all valid
        subexpr <- pExpression
        return (Application name subexpr)

    pLiteral =
        (do
            str <- stringLiteral
            return (Text (intoRope str))) <|>
        (do
            qty <- pQuantity
            return (Amount qty))

    pVariable = do
        name <- pIdentifier
        return (Variable name)

pStatement :: Parser Statement
pStatement =
    try pAssignment <|>
    try pDeclaration <|>
    try pExecute <|>
    try pBlank <|>
    try pSeries
  where
    pAssignment = label "an assignment" $ do
        name <- pIdentifier
        skipSpace
        void (char '=')
        skipSpace
        expr <- pExpression
        return (Assignment name expr)

    pDeclaration = label "a declaration" $ do
        -- only dive into working out if this is a Procedure if there's a ':' here
        proc <- pProcedureCode
        return (Declaration proc)

    pExecute = label "a value to execute" $ do
        expr <- pExpression
        return (Execute expr)

    pBlank = hidden $ do -- label "a blank line"
        void newline
        return Blank

    pSeries = do
        void (char ';')
        skipSpace
        return Series

---------------------------------------------------------------------

pBlock :: Parser Block
pBlock = do
    -- open block, absorb whitespace
    void (char '{' <* hidden space)

    -- process statements, but only single newline at a time
    statements <- many
         (pStatement <* skipSpace <* optional newline <* skipSpace)

    -- close block, and wipe out any trailing whitespace
    void (char '}' <* skipSpace <* optional newline)

    return (Block statements)

-- Frankly, this parser looks ridiculous. Someone who knows what they are
-- doing *please* help refactor this. It seems unavoidlable to run the
-- pProcedureDeclaration parser twice, unless we can combine the successful
-- parse and the consumtion of description lines into one function. Maybe
-- this would be better done scanning ahead to count characters until a
-- declaration shows up, then explicitly taking that many?

fourSpaces :: Parser ()
fourSpaces = -- label "a code block indented by four spaces" $
    void (char ' ' <* char ' ' <* char ' ' <* char ' ') <|>
    fail "code blocks must be indented by four spaces"

pMarkdown :: Parser Markdown
pMarkdown = do
    -- gobble blank newlines before a heading
    void (many (do
        notFollowedBy fourSpaces
        notFollowedBy pProcedureDeclaration
        void (skipSpace *> hidden newline)))

    -- TODO heading

    results <- some (do
        notFollowedBy fourSpaces
        notFollowedBy pProcedureDeclaration
        line <- takeWhileP (Just "another line of description text") (/= '\n')
        void (hidden newline)
        return line)

    let description = foldl' (\acc text -> appendRope text acc <> "\n") emptyRope results
    return (Markdown description)

pProcedureCode :: Parser Procedure
pProcedureCode = do
    (name,params,ins,out) <- pProcedureDeclaration <* skipSpace <* optional newline <* skipSpace

    block <- pBlock <* skipSpace <* optional newline

    return (Procedure
        { procedureName = name
        , procedureParams = params
        , procedureInput = ins
        , procedureOutput = out
        , procedureLabel = Nothing          -- FIXME
        , procedureDescription = Nothing
        , procedureBlock = block
        })

pProcedure :: Parser Procedure
pProcedure = do
    description <- optional pMarkdown
    fourSpaces
    proc <- pProcedureCode

    return (proc
        { procedureLabel = Nothing          -- FIXME
        , procedureDescription = description
        })

---------------------------------------------------------------------

pTechnique :: Parser Technique
pTechnique = do
    version <- pMagicLine
    unless (version == __VERSION__) (fail ("currently the only recognized language version is v" ++ show __VERSION__))
    (license,copyright) <- pSpdxLine
    void (many newline)

    body <- many pProcedure

    return $ Technique
        { techniqueVersion = version
        , techniqueLicense = intoRope license
        , techniqueCopyright = fmap intoRope copyright
        , techniqueBody = body
        }
