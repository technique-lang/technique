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
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Technique.Language
import Technique.Quantity

type Parser = Parsec Void Text

__VERSION__ :: Int
__VERSION__ = 0

consumer :: (MonadParsec e s m, Token s ~ Char) => m ()
consumer = L.space space1 empty empty

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = L.lexeme consumer

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

pSpdxLine :: Parser (Text,Maybe Text)
pSpdxLine = do
    void (char '!') <?> "second line to begin with ! character"
    void spaceChar <?> "a space character"

    license <- takeWhile1P (Just "software license description (ie an SPDX-Licence-Header value)") (\c -> not (c == ',' || c == '\n'))

    copyright <- optional $ do
        void (char ',') <?> "a comma"
        hidden $ skipMany (spaceChar <?> "a space character")
        void (char '©') <|> void (string "(c)")
        void (spaceChar <?> "a space character")
        takeWhile1P (Just "a copyright declaration") (/= '\n')

    void newline
    return (license,copyright)

pProcfileHeader :: Parser Technique
pProcfileHeader = do
    version <- pMagicLine
    unless (version == __VERSION__) (fail ("currently the only recognized language version is v" ++ show __VERSION__))
    (license,copyright) <- pSpdxLine

    return $ Technique
        { techniqueVersion = version
        , techniqueLicense = intoRope license
        , techniqueCopyright = fmap intoRope copyright
        , techniqueBody = []
        }

---------------------------------------------------------------------

-- FIXME consider making this top down, not LR
-- FIXME need to do lexeme to gobble optional whitespace instead of space1

pProcedureDeclaration :: Parser (Identifier,[Identifier],[Type],Type)
pProcedureDeclaration = do
    name <- pIdentifier
    void (many space1)
    -- zero or more separated by comma
    params <- sepBy pIdentifier (char ',')

    void (many space1)
    void (char ':')
    void (many space1)

    ins <- sepBy pType (char ',')

    void (many space1)
    void (string "->")
    void (many space1)

    out <- pType
    return (name,params,ins,out)

identifierChar :: Parser Char
identifierChar = lowerChar <|> digitChar <|> char '_'

pIdentifier :: Parser Identifier
pIdentifier = do
    first <- lowerChar
    remainder <- many identifierChar
    return (Identifier (singletonRope first <> intoRope remainder))

typeChar :: Parser Char
typeChar = upperChar <|> lowerChar <|> digitChar

pType :: Parser Type
pType = do
    first <- upperChar
    remainder <- many typeChar
    return (Type (singletonRope first <> intoRope remainder))


---------------------------------------------------------------------

-- TODO What is special about L.charLiteral vs just using normal parsers?

stringLiteral0 :: Parser Text
stringLiteral0 = do
    void (char '\"')
    str <- takeWhileP Nothing (/= '"')
    notFollowedBy eol
    void (char '\"')
    return str

stringLiteral :: Parser Text
stringLiteral = do
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

pQuantity :: Parser Quantity
pQuantity = do
    try (do
        str <- stringLiteral
        return (Text (intoRope str)))
    <|> try (do
        num <- numberLiteral
        return (Number num))

pExpression :: Parser Expression
pExpression = do
    try (do
        between (char '(') (char ')') $ do
            subexpr <- pExpression
            return (Grouping subexpr))
    <|> try (do
        name <- pIdentifier
        -- ie at least one space
        void (some space1)
        -- FIXME better do this manually, not all valid
        subexpr <- pExpression
        return (Application name subexpr))
    <|> try (do
        qty <- pQuantity
        return (Literal qty))
    <|> try (do
        name <- pIdentifier
        return (Variable name))


pStatement :: Parser Statement
pStatement = do
    try (do
        name <- pIdentifier
        void (many space1)
        void (char '=')
        void (many space1)
        expr <- pExpression
        void newline
        return (Assignment name expr))
    <|> try (do
        -- only dive into working out if this is a Procedure if there's a ':' here
        void (lookAhead (takeWhileP Nothing (/= ':') *> char ':' <* eol))
        proc <- pProcedureFunction
        void newline
        return (Declaration proc))
    <|> try (do
        expr <- pExpression
        void newline
        return (Execute expr))
    <|> try (do
        void newline
        return Blank)


-- FIXME documentation says `between (symbol "{") (symbol "}")` which implies lexing yeah?
pBlock :: Parser Block
pBlock = do
    statements <- between (char '{' <* many space1) (many space1 *> char '}') (many pStatement)
    return (Block statements)

pProcedureFunction :: Parser Procedure
pProcedureFunction = do
    (name,params,ins,out) <- pProcedureDeclaration

    fail (show (name,params,ins,out))
