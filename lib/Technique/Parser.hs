{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- /Commentary/
--
-- We're optimizing for simplicity here. The language balances conventions
-- from other languages with choices to not overcomplicate things. Not
-- overloading operators, for example. Mostly we want to have "good error
-- messages" which is tough and subjective anyway. Not having multiline
-- anything, for example, might be a good choice, except that we also want to
-- be whitepsace insensitive.
module Technique.Parser
  ( -- parser for technique procedure files.
    pTechnique,
    -- everthing else is only exposed for testing purposes.
    pMagicLine,
    pSpdxLine,
    pIdentifier,
    pType,
    stringLiteral,
    numberLiteral,
    pQuantity,
    pAttribute,
    pExpression,
    pStatement,
    pBlock,
    pProcedureDeclaration,
    pProcedureCode,
  )
where

import Control.Monad
  ( unless,
    void,
  )
import Control.Monad.Combinators
  ( (<|>),
    many,
    optional,
    sepBy,
    sepBy1,
    some,
  )
import Core.Text.Rope
  ( Rope,
    appendRope,
    emptyRope,
    intoRope,
    singletonRope,
  )
import Data.Foldable
  ( foldl',
  )
import Data.Int
  ( Int64,
    Int8,
  )
import Data.Text
  ( Text,
  )
import qualified Data.Text as T (pack)
import Data.Void
  ( Void,
  )
import Technique.Language
import Technique.Quantity
import Text.Megaparsec
  ( (<?>),
    Parsec,
    getOffset,
    hidden,
    label,
    lookAhead,
    notFollowedBy,
    oneOf,
    skipMany,
    takeWhile1P,
    takeWhileP,
    try,
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    lowerChar,
    newline,
    printChar,
    space,
    spaceChar,
    string,
    upperChar,
  )
import Text.Read
  ( readMaybe,
  )

type Parser = Parsec Void Text

__VERSION__ :: Int
__VERSION__ = 0

-- |
-- Skip /zero/ or more actual space characters. The __megaparsec__ function
-- @space@ etc consume all whitespace, not just ' '. That includes newlines,
-- which is very unhelpful.
skipSpace :: Parser ()
skipSpace = void (hidden (many (char ' ' <|> char '\t')))

-- |
-- Skip at least /one/ actual space character.
skipSpace1 :: Parser ()
skipSpace1 = void (hidden (some (char ' ' <|> char '\t')))

digitChar0 :: Parser Char
digitChar0 = label "a digit" $ digitChar

pMagicLine :: Parser Int
pMagicLine = do
  void (char '%') <?> "first line to begin with % character"
  void spaceChar <?> "a space character"
  void (string "technique")
  void spaceChar <?> "a space character"
  void (char 'v') <?> "the character 'v' and then a number"
  v <- numberLiteral <?> "the language version"
  void newline
  return (fromIntegral v)

pSpdxLine :: Parser (Rope, Maybe Rope)
pSpdxLine = do
  void (char '!') <?> "second line to begin with ! character"
  skipSpace

  -- I know we're supposed to use takeWhile1P in cases like this, but aren't
  -- we just duplicating the work of the parser combinators?
  license <-
    takeWhile1P
      (Just "software license description (ie an SPDX-Licence-Header value)")
      (\c -> not (c == ';' || c == '\n'))

  copyright <- optional $ do
    void (char ';') <?> "a semicolon"
    skipSpace
    void (char '©') <|> void (string "(c)")
    skipSpace
    takeWhile1P (Just "a copyright declaration") (/= '\n')
  void newline
  return (intoRope license, fmap intoRope copyright)

---------------------------------------------------------------------

pProcedureDeclaration :: Parser (Identifier, [Identifier], [Type], [Type])
pProcedureDeclaration = do
  name <- pIdentifier
  skipSpace
  -- zero or more separated by comma
  params <- pIdentifiers

  skipSpace
  void (char ':')
  skipSpace

  ins <- pTypes1

  skipSpace
  void (string "->")
  skipSpace

  out <- pTypes1
  return (name, params, ins, out)

identifierChar :: Parser Char
identifierChar = hidden (lowerChar <|> digitChar0 <|> char '_' <|> char '\'')

-- these do NOT consume trailing space. That's for pExpression to do.
pIdentifier :: Parser Identifier
pIdentifier = label "a valid identifier" $ do
  first <- lowerChar
  remainder <- many identifierChar
  return (Identifier (singletonRope first <> intoRope remainder))

pIdentifiers :: Parser [Identifier]
pIdentifiers = sepBy (pIdentifier <* skipSpace) (char ',' <* skipSpace)

pIdentifiers1 :: Parser [Identifier]
pIdentifiers1 = sepBy1 (pIdentifier <* skipSpace) (char ',' <* skipSpace)

typeChar :: Parser Char
typeChar = hidden (upperChar <|> lowerChar <|> digitChar0)

pType :: Parser Type
pType =
  label "a valid type" $
    try
      ( do
          void (string "()")
          return (Type "()")
      )
      <|> ( do
              first <- upperChar
              remainder <- many typeChar
              return (Type (singletonRope first <> intoRope remainder))
          )

pTypes1 :: Parser [Type]
pTypes1 = sepBy1 (pType <* skipSpace) (char ',' <* skipSpace)

---------------------------------------------------------------------

stringLiteral :: Parser Text
stringLiteral = label "a string literal" $ do
  void (char '\"')
  str <-
    many
      ( do
          try
            ( do
                void (char '\\')
                void (char '"')
                return '"'
            )
          <|> ( do
                  notFollowedBy (char '\"')
                  printChar
              )
      )
  void (char '\"')
  return (T.pack str)

unitChar :: Parser Char
unitChar = hidden (upperChar <|> lowerChar <|> char '°')

unitLiteral :: Parser Rope
unitLiteral = label "a units symbol" $ do
  str <- some unitChar
  return (intoRope str)

numberLiteral :: Parser Int64
numberLiteral = label "a number literal" $ do
  digits <- some digitChar0
  let result = readMaybe digits
  case result of
    Just number -> return number
    Nothing -> fail "expected a number but couldn't parse"

decimalLiteral :: Parser Decimal
decimalLiteral = label "a decimal literal" $ do
  digits1 <- some digitChar0
  fraction <-
    optional
      ( do
          void (char '.')
          some digitChar0
      )

  return
    ( case fraction of
        Nothing ->
          let number = read digits1
           in Decimal number 0
        Just digits2 ->
          let e = fromIntegral (length digits2)
              decimal = read digits1 * 10 ^ e + read digits2
           in Decimal decimal e
    )

superscriptLiteral :: Parser Int8
superscriptLiteral = label "a superscript literal" $ do
  sign <- optional (char '⁻' <|> char '¯') -- honestly not sure what the second of those is
  digits <- some (oneOf ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'])
  let number = read (map toNumbers digits)
  return
    ( case sign of
        Just _ -> negate number
        Nothing -> number
    )

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
  _ -> error "Invalid, superscript expected"

pQuantity :: Parser Quantity
pQuantity =
  ( do
      -- look ahead far enough to commit to this branch:  the pieces of a
      -- decimal, a space, and then one of the characters that starts an
      -- uncertainty, magnitude, or symbol.
      lookAhead
        ( try
            ( do
                skipMany (digitChar0 <|> char '.' <|> char '-' <|> char ' ')
                void (char '±' <|> char '+' <|> char '×' <|> char 'x' <|> unitChar)
            )
        )

      n <- pMantissa
      u <- pUncertainty <|> pure (Decimal 0 0)
      m <- pMagnitude <|> pure 0
      s <- pSymbol
      return (Quantity n u m s)
  )
    <|> ( do
            n <- pNumber
            return (Number n)
        )
  where
    pNumber = do
      sign <- optional (char '-')
      number <- numberLiteral
      return
        ( case sign of
            Just _ -> negate number
            Nothing -> number
        )
    pMantissa = do
      sign <- optional (char '-')
      decimal <- try decimalLiteral
      skipSpace
      return
        ( case sign of
            Just _ -> negateDecimal decimal
            Nothing -> decimal
        )
    pUncertainty = do
      void (char '±') <|> void (string "+/-")
      skipSpace
      decimal <- decimalLiteral
      skipSpace
      return decimal
    pMagnitude = do
      void (char '×') <|> void (char 'x') <|> hidden (void (char '*'))
      skipSpace
      void (string "10")
      number <-
        ( do
            void (char '^')
            sign <- optional (char '-')
            e <- numberLiteral
            pure
              ( fromIntegral
                  ( case sign of
                      Just _ -> negate e
                      Nothing -> e
                  )
              )
            <|> superscriptLiteral
          )
      skipSpace
      return number
    pSymbol = do
      symbol <- unitLiteral
      skipSpace
      return symbol

pOperator :: Parser Operator
pOperator =
  (char '&' *> return WaitBoth)
    <|> (char '|' *> return WaitEither)
    <|> (char '+' *> return Combine)

-- |
-- Parse a Tablet. This consumes trailing space around initial delimiter and
-- removes blank lines within the table (they're not syntactically meaningful)
-- but only cosnsumes a single newline after trailing delimeter, leaving
-- further consumption to pStatement.
--
-- TODO this doesn't preserve alternate syntax if employed by user
pTablet :: Parser Tablet
pTablet = do
  void (char '[' <* hidden space)

  bindings <-
    many
      (pBinding <* hidden space)

  void (char ']' <* skipSpace)

  return (Tablet bindings)
  where
    pBinding = do
      name <- stringLiteral
      skipSpace
      void (char '~')
      skipSpace
      subexpr <- pExpression

      -- handle alternate syntax here
      {-
              -- FIXME this is not working
              void (optional (char ','))
      -}
      return (Binding (Label (intoRope name)) subexpr)

pAttribute :: Parser Attribute
pAttribute =
  ( do
      void (char '@')
      role <- pIdentifier <|> pAny
      return (Role role)
  )
    <|> ( do
            void (char '#')
            place <- pIdentifier <|> pAny
            return (Place place)
        )
  where
    pAny = do
      void (char '*')
      return (Identifier (singletonRope '*'))

pExpression :: Parser Expression
pExpression = do
  o <- getOffset
  expr1 <- pTerm o
  skipSpace
  rest <- (optional (try pOperation2))
  skipSpace
  case rest of
    Just (oper, expr2) -> return (Operation o oper expr1 expr2)
    Nothing -> return expr1
  where
    pTerm o =
      pNone o
        <|> pUndefined o
        <|> pRestriction o
        <|> pGrouping o
        <|> pObject o
        <|> pApplication o
        <|> pLiteral o
        <|> pVariable o
    pNone :: Offset -> Parser Expression
    pNone o = do
      void (string "()")
      return (None o)
    pUndefined o = do
      void (char '?')
      return (Undefined o)
    pOperation2 = do
      -- 2 as in 2nd half
      operator <- pOperator
      skipSpace
      subexpr2 <- pExpression
      return (operator, subexpr2)
    pRestriction o = do
      attr <- pAttribute
      hidden space
      block <- pBlock
      return (Restriction o attr block)
    pGrouping o = do
      void (char '(')
      skipSpace

      subexpr <- pExpression

      void (char ')')
      skipSpace

      return (Grouping o subexpr)
    pObject o = do
      tablet <- pTablet
      return (Object o tablet)
    pApplication o = do
      lookAhead
        ( try
            ( do
                skipMany identifierChar
                skipSpace1
                void (identifierChar <|> digitChar0 <|> char '(' <|> char '\"')
            )
        )

      name <- pIdentifier
      -- ie at least one space
      skipSpace1
      -- FIXME better do this manually, not all valid
      subexpr <- pExpression
      return (Application o name subexpr)
    pLiteral o =
      ( do
          str <- stringLiteral
          return (Text o (intoRope str))
      )
        <|> ( do
                qty <- pQuantity
                return (Amount o qty)
            )
    pVariable o = do
      names <- pIdentifiers1

      return (Variable o names)

pStatement :: Parser Statement
pStatement = do
  o <- getOffset
  statement <-
    pAssignment o
      <|> pDeclaration o
      <|> pExecute o
      <|> pBlank o
      <|> pSeries o
  return statement
  where
    pAssignment o = label "an assignment" $ do
      lookAhead
        ( try
            ( do
                skipMany (identifierChar <|> char ',' <|> char ' ')
                void (char '=')
            )
        )
      names <- pIdentifiers1
      skipSpace
      void (char '=')
      hidden space
      expr <- pExpression
      return (Assignment o names expr)
    pDeclaration o = label "a declaration" $ do
      lookAhead
        ( try
            ( do
                skipMany (identifierChar <|> char ',' <|> char ' ')
                void (char ':')
            )
        )

      proc <- pProcedureCode
      return (Declaration o proc)
    pExecute o = label "a value to execute" $ do
      expr <- pExpression
      return (Execute o expr)
    pBlank o = hidden $ do
      -- label "a blank line"
      void newline
      return (Blank o)
    pSeries o = do
      void (char ';')
      return (Series o)

---------------------------------------------------------------------

pBlock :: Parser Block
pBlock = do
  -- open block, absorb whitespace
  void (char '{' <* hidden space)

  -- process statements, but only single newline at a time
  statements <-
    many
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
fourSpaces =
  -- label "a code block indented by four spaces" $
  void (char ' ' <* char ' ' <* char ' ' <* char ' ')
    <|> fail "code blocks must be indented by four spaces"

pMarkdown :: Parser Markdown
pMarkdown = do
  -- gobble blank newlines before a heading
  void
    ( many
        ( do
            notFollowedBy fourSpaces
            notFollowedBy pProcedureDeclaration
            void (skipSpace *> hidden newline)
        )
    )

  -- TODO heading

  results <-
    some
      ( do
          notFollowedBy fourSpaces
          notFollowedBy pProcedureDeclaration
          line <- takeWhileP (Just "another line of description text") (/= '\n')
          void (hidden newline)
          return line
      )

  let description = foldl' (\acc text -> appendRope text acc <> "\n") emptyRope results
  return (Markdown description)

pProcedureCode :: Parser Procedure
pProcedureCode = do
  o <- getOffset
  (name, params, ins, out) <- pProcedureDeclaration <* skipSpace <* optional newline <* skipSpace

  block <- pBlock <* skipSpace <* optional newline

  return
    ( Procedure
        { procedureOffset = o,
          procedureName = name,
          procedureParams = params,
          procedureInput = ins,
          procedureOutput = out,
          procedureTitle = Nothing, -- FIXME
          procedureDescription = Nothing,
          procedureBlock = block
        }
    )

pProcedure :: Parser Procedure
pProcedure = do
  description <- optional pMarkdown
  fourSpaces
  proc <- pProcedureCode

  return
    ( proc
        { procedureTitle = Nothing, -- FIXME
          procedureDescription = description
        }
    )

---------------------------------------------------------------------

pTechnique :: Parser Technique
pTechnique = do
  version <- pMagicLine
  unless (version == __VERSION__) (fail ("currently the only recognized language version is v" ++ show __VERSION__))
  (license, copyright) <- pSpdxLine
  void (many newline)

  body <- many pProcedure

  return $
    Technique
      { techniqueVersion = version,
        techniqueLicense = intoRope license,
        techniqueCopyright = fmap intoRope copyright,
        techniqueBody = body
      }
