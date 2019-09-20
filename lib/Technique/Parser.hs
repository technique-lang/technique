module Technique.Parser where

import Control.Monad
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

__VERSION__ :: Int
__VERSION__ = 0

parseMagicLine :: Parser Int
parseMagicLine = do
    void (char '%') <?> "first line to begin with % character"
    void spaceChar <?> "a space character"
    void (string "technique")
    void spaceChar <?> "a space character"
    void (char 'v') <?> "the character v and then a number"
    v <- L.decimal <?> "the language version"
    unless (v == __VERSION__) (fail ("currently the only recognized language version is v" ++ show __VERSION__))
    void newline
    return v

type AbstractSyntaxTree = ()    -- FIXME

parseBookfile :: Parser AbstractSyntaxTree
parseBookfile = do
    version <- parseMagicLine
    return ()
