{-# LANGUAGE OverloadedStrings #-}

module LibSolver.Util.Parser where

import Control.Applicative
import Data.Void (Void)
import Data.Text (Text)

import           Text.Megaparsec.Char
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

type Parser = P.Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

var :: Parser String
var = lexeme ((:) <$> letterChar <*> many alphaNumChar)
