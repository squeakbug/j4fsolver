{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSolver.BoolExpr.Parser
  ( pBoolExpr
  )
where

import Data.Void (Void)
import Data.Text (Text, pack)

import Control.Monad.Combinators

import           Text.Megaparsec.Char
import           Text.Megaparsec            ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

import LibSolver.BoolExpr

type Parser = P.Parsec Void Text

-----------------------------------------------------------------------------

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

-----------------------------------------------------------------------------

-- E => E \/ T | E /\ T | T
-- T => T -> F | T = F | F
-- F => Var | Const | !F | (E)

pVar :: Parser (BoolExpr f Bool)
pVar = Var <$> textVar
  where strVar = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "variable"
        textVar = pack <$> strVar 

pNotVar :: Parser (BoolExpr f Bool)
pNotVar = Not <$> pVar <* string "!"

pFactor :: Parser (BoolExpr f Bool)
pFactor = choice
  [ pVar
  , pNotVar
  , parens pBoolExpr
  ]

pTerm :: Parser (BoolExpr f Bool)
pTerm = choice
  [ pTerm *> string "->" *> pFactor
  , pTerm *> string "=" *> pFactor
  , pFactor
  ]

pBoolExpr :: Parser (BoolExpr f Bool)
pBoolExpr = choice
  [ pBoolExpr *> string "\\/" *> pTerm
  , pBoolExpr *> string "/\\" *> pTerm
  , pTerm
  ]