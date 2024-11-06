{-# LANGUAGE FlexibleContexts #-}

module LibSolver.BoolExpr.Parser
  (-- * Parsing function
  parseBoolExpr
   -- * Language definition and components
  ,languageDef
  ,lexer
  ,identifier
  ,whiteSpace
  ,symbol
  )
where

import Data.Text (Text)
import Data.Void

import Control.Applicative
import Control.Monad

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import LibSolver.BoolExpr

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pVariable :: Parser BoolExpr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

parseBoolExpr :: CharParser st a -> CharParser st (BoolExpr a)
parseBoolExpr parseConst = disj
   where disj   = conj   `chainl1` orOp
         conj   = factor `chainl1` andOp
         factor =     (      (symbol "-"   >> return BNot)
                     <|> try (symbol "NOT" >> return BNot)
                     <|>     (return id                  )
                      )
                      `ap`
                         (  parens disj
                        <|> BConst `fmap` (Positive `fmap` parseConst)
                         )

         andOp = BAnd <$ option "" (symbol "AND")
         orOp  = BOr <$ symbol "OR"
