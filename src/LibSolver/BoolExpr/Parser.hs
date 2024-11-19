{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSolver.BoolExpr.Parser
  ( pBoolExpr
  )
where

import Data.Text (pack)

import Control.Monad.Combinators

import           Text.Megaparsec.Char
import           Text.Megaparsec            ((<?>))

import LibSolver.BoolExpr
import LibSolver.Util.Parser hiding (Expr(Var))

-- E => E '\/' T | E '/\' T | T
-- T => T '->' F | T = F | F
-- F => Var | Const | '!' F | '(' E ')'

pVar :: Parser (BoolExpr f Bool)
pVar = Var . pack <$> (var <?> "variable")

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
