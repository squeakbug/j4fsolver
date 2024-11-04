module LibSolver.BoolExpr.CNF
  ( CNF
  , cnf
  ) where

import Data.Text

import LibSolver.BoolExpr

-- Not safe. Try to use Data.Tagged
newtype CNF a = BoolExpr a

-- Приведение к КНФ
cnf :: (Boolean a) => BoolExpr a -> CNF a
cnf expr =
  if updated == expr
  then expr
  else cnf updated

  where
    updated = distribute (fixNegations expr)

-- Префиксный обход графа
clauses :: Boolean a => CNF a -> [CNF a]
clauses (And x y) = clauses x ++ clauses y
clauses expr = [expr]

-- Выделить все выражения, состоящие из одной переменной
allUnitClauses :: (Boolean a) => CNF a -> [(Text, a)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagation :: (Boolean a) => CNF a -> CNF a
unitPropagation expr = replaceAll expr
  where
    assignments = allUnitClauses expr

    replaceAll = foldl (.) id (map (uncurry guessVariable) assignments)

