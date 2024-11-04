module LibSolver.BoolExpr.DNF
  ( DNF
  , dnf
  ) where

import Data.Text

import LibSolver.BoolExpr

-- Not safe. Try to use Data.Tagged
newtype DNF a = BoolExpr a

-- Приведение к ДНФ
dnf :: (Boolean a) => BoolExpr a -> DNF a
dnf expr =
  if updated == expr
  then expr
  else cnf updated

  where
    updated = distribute (fixNegations expr)

-- Префиксный обход графа
clauses :: Boolean a => DNF a -> [DNF a]
clauses (Or x y) = clauses x ++ clauses y
clauses expr = [expr]

-- Выделить все выражения, состоящие из одной переменной
allUnitClauses :: (Boolean a) => DNF a -> [(Text, a)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagation :: (Boolean a) => DNF a -> DNF a
unitPropagation expr = replaceAll expr
  where
    assignments = allUnitClauses expr

    replaceAll = foldl (.) id (map (uncurry guessVariable) assignments)

