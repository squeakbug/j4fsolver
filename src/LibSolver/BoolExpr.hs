module LibSolver.BoolExpr where

import Control.Applicative ((<|>))
import Data.Set (Set)
import qualified Data.Set as Set

data Expr = Var Char
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Const Bool
  deriving (Show, Eq)

-----------------------------------------------------------------------------

-- Поиск первой свободной переменной в выражении
freeVariable :: Expr -> Maybe Char
freeVariable (Const _) = Nothing
freeVariable (Var v) = Just v
freeVariable (Not e) = freeVariable e 
freeVariable (Or x y) = freeVariable x <|> freeVariable y
freeVariable (And x y) = freeVariable x <|> freeVariable y

-- Get all the literals in an expression.
literals :: Expr -> Set Char
literals (Var v) = Set.singleton v
literals (Not e) = literals e
literals (And x y) = Set.union (literals x) (literals y)
literals (Or x y) = Set.union (literals x) (literals y)
literals _ = Set.empty

-----------------------------------------------------------------------------

-- Заменяет все вхождения переменной var на val
guessVariable :: Char -> Bool -> Expr -> Expr
guessVariable var val e =
  case e of 
    Var v -> if v == var
             then Const val
             else Var v
    Not e' -> Not (guess e')
    Or x y -> Or (guess x) (guess y)
    And x y -> And (guess x) (guess y)
    Const b -> Const b
  where
    guess = guessVariable var val

-----------------------------------------------------------------------------

simplify :: Expr -> Expr
simplify (Const b) = Const b
simplify (Var v) = Var v
simplify (Not e) =
  case simplify e of
    Const b' -> Const (not b')
    e' -> Not e'
    
simplify (Or x y) =
  let es = filter (/= Const False) [simplify x, simplify y]
  in
    if Const True `elem` es
    then Const True
    else
      case es of
        [] -> Const False
        [e] -> e
        [e1, e2] -> Or e1 e2

simplify (And x y) =
  let es = filter (/= Const True) [simplify x, simplify y]
  in
    if Const False `elem` es
    then Const False
    else
      case es of
        [] -> Const True
        [e] -> e
        [e1, e2] -> And e1 e2

-- Удаление двойных отрицаний
fixNegations :: Expr -> Expr
fixNegations expr =
  case expr of
    -- Удаление двойного отрицания
    Not (Not x) -> fixNegations x
    
    -- Закон Де Моргана
    Not (And x y) -> Or (fixNegations $ Not x) (fixNegations $ Not y)
    Not (Or x y) -> And (fixNegations $ Not x) (fixNegations $ Not y)

    Not (Const b) -> Const (not b)

    Not x -> Not (fixNegations x)
    And x y -> And (fixNegations x) (fixNegations y)
    Or x y -> Or (fixNegations x) (fixNegations y)
    x -> x

-- Закон дистрибутивности для булевого кольца
-- Например, A /\ (B \/ C) -> (A \/ B) /\ (A \/ C)
distribute :: Expr -> Expr
distribute expr =
  case expr of
    Or x (And y z) ->
      And (Or (distribute x) (distribute y))
          (Or (distribute x) (distribute z))
    Or (And y z) x -> 
      And (Or (distribute x) (distribute y))
          (Or (distribute x) (distribute z))

    Or x y -> Or (distribute x) (distribute y)
    And x y -> And (distribute x) (distribute y)
    Not x -> Not (distribute x)
    x -> x

-- Приведение к КНФ
cnf :: Expr -> Expr
cnf expr =
  if updated == expr
  then expr
  else cnf updated
  
  where
    updated = distribute (fixNegations expr)

-- catMaybes :: [Maybe a] -> [a]
-- Extract only the Just values of a list.
import Data.Maybe (catMaybes)

-- Literal polarity. Positive means the literal
-- is never negated; negative means it always is.
data Polarity = Positive | Negative | Mixed deriving (Show, Eq)

-- Determine whether a literal has a polarity.
-- Return Nothing if the literal doesn't appear in the expression.
literalPolarity :: Expr -> Char -> Maybe Polarity

-- Literal in positive polarity
literalPolarity (Var v) v'
  | v == v'   = Just Positive
  | otherwise = Nothing  
-- Literal in negative polarity
literalPolarity (Not (Var v)) v'
  | v == v'   = Just Negative
  | otherwise = Nothing

-- Combine polarities of sub expressions
literalPolarity e v =
  case e of
    And x y -> combinePolarities [x, y]
    Or x y -> combinePolarities [x, y]
    Not x -> error $ "Not in CNF: negation of a non-literal: " ++ show x
    Const _ -> Nothing
  where
    combinePolarities es =
      let polarities = mapMaybe (flip literalPolarity v) es
      in case polarities of
           [] -> Nothing
           ps -> if all (== Positive) ps
                 then Just Positive
                 else if all (== Negative) ps
                      then Just Negative
                      else Just Mixed

literalElimination :: Expr -> Expr
literalElimination e =
  let ls = Set.toList (literals e)
      ps = map (literalPolarity e) ls
      
      -- Find assignments we can make
      extractPolarized :: Char -> Maybe Polarity -> Maybe (Char, Bool)
      extractPolarized v (Just Positive) = Just (v, True)
      extractPolarized v (Just Negative) = Just (v, False)
      extractPolarized _ _ = Nothing
      
      -- Find *all* possible assignments
      assignments :: [(Char, Bool)]
      assignments = catMaybes $ zipWith extractPolarized ls ps
      
      -- Apply all the assignments.
      replacers :: [Expr -> Expr]
      replacers = map (uncurry guessVariable) assignments
      
      replaceAll :: Expr -> Expr
      replaceAll = foldl (.) id replacers
  in replaceAll e

-- Given a clause, determine whether it is a unit clause.
-- If it is, then return the variable name and the polarity;
-- if it isn't, return Nothing.
unitClause :: Expr -> Maybe (Char, Bool)
unitClause (Var v) = Just (v, True)
unitClause (Not (Var v)) = Just (v, False)
unitClause _ = Nothing

-- Reduce an expression into a list of clauses.
-- This has to traverse a tree of And constructors
-- and create a list out of them.
clauses :: Expr -> [Expr]
clauses (And x y) = clauses x ++ clauses y
clauses expr = [expr]

-- Extract all unit clauses from a CNF expression.
allUnitClauses :: Expr -> [(Char, Bool)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagation :: Expr -> Expr
unitPropagation expr = replaceAll expr
  where
    assignments :: [(Char, Bool)]
    assignments = allUnitClauses expr
  
    replaceAll :: Expr -> Expr
    replaceAll = foldl (.) id (map (uncurry guessVariable) assignments)
