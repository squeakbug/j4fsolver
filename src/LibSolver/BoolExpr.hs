{-# LANGUAGE DatatypeContexts #-}

module LibSolver.BoolExpr where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

infix 4 /\
infix 4 \/

-- | A boolean type class.
class (Eq a, Ord a, Show a, Read a) => Boolean a where
  ( /\ ) :: a -> a -> a
  ( \/ ) :: a -> a -> a
  bNot   :: a -> a
  bTrue  :: a
  bFalse :: a
  beval  :: a -> Bool

instance Boolean Bool where
  ( /\ ) = (&&)
  ( \/ ) = (||)
  bNot = not
  bTrue = True
  bFalse = False
  beval True = True
  beval False = False

-- Может все же аппликативный функтор?
class BooleanFunc a where
  apply :: a -> [(Text, Bool)] -> Bool

-----------------------------------------------------------------------------

data (Boolean a) => BoolExpr a = Var Text
          | And (BoolExpr a) (BoolExpr a)
          | Or (BoolExpr a) (BoolExpr a)
          | Not (BoolExpr a)
          | Equal (BoolExpr a) (BoolExpr a)
          | Impl (BoolExpr a) (BoolExpr a)
          | Const a
  deriving (Eq, Ord, Show, Read)

instance Boolean a => BooleanFunc (BoolExpr a) where
  apply (Var text) args =
    case lookup text args of
      Just result -> result
      Nothing -> error $ "Словарь не определяет значение для параметра: " ++ show text

  apply (And x y) args = apply x args /\ apply y args
  apply (Or x y) args = apply x args \/ apply y args
  apply (Not x) args = not (apply x args)
  apply (Equal x y) args = apply x args /\ apply y args
  apply (Impl x y) args = apply x args /\ apply y args
  apply (Const x) _ = beval x

-- Literal polarity. Positive means the literal
-- is never negated; negative means it always is.
data Polarity = Positive | Negative | Mixed
  deriving (Eq, Ord, Show, Read)

-----------------------------------------------------------------------------

-- Поиск первой свободной переменной в выражении
freeVariable :: Boolean a => BoolExpr a -> Maybe Text
freeVariable (Const _) = Nothing
freeVariable (Var v) = Just v
freeVariable (Not e) = freeVariable e
freeVariable (Or x y) = freeVariable x <|> freeVariable y
freeVariable (And x y) = freeVariable x <|> freeVariable y
freeVariable (Equal x y) = freeVariable x <|> freeVariable y
freeVariable (Impl x y) = freeVariable x <|> freeVariable y

-- Get all the literals in an expression.
literals :: Boolean a => BoolExpr a -> Set Text
literals (Var v) = Set.singleton v
literals (Not e) = literals e
literals (And x y) = Set.union (literals x) (literals y)
literals (Or x y) = Set.union (literals x) (literals y)
literals (Impl x y) = Set.union (literals x) (literals y)
literals (Equal x y) = Set.union (literals x) (literals y)
literals _ = Set.empty

-----------------------------------------------------------------------------

-- Заменяет все вхождения переменной var на значение val
guessVariable :: Boolean a => Text -> a -> BoolExpr a -> BoolExpr a
guessVariable var val e =
  case e of
    Var v -> if v == var
             then Const val
             else Var v
    Not e' -> Not (guess e')
    Or x y -> Or (guess x) (guess y)
    And x y -> And (guess x) (guess y)
    Equal x y -> Equal (guess x) (guess y)
    Impl x y -> Impl (guess x) (guess y)
    Const b -> Const b
  where
    guess = guessVariable var val

-----------------------------------------------------------------------------

-- Операции упрощения

-- Удаление двойных отрицаний
-- (Not (Not (Var a))) -> Var a
fixNegations :: (Boolean a) => BoolExpr a -> BoolExpr a
fixNegations expr =
  case expr of
    -- Удаление двойного отрицания
    Not (Not x) -> fixNegations x

    Not (Const b) -> Const (bNot b)

    Not x -> Not (fixNegations x)
    And x y -> And (fixNegations x) (fixNegations y)
    Or x y -> Or (fixNegations x) (fixNegations y)
    x -> x

-- Закон исключения третьего
fixThird :: (Boolean a) => BoolExpr a -> BoolExpr a
fixThird expr =
  case expr of
    And (Var x) (Not (Var y)) | x == y -> (Const bFalse)
    And (Not (Var x)) (Var y) | x == y -> (Const bFalse)
    Or (Var x) (Not (Var y)) | x == y -> (Const bTrue)
    Or (Not (Var x)) (Var y) | x == y -> (Const bTrue)

-- Упрощает операции с константами:
-- (Var A) /\ (Const False) -> (Const False)
-- (Var A) \/ (Const True)  -> (Const True)
fixConstOps :: (Boolean a) => BoolExpr a -> BoolExpr a
fixConstOps (Const b) = Const b
fixConstOps (Var v) = Var v
fixConstOps (Not e) =
  case fixConstOps e of
    Const b' -> Const (bNot b')
    e' -> Not e'

fixConstOps (Or x y) =
  let es = filter (/= Const bFalse) [fixConstOps x, fixConstOps y]
  in
    if Const bTrue `elem` es
    then Const bTrue
    else
      case es of
        [] -> Const bFalse
        [e] -> e
        [e1, e2] -> Or e1 e2
        _ -> error "Unreachable"

fixConstOps (And x y) =
  let es = filter (/= Const bTrue) [fixConstOps x, fixConstOps y]
  in
    if Const bFalse `elem` es
    then Const bFalse
    else
      case es of
        [] -> Const bTrue
        [e] -> e
        [e1, e2] -> And e1 e2
        _ -> error "Unreachable"

fixConstOps _ = error "Not implemented"

-- Повторения
fixRepeats :: Boolean a => BoolExpr a -> BoolExpr a
fixRepeats (Var v) = (Var v)
fixRepeats (Not e) = Not (fixRepeats e)
fixRepeats (Or (Var x) (Var y)) | x == y = Var x
fixRepeats (Or x y) = Or (fixRepeats x) (fixRepeats y)
fixRepeats (And (Var x) (Var y)) | x == y = Var x
fixRepeats (And x y) = And (fixRepeats x) (fixRepeats y)

-- Распределительный закон для булевого кольца
-- Например, A /\ (B \/ C) -> (A \/ B) /\ (A \/ C)
distribute :: Boolean a => BoolExpr a -> BoolExpr a
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

-- Закон Де Моргана
fixDeMorgan :: Boolean a => BoolExpr a -> BoolExpr a
fixDeMorgan expr =
  case expr of
    Not (And x y) -> Or (fixNegations $ Not x) (fixNegations $ Not y)
    Not (Or x y) -> And (fixNegations $ Not x) (fixNegations $ Not y)
    x -> x

-----------------------------------------------------------------------------

-- Determine whether a literal has a polarity.
-- Return Nothing if the literal doesn't appear in the expression.
literalPolarity :: (Boolean a) => BoolExpr a -> Text -> Maybe Polarity

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
    _ -> error "Unreachable"
  where
    combinePolarities es =
      let polarities = mapMaybe (`literalPolarity` v) es
      in case polarities of
           [] -> Nothing
           ps -> if all (== Positive) ps
                 then Just Positive
                 else if all (== Negative) ps
                      then Just Negative
                      else Just Mixed

literalElimination :: (Boolean a) => BoolExpr a -> BoolExpr a
literalElimination e =
  let ls = Set.toList (literals e)
      ps = map (literalPolarity e) ls

      -- Find assignments we can make
      extractPolarized v (Just Positive) = Just (v, bTrue)
      extractPolarized v (Just Negative) = Just (v, bFalse)
      extractPolarized _ _ = Nothing

      -- Find *all* possible assignments
      assignments = catMaybes $ zipWith extractPolarized ls ps

      -- Apply all the assignments.
      replacers = map (uncurry guessVariable) assignments

      replaceAll = foldl (.) id replacers
  in replaceAll e

-- Определение, состоит ли выражение из одной переменной
-- Если да, то функция принимает значение (имя переменной, ее "полярность");.
unitClause :: (Boolean a) => BoolExpr a -> Maybe (Text, a)
unitClause (Var v) = Just (v, bTrue)
unitClause (Not (Var v)) = Just (v, bFalse)
unitClause _ = Nothing
