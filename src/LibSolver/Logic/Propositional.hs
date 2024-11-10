{-# LANGUAGE DataKinds #-}

module LibSolver.Logic.Propositional where

import qualified Data.List as L
import           Data.Map ((!))
import qualified Data.Map as M
import           Data.Text (unpack)

import LibSolver.BoolExpr
    ( BoolExpr(..)
    , BoolExprForm(CNF, DNF)
    , Boolean (bTrue, bFalse)
    , isAtom
    , complementary
    )
import LibSolver.BoolExpr.CNF (cnf, conjuncts)
import LibSolver.Logic (KB(..))
import LibSolver.BoolExpr.DNF (disjuncts, isTautology, dnf)
import LibSolver.Util (isSubSet, unorderedPairs)

-----------------------------------------------------------------------------

type ThrowsError   = Either LogicError

data LogicError    = ParseError
                   | InvalidExpression
                   | UnknownCommand
                   | DefaultError deriving (Show)

-- | A simple knowledge base for propositional logic. We keep a list of known
--   propositions (the axioms) to be used as an inference base.
-- 
--   TODO: Здесь определяется тип для kind, поэтому p - фантомный тип
--         Следует определить тип для kind в KB типаже
newtype PropKB p t = PropKB [BoolExpr CNF t]

-- | Экземпляр типажа базы знаний в пропозиционной логике. 
--   Для разрешения используется функция 'plResolution'.
instance (Boolean a) => KB PropKB (BoolExpr CNF a) a where
    empty                  = PropKB []
    tell     (PropKB ps) p = PropKB $ ps ++ conjuncts (cnf p)
    retract  (PropKB ps) p = PropKB $ L.delete p ps
    ask      (PropKB [l, r]) p = plResolution (And l r) p
    ask      _ _ = error "ask"
    askVars                = undefined
    axioms   (PropKB ps)   = ps

-----------------------------------------------------------------------------

-- Resolution

plResolution :: (Boolean a) => BoolExpr f a -> BoolExpr f a -> Bool
plResolution s t = go $ map dnf $ conjuncts $ cnf $ And s (Not t)
    where
        go clauses
          | contradictionDerived = True
          | new `isSubSet` clauses = False
          | otherwise = go (clauses `L.union` new)
          where
              (contradictionDerived, new)
                = foldr resolve (False, []) (unorderedPairs clauses)

        -- resolve :: ([BoolExpr DNF a], [BoolExpr DNF a]) 
        --         -> (Bool, [BoolExpr DNF a]) 
        --         -> (Bool, [BoolExpr DNF a])
        resolve (_,_) (True, new)  = (True, new)
        resolve (x,y) (False, new) = if Const bFalse `elem` resolvents
            then (True, new)
            else (False, new `L.union` resolvents)
            where
                resolvents = plResolve x y

-- | Return the set of all possible clauses obtained by resolving the two inputs.
plResolve :: (Boolean a) => BoolExpr DNF a -> BoolExpr DNF a -> [BoolExpr DNF a]
plResolve p q =
    filter (not . isTautology) [resolve x y | x <- ps, y <- qs, complementary x y]
    where
        ps = disjuncts p
        qs = disjuncts q

        resolve x y = case (x `L.delete` ps) `L.union` (y `L.delete` qs) of
            [] -> Const bFalse
            [l, r] -> Or l r
            _ -> error "Unreachable"


-----------------------------------------------------------------------------

-- Definite Clauses

type Symbol = String

data DefiniteClause = DefiniteClause
    { premises :: [Symbol]
    , conclusion :: Symbol
    } deriving (Eq,Ord)

instance Show DefiniteClause where
    show (DefiniteClause []   hd) = hd
    show (DefiniteClause body hd) =
        L.intercalate " & " body ++ " => " ++ hd

toDefiniteClause :: (Show a, Boolean a) => BoolExpr CNF a -> ThrowsError DefiniteClause
toDefiniteClause (Const x) = return $ DefiniteClause [] (show x)
toDefiniteClause (Var x) = return $ DefiniteClause [] (unpack x)
toDefiniteClause (p `Impl` q) = if all isAtom xs && isAtom q
    then return $ DefiniteClause (map toSym xs) (toSym q)
    else error "InvalidExpression"
        where xs = conjuncts p
toDefiniteClause _ = error "InvalidExpression"

toSym :: (Show a, Boolean a) => BoolExpr f a -> Symbol
toSym (Const x) = show x
toSym (Var x) = unpack x
toSym _       = error "Not an atom -- AI.Logic.Propositional.toSym"

isFact :: (Boolean a) => DefiniteClause -> a
isFact (DefiniteClause [] _) = bTrue
isFact _                     = bFalse

facts :: [DefiniteClause] -> [String]
facts = map conclusion . filter isFact

-----------------------------------------------------------------------------

-- Forward Chaining

fcEntails :: [DefiniteClause] -> Symbol -> Bool
fcEntails kb q = go initialCount [] (facts kb)
    where
        go _     _        []     = False
        go count inferred (p:ps)
          | p == q = True
          | p `elem` inferred = go count inferred ps
          | otherwise = go count' (p:inferred) agenda'
          where
              (count', agenda') = run kb p count ps

        run []     _ count agenda = (count, agenda)
        run (c:cs) p count agenda
          | p `notElem` premises c = run cs p count agenda
          | n == 1 = run cs p count' (conclusion c:agenda)
          | otherwise = run cs p count' agenda
          where
              n = count ! c
              count' = M.insert c (n - 1) count

        initialCount = foldr f M.empty kb
            where f c = M.insert c (length $ premises c)
