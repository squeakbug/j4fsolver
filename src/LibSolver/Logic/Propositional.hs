{-# LANGUAGE DataKinds #-}

module LibSolver.Logic.Propositional
    ( 
    ) where

import qualified Data.List as L
import           Data.Map (Map, (!))
import qualified Data.Map as M

import LibSolver.BoolExpr (BoolExpr(..), BoolExprForm(CNF), Boolean (bTrue, bFalse), isAtom)
import LibSolver.BoolExpr.CNF (cnf, conjuncts)
import LibSolver.Logic (KB(..))
import Control.Monad.Trans.Error (throwError)
import LibSolver.BoolExpr.DNF (disjuncts)

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
instance KB PropKB (BoolExpr CNF a) a where
    empty                  = PropKB []
    tell     (PropKB ps) p = PropKB $ ps ++ conjuncts (cnf p)
    retract  (PropKB ps) p = PropKB $ L.delete p ps
    ask      (PropKB ps) = plResolution (And ps)
    askVars                = undefined
    axioms   (PropKB ps)   = ps

-----------------------------------------------------------------------------

-- Resolution

plResolution :: BoolExpr f a -> BoolExpr f a -> a
plResolution s t = go $ conjuncts $ cnf $ And [s, Not t]
    where
        go clauses = if contradictionDerived
            then True
            else if new `isSubSet` clauses
                then False
                else go (L.union clauses new)

            where
                (contradictionDerived, new) =
                    foldr resolve (False, []) (unorderedPairs clauses)

        resolve (_,_) (True, new)  = (True, new)
        resolve (x,y) (False, new) = if false `elem` resolvents
            then (True, new)
            else (False, L.union new resolvents)
            where
                resolvents = plResolve x y

-- | Return the set of all possible clauses obtained by resolving the two inputs.
plResolve :: BoolExpr f a -> BoolExpr f a -> [BoolExpr f a]
plResolve p q =
    filter (not . isTautology) [resolve x y | x <- ps, y <- qs, complementary x y]
    where
        ps = disjuncts p
        qs = disjuncts q

        resolve x y = case L.union (L.delete x ps) (L.delete y qs) of
            [] -> Const bFalse
            xs -> Or xs


-----------------------------------------------------------------------------

-- Definite Clauses

type Symbol = String

data DefiniteClause = DefiniteClause { premises :: [Symbol]
                                     , conclusion :: Symbol } deriving (Eq,Ord)

instance Show DefiniteClause where
    show (DefiniteClause []   hd) = hd 
    show (DefiniteClause body hd) =
        (concat $ L.intersperse " & " body) ++ " => " ++ hd

toDefiniteClause :: BoolExpr f a -> ThrowsError DefiniteClause
toDefiniteClause (Const x) = return $ DefiniteClause [] (show x)
toDefiniteClause (Var x) = return $ DefiniteClause [] x
toDefiniteClause (p `Impl` q) = if all isAtom xs && isAtom q
    then return $ DefiniteClause (map toSym xs) (toSym q)
    else throwError InvalidExpression
        where xs = conjuncts p
toDefiniteClause _ = throwError InvalidExpression

toSym :: BoolExpr f a -> Symbol
toSym (Const x) = show x
toSym (Var x) = x
toSym _       = error "Not an atom -- AI.Logic.Propositional.toSym"

isFact :: (Boolean a) => DefiniteClause -> a
isFact (DefiniteClause [] _) = bTrue
isFact _                     = bFalse

facts :: [DefiniteClause] -> [String]
facts = map conclusion . filter isFact

-----------------------------------------------------------------------------

-- Forward Chaining

fcEntails :: [DefiniteClause] -> Symbol -> a
fcEntails kb q = go initialCount [] (facts kb)
    where
        go count inferred []     = False
        go count inferred (p:ps) = if p == q
            then True
            else if p `elem` inferred
                    then go count inferred ps
                    else go count' (p:inferred) agenda'
                        where (count', agenda') = run kb p count ps

        run []     p count agenda = (count, agenda)
        run (c:cs) p count agenda = if not (p `elem` premises c)
            then run cs p count agenda
            else if n == 1
                    then run cs p count' (conclusion c:agenda)
                    else run cs p count' agenda
            where
                n    = count ! c
                count' = M.insert c (n-1) count

        initialCount = foldr f M.empty kb
            where f c count = M.insert c (length $ premises c) count
