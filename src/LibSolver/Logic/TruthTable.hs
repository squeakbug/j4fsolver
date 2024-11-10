{-# LANGUAGE DataKinds #-}

module LibSolver.Logic.TruthTable where

import Data.Text (Text)

import qualified Data.List as L
import qualified Data.Map as M

import LibSolver.BoolExpr 
    ( BoolExpr(..)
    , BoolExprForm(CNF)
    , Boolean (bTrue, bFalse, bNot, (/\), (\/))
    , isAtom
    )
import LibSolver.Util
import LibSolver.BoolExpr.CNF (cnf, conjuncts)
import LibSolver.Logic (KB(..))

-----------------------------------------------------------------------------

-- |Конкретный экземпляр базы знаний пропозициональной логики, 
--  использующий таблицы истинности для выводов.
newtype TruthTableKB p t = TT [BoolExpr CNF t]

-- |The 'KB' instance for a knowledge base that uses truth tables for inference.
instance KB TruthTableKB (BoolExpr CNF a) a where
    empty              = TT []
    tell     (TT ps) p = TT $ ps ++ conjuncts (cnf p)
    retract  (TT ps) p = TT $ L.delete p ps
    ask      (TT [l, r]) p = ttEntails (And l r) p
    ask      _ _ = error "ask"
    askVars            = undefined
    axioms   (TT ps)   = ps

-----------------------------------------------------------------------------

-- |Does the first logical expression entail the second? This algorithm uses
--  truth tables (Fig 7.10).
ttEntails :: (Boolean a) => BoolExpr f a -> BoolExpr f a -> Bool
ttEntails s t = and $ ttCheck (s `Impl` t)

-- |Helper function for ttEntails. Evaluates the expression in all possible
--  models.
ttCheck :: (Boolean a) => BoolExpr f a -> [Bool]
ttCheck expr = map check $ allModels (vars expr)
    where
        check model = case plTrue model expr of
            Nothing -> error "Should never see this."
            Just v  -> v

        allModels vars = map (zip vars) (bools $ length vars)

-- |Is the propositional sentence a tautology - is it true in all possible
--  models (i.e. is it entailed by true?)
ttTrue :: (Boolean a) => BoolExpr f a -> Bool
ttTrue s = true `ttEntails` s

-- |Is the propositional sentence a contradiction - is it false in all
--  possible models (i.e. does it entail false?)
ttFalse :: (Boolean a) => BoolExpr f a -> Bool
ttFalse s = s `ttEntails` False

-- |Return 'True' if the propositional logic expression is true in the model,
--  and 'False' if it is false. If the model does not specify the value for
--  every proposition then return 'Nothing' (note that this may happen even
--  when the expression is tautological).
plTrue :: (Boolean a) => [(Text, a)] -> BoolExpr f a -> Maybe a
plTrue _ (Const b)  = Just b
plTrue model (Var p)  = lookup p model
plTrue model (Not p)  = bNot <$> plTrue model p
plTrue model (And l r) = (/\) <$> mapM (plTrue model) [l, r]
plTrue model (Or l r)  = (\/)  <$> mapM (plTrue model) [l, r]
plTrue model (Impl p q) = do
    x <- plTrue model p
    y <- plTrue model q
    return (bNot x \/ y)
plTrue model (Equal p q) = do
    x <- plTrue model p
    y <- plTrue model q
    return (x == y)
