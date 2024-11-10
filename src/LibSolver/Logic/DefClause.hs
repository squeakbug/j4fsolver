module LibSolver.Logic.DefClause where

{-
data DefClauseKB p t = DC [DefiniteClause]

instance KB DefClauseKB DefiniteClause Bool where
    empty             = DC []
    tell    (DC cs) c = DC $ cs ++ [c]
    retract (DC cs) c = DC $ L.delete c cs
    ask     (DC cs) c = if isFact c
        then fcEntails cs (conclusion c)
        else False
    askVars           = undefined
    axioms  (DC cs)   = cs 
-}
