{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Logic.Propositional
    ( Fact(..)
    , bfsFromGoal
    , bfsFromPredicate
    ) where

import LibSolver.BoolExpr 

-----------------------------------------------------------------------------

-- |A simple knowledge base for propositional logic. We keep a list of known
--  propositions (the axioms) to be used as an inference base.
data PropKB p t = PropKB [BoolExpr]

-- |Экземпляр типажа базы знаний в пропозиционной логике. 
--   Для разрешения используется функция 'plResolution'.
instance KB PropKB PLExpr Bool where
    empty                  = PropKB []
    tell     (PropKB ps) p = PropKB $ ps ++ conjuncts (toCnf p)
    retract  (PropKB ps) p = PropKB $ L.delete p ps
    ask      (PropKB ps) p = plResolution (And ps) p
    askVars                = undefined
    axioms   (PropKB ps)   = ps

-----------------------------------------------------------------------------

plResolution :: BoolExpr -> BoolExpr -> Bool
plResolution s t = go $ conjuncts $ toCnf $ And [s, Not t]
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

-- |Return the set of all possible clauses obtained by resolving the two inputs.
plResolve :: PLExpr -> PLExpr -> [PLExpr]
plResolve p q =
    filter (not . isTautology) [resolve x y | x <- ps, y <- qs, complementary x y]
    where
        ps = disjuncts p
        qs = disjuncts q

        resolve x y = case L.union (L.delete x ps) (L.delete y qs) of
            [] -> Val False
            xs -> Or xs


----------------------
-- Definite Clauses --
----------------------

type Symbol = String

data DefiniteClause = DefiniteClause { premises :: [Symbol]
                                     , conclusion :: Symbol } deriving (Eq,Ord)

instance Show DefiniteClause where
    show (DefiniteClause []   hd) = hd 
    show (DefiniteClause body hd) =
        (concat $ L.intersperse " & " body) ++ " => " ++ hd

toDefiniteClause :: PLExpr -> ThrowsError DefiniteClause
toDefiniteClause (Val x) = return $ DefiniteClause [] (show x)
toDefiniteClause (Var x) = return $ DefiniteClause [] x
toDefiniteClause (p `Implies` q) = if all isAtom xs && isAtom q
    then return $ DefiniteClause (map toSym xs) (toSym q)
    else throwError InvalidExpression
        where xs = conjuncts p
toDefiniteClause _ = throwError InvalidExpression

toSym :: PLExpr -> Symbol
toSym (Val x) = show x
toSym (Var x) = x
toSym _       = error "Not an atom -- AI.Logic.Propositional.toSym"

isFact :: DefiniteClause -> Bool
isFact (DefiniteClause [] _) = True
isFact _                     = False

facts :: [DefiniteClause] -> [String]
facts = map conclusion . filter isFact

----------------------
-- Forward Chaining --
----------------------

fcEntails :: [DefiniteClause] -> Symbol -> Bool
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

-----------------------------------------------------------------------------

class Fact a where
    isTautology :: a -> Bool

-- TODO: restrict only for proofed facts
class (Fact a) => SearchRule a where
    implies :: [f] -> r -> Maybe a

data (Fact f) => ProofResult f = ProofResult
    { prPath         :: [f]
    , pfIsProofed    :: Bool
    , pfFinishedFact :: f
    }

data (Fact f, SearchRule r) => ProofState f r = ProofState
    { psFactStore     :: [f]       -- Исходная база фактов
    , psRuleStore     :: [r]       -- Исходная база правил
    , psQueue         :: [r]       -- Очередь недоказанных правил
    , psProofedFacts  :: [f]       -- Список доказанных фактов
    , psProofedRules  :: [r]       -- Список доказанных правил
    , psIsFactToProof :: f -> Bool -- Цель доказательства
    , psPath          :: [f]       -- Путь по правилам
    , psIsFinished    :: Bool      -- Завершен вывод?
    }

fromPredicate :: (Fact f, SearchRule r) => [f] -> [f] -> [r] -> (f -> Bool) -> ProofState f r
fromPredicate fs npfs rs p = ProofState
    { psFactStore = fs
    , psRuleStore = rs
    , psQueue = rs
    , psProofedFacts = filter (`notElem` npfs) fs
    , psProofedRules = []
    , psIsFactToProof = p
    , psPath = []
    , psIsFinished = False
    }

fromGoal :: (Eq f, Fact f, SearchRule r) => [f] -> [f] -> [r] -> f -> ProofState f r
fromGoal fs npfs rs goal = fromPredicate fs npfs rs (== goal)

fromProofState :: (Fact f, SearchRule r) => ProofState f r -> ProofResult f
fromProofState ProofState
    { psPath=path@(p:_)
    , psQueue=[]
    } = ProofResult
    { prPath=path
    , pfIsProofed=True
    , pfFinishedFact=p
    }

fromProofState ProofState
    { psPath=path@(p:_)
    } = ProofResult
    { prPath=path
    , pfIsProofed=True
    , pfFinishedFact=p
    }

-----------------------------------------------------------------------------

proofStepHelper :: (Fact f, SearchRule r) => [r] -> [f] -> [(r, f)] -> [(r, f)]
proofStepHelper [] _ result = result
proofStepHelper (r:rs) fs result =
    case implies fs r of
        Just f -> proofStepHelper rs (f:fs) result
        Nothing -> proofStepHelper rs fs result

-- Очередь пуста, а вершина не найдена -> заканчиваем поиск
proofStep :: (Fact f, SearchRule r) => ProofState f r -> ProofState f r
proofStep ps@ProofState
    { psFactStore=[]
    } = ps
    { psIsFinished=True
    }

-- Очередь не пуста. Просматриваем список правил в поисках правила, которое можем доказать
-- Если ничего не может доказать, то заканчиваем доказательство
proofStep ps@ProofState
    { psFactStore=fs
    , psRuleStore=rs
    , psQueue=q
    , psProofedFacts=pf
    , psProofedRules=pr
    , psIsFactToProof=isFactToProof
    , psPath=path
    , psIsFinished=isFinished
    } = ps
    { psQueue=q'
    , psProofedFacts=pf'
    , psProofedRules=pr'
    , psIsFinished=psIsFinished'
    } where (newProofedRules, newProofedFacts) = unzip $ proofStepHelper rs fs []
            (psIsFinished', isProofed') = case newProofedFacts of
                [] -> True
                _ -> False
            pf' = pf ++ newProofedFacts
            pr' = pr ++ newProofedRules
            q' = filter (`notElem` pr) rs
            psIsFinished'' = foldMap isFactToProof newProofedFacts

proofHelper :: (Fact f, SearchRule r) => ProofState f r -> ProofState f r
proofHelper initState =
    let nextStep = proofStep initState
    in (if psIsFinished nextStep then nextStep else proofHelper nextStep)

-----------------------------------------------------------------------------

bfsFromGoal :: (Fact f, SearchRule r) => [f] -> [r] -> (f -> Bool) -> ProofResult f
bfsFromGoal fs rs p = fromProofState $ proofHelper $ fromPredicate fs fs rs p

proofFact :: (Fact f, SearchRule r) => [f] -> [r] -> f -> ProofResult f
proofFact fs rs goal = fromProofState $ proofHelper $ fromGoal fs fs rs goal

