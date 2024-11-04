{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Logic.Propositional
    ( Fact(..)
    , bfsFromGoal
    , bfsFromPredicate
    ) where
import LibSolver.Search.Bfs (bfsFromPredicate)

-- |A KB for propositional logic. Inefficient, with no indexing.
class KB k => PropKB k:
    def __init__(self, sentence=None):
        super().__init__(sentence)
        self.clauses = []

    def tell(self, sentence):
        """Add the sentence's clauses to the KB."""
        self.clauses.extend(conjuncts(to_cnf(sentence)))

    def ask_generator(self, query):
        """Yield the empty substitution {} if KB entails query; else no results."""
        if tt_entails(Expr('&', *self.clauses), query):
            yield {}

    def ask_if_true(self, query):
        """Return True if the KB entails query, else return False."""
        for _ in self.ask_generator(query):
            return True
        return False

    def retract(self, sentence):
        """Remove the sentence's clauses from the KB."""
        for c in conjuncts(to_cnf(sentence)):
            if c in self.clauses:
                self.clauses.remove(c)

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

