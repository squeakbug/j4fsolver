{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Proposition.Forward
    ( proofFact
    ) where

class Fact a where
    isTautology :: a -> Bool

class (Fact a) => SearchRule a where
    implies :: a -> Maybe a

newtype (SearchRule a) => RuleStore a = RuleStore [a]
newtype (Fact a) => FactStore a = FactStore [a]

data (Fact f) => ProofResult f = ProofResult
    { prPath      :: [f]
    , pfIsProofed :: Bool
    }

data (Fact f, SearchRule r) => ProofState f r = ProofState
    { psFactStore    :: FactStore f -- Исходный граф
    , psRuleStore    :: RuleStore r -- Предикат: вершина является целевой
    , psQueue        :: [r]         -- Очередь
    , psProofedFacts :: [f]         -- Доказанные факты
    , psProofedRules :: [r]         -- Доказанные правила
    , psPath         :: [f]         -- Путь от данных к цели
    , psIsFinished   :: Bool        -- Завершен вывод?
    }

-----------------------------------------------------------------------------

-- Очередь пуста, а вершина не найдена -> заканчиваем поиск
proofStep :: (Fact f, SearchRule r) => ProofState f r -> ProofState f r
proofStep ps@ProofState
    { psFactStore=FactStore []
    } = ps
    { psIsFinished=True
    }

proofHelper :: (Fact f, SearchRule r) => ProofState f r -> ProofState f r
proofHelper initState =
    let nextStep = proofStep initState
    in (if psIsFinished nextStep then nextStep else proofHelper nextStep)

-----------------------------------------------------------------------------

proofFact :: (Fact f, SearchRule r) => FactStore f -> RuleStore r -> f -> ProofResult f
proofFact factStore ruleStore fact = ProofResult { prPath = [], pfIsProofed = True}

