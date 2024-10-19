{-# LANGUAGE NoImplicitPrelude #-}

module LibSolver
    ( module LibSolver.Search
    , module LibSolver.Graph
    , module LibSolver.Executor
    ) where

import Relude

import LibSolver.Search
import LibSolver.Graph
import LibSolver.Executor

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Как эффективно представлять пространства состояний (например, игровую карту)
