{-# OPTIONS_GHC -Wno-orphans #-}
module LibSolver.Search.Compare where

import Control.Monad
import Data.IORef
import Data.Maybe

import GHC.IO
import Control.DeepSeq

import LibSolver.Search
import LibSolver.Util
import LibSolver.Types.Table

-- |Wrapper for a problem that keeps statistics on how many times nodes were
--  expanded in the course of a search. We track the number of times 'goalCheck'
--  was called, the number of times 'successor' was called, and the total number
--  of states expanded.
data ProblemIO p s a = PIO
    { problemIO     :: p s a
    , numGoalChecks :: IORef Int
    , numSuccs      :: IORef Int
    , numStates     :: IORef Int
    }

-- |Construct a new ProblemIO, with all counters initialized to zero.
mkProblemIO :: p s a -> IO (ProblemIO p s a)
mkProblemIO p = do
    i <- newIORef 0
    j <- newIORef 0
    k <- newIORef 0
    return (PIO p i j k)

-- |Make ProblemIO into an instance of Problem. It uses the same implementation
--  as the problem it wraps, except that whenever 'goalTest' or 's'
instance (Problem p s a, Eq s, Show s) => Problem (ProblemIO p) s a where
    initial (PIO p _ _ _) = initial p

    goalTest (PIO p n _ _) s = unsafePerformIO $ do
        modifyIORef n (+1)
        return (goalTest p s)

    successor (PIO p _ n m) s = unsafePerformIO $ do
        let succs = successor p s
        modifyIORef n (+1)
        modifyIORef m (+length succs)
        return succs

    costP (PIO p _ _ _) = costP p

    heuristic (PIO p _ _ _) = heuristic p

-- |Given a problem and a search algorithm, run the searcher on the problem
--  and return the solution found, together with statistics about how many
--  nodes were expanded in the course of finding the solution.
testSearcher :: p s a -> (ProblemIO p s a -> t) -> IO (t,Int,Int,Int)
testSearcher prob searcher = do
    p@(PIO _ numGoalChecks numSuccs numStates) <- mkProblemIO prob
    let result = searcher p in result `seq` do
        i <- readIORef numGoalChecks
        j <- readIORef numSuccs
        k <- readIORef numStates
        return (result, i, j, k)

-- |NFData instance for search nodes.
instance (NFData s, NFData a) => NFData (Node s a) where
    rnf (Node state parent action cost depth value) = 
        state `seq` parent `seq` action `seq`
        cost `seq` depth `seq` value `seq`
        Node state parent action cost depth value `seq` ()

-- |Run a search algorithm over a problem, returning the time it took as well
--  as other statistics.
testSearcher' :: (NFData t) => p s a -> (ProblemIO p s a -> t) -> IO (t,Int,Int,Int,Int)
testSearcher' prob searcher = do
    p@(PIO _ numGoalChecks numSuccs numStates) <- mkProblemIO prob
    (result, t) <- timed (searcher p)
    i <- readIORef numGoalChecks
    j <- readIORef numSuccs
    k <- readIORef numStates
    return (result, t, i, j, k)

-- |Test multiple searchers on the same problem, and return a list of results
--  and statistics.
testSearchers :: [ProblemIO p s a -> t] -> p s a -> IO [(t,Int,Int,Int)]
testSearchers searchers prob = testSearcher prob `mapM` searchers

-- |Given a list of problems and a list of searchers, run every algorithm on
--  every problem and print out a table showing the performance of each.
compareSearchers :: (Show t) =>
                    [ProblemIO p s a -> t]  -- ^ List of search algorithms
                 -> [p s a]                 -- ^ List of problems
                 -> [String]                -- ^ Problem names
                 -> [String]                -- ^ Search algorithm names
                 -> IO [[(t,Int,Int,Int)]]  
compareSearchers searchers probs header rownames = do
    results <- testSearchers searchers `mapM` probs
    printTable 20 (map (map f) (transpose results)) header rownames
    return results
    where
        f (x,i,j,k) = SB (i,j,k)

-- |Given a problem and a list of searchers, run each search algorithm over the
--  problem, and print out a table showing the performance of each searcher.
--  The columns of the table indicate: [Algorithm name, Depth of solution, 
--  Cost of solution, Number of goal checks, Number of node expansions,
--  Number of states expanded] .
detailedCompareSearchers ::
        [ProblemIO p s a -> Maybe (Node s1 a1)] -- ^ List of searchers
     -> [String]                                -- ^ Names of searchers
     -> p s a                                   -- ^ Problem
     -> IO ()
detailedCompareSearchers searchers names prob = do
    result <- testSearchers searchers prob
    table  <- forM result $ \(n,numGoalChecks,numSuccs,numStates) -> do
        let d = depth $ fromJust n
        let c = round $ cost $ fromJust n
        let b = fromIntegral numStates ** (1/fromIntegral d)
        return [SB d,SB c,SB numGoalChecks,SB numSuccs,SB numStates,SB b]
    printTable 20 table header names
    where
        header = ["Searcher","Depth","Cost","Goal Checks","Successors",
                  "States","Eff Branching Factor"]
