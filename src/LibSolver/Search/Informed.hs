module LibSolver.Search.Informed where

import LibSolver.Search
import LibSolver.Search.Core
import LibSolver.Types.Queue

-- |Type synonym for heuristic functions. In principle they can take any
--  information at a search node into account, including cost already incurred
--  at this node, depth of the node, or the state reached so far.
type Heuristic s a = Node s a -> Double

-- |Best-first tree search takes a function that scores each potential successor
--  and prefers to explore nodes with the lowest score first.
bestFirstTreeSearch :: (Problem p s a) =>
                       Heuristic s a    -- ^ Function to score each node
                    -> p s a            -- ^ Problem
                    -> Maybe (Node s a)
bestFirstTreeSearch f = treeSearch (newPriorityQueue f)

-- |Best-first graph search keeps track of states that have already been visited
--  and won't visit the same state twice.
bestFirstGraphSearch :: (Problem p s a, Ord s) =>
                        Heuristic s a   -- ^ Function to score each node
                     -> p s a           -- ^ Problem
                     -> Maybe (Node s a)
bestFirstGraphSearch f = graphSearch (newPriorityQueue f)

-- |Minimum cost search preferentially explores nodes with the lowest cost
--  accrued, to guarantee that it finds the best path to the solution.
--  Also are called Dijkstra's algorithm
uniformCostSearch :: (Problem p s a, Ord s) => 
                     p s a 
                  -> Maybe (Node s a)
uniformCostSearch = bestFirstGraphSearch cost

-- |Greedy best-first search preferentially explores nodes with the lowest
--  cost remaining to the goal, ignoring cost already accrued.
greedyBestFirstSearch :: (Problem p s a, Ord s) => 
                         p s a 
                      -> Maybe (Node s a)
greedyBestFirstSearch prob = bestFirstGraphSearch (heuristic prob) prob

aStarSearch :: (Problem p s a, Ord s) =>
               Heuristic s a
            -> p s a
            -> Maybe (Node s a)
aStarSearch h = bestFirstGraphSearch (\n -> h n + cost n)

aStarSearch' :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
aStarSearch' prob = aStarSearch (heuristic prob) prob
