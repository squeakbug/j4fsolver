module LibSolver.Search.AStar where

import LibSolver.Search
import LibSolver.Search.Informed

aStarSearch :: (Problem p s a, Ord s) =>
               Heuristic s a
            -> p s a
            -> Maybe (Node s a)
aStarSearch h = bestFirstGraphSearch (\n -> h n + cost n)

aStarSearch' :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
aStarSearch' prob = aStarSearch (heuristic prob) prob
