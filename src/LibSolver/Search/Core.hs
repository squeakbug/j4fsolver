module LibSolver.Search.Core where

import Data.Maybe (listToMaybe)

import LibSolver.Search
import LibSolver.Types.Queue

import qualified Data.Set as S

-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. We don't worry about repeated paths
--  to a state. Returns first solution
treeSearch :: (Problem p s a, Queue q) =>
              q (Node s a)      -- ^ Контейнер
           -> p s a             -- ^ Задача
           -> Maybe (Node s a)  -- ^ Возможное решение
treeSearch q prob  = listToMaybe $ genericSearch f q prob
    where
        f node  closed = (expand prob node, closed)


-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. If two paths reach the same state, use
--  only the best one. Returns first solution
graphSearch :: (Problem p s a, Queue q, Ord s) =>
               q (Node s a)     -- ^ Контейнер
            -> p s a            -- ^ Задача
            -> Maybe (Node s a) -- ^ Возможное решение
graphSearch q prob = listToMaybe $ genericSearch f q prob
    where
        f node closed
            | state node `S.member` closed  = (newQueue,closed)
            | otherwise = (expand prob node, closed')
                where
                    closed' = state node `S.insert` closed


genericSearch :: (Queue q, Problem p s a) =>
                 (Node s a -> S.Set a1 -> ([Node s a], S.Set a1))
              -> q (Node s a) 
              -> p s a 
              -> [Node s a]
genericSearch f q prob = findFinalState  (genericSearchPath f (root prob `push` q))
    where
        findFinalState = filter  (goalTest prob . state)

-- Return a (potentially infinite) list of nodes to search.
-- Since the reult is lazy, you can break out early if you find a resut. 
genericSearchPath :: Queue q => 
                     (a -> S.Set a1 -> ([a], S.Set a1))
                  -> q a -> [a]
genericSearchPath f q  = go (q,S.empty)
    where
        go (fringe,closed)
            | empty fringe = []
            | otherwise = go'  (pop fringe) closed
        go' (node, rest) closed
            | (new,closed') <- f node closed = node : go (new `extend` rest, closed')
