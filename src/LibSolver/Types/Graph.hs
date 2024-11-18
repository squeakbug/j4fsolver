module LibSolver.Types.Graph where

import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as M
import qualified Data.List as L

-- |Type for unweighted undirected graphs.
type Graph a = Map a [a]

type DiGraph a = Map a [a]

-- |Create a directed graph from an adjacency list.
toGraph :: (Ord a) => [(a, [a])] -> Graph a
toGraph = M.fromList

-- |Create an undirected graph from an adjacency list.
toUndirectedGraph :: Ord a => [(a, [a])] -> Graph a
toUndirectedGraph xs = fromPairRep . symmetrize .
    toPairRep $ toGraph xs

-- |Convert an unweighted graph to its adjacency list representation.
fromGraph :: Graph a -> [(a, [a])]
fromGraph = M.toList

-- |Get a list of the nodes of the graph.
getNodes :: Graph a -> [a]
getNodes = M.keys

-- |Get a list of the outbound links from node @a@.
getNeighbours :: Ord a => a -> Graph a -> [a]
getNeighbours a g = case M.lookup a g of
    Nothing -> error "Vertex not found in graph -- GETNEIGHBOURS"
    Just ls -> ls

-- |Return 'True' if and only if an edge exists between @x@ and @y@.
getEdge :: Ord a => a -> a -> Graph a -> Bool
getEdge x y g = case M.lookup x g of
    Nothing -> error "Vertex not found in graph -- GETEDGE"
    Just ys -> y `elem` ys

-- |Add an edge between two vertices to a 'Graph'.
addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge x y = M.adjust (y:) x

-- |Add an undirected edge between two vertices to a WeightedGraph.
addUndirectedEdge :: Ord a => a -> a -> Graph a -> Graph a
addUndirectedEdge x y graph = addEdge y x (addEdge x y graph)

-- |Convert an unweighted graph to its ordered pair representation.
toPairRep :: Graph a -> [(a,a)]
toPairRep xs = [ (a,b) | (a,bs) <- fromGraph xs, b <- bs ]

-- |Convert an unweighted graph from its ordered pair representation.
fromPairRep :: Ord a => [(a,a)] -> Graph a
fromPairRep xs = go xs M.empty
    where
        go []         m = m
        go ((a,b):xs') m = go xs' (M.insert a newList m)
            where
                newList = b : fromMaybe [] (M.lookup a m)

-----------------------------------------------------------------------------

-- |Take an unweighted graph in ordered pair representation and add in all of
--  the reverse links, so that the resulting graph is directed.
symmetrize :: Eq a => [(a,a)] -> [(a,a)]
symmetrize xs = L.nub $ concat [ [(a,b),(b,a)] | (a,b) <- xs ]
