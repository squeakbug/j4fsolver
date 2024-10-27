module LibSolver.Search.Dijkstra where


import Control.Applicative hiding (empty)
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as PSQ

----------------------------------------------------------------

data Vertex = A | B | C | D | E deriving (Eq,Ord,Show)

type Cost = Int

----------------------------------------------------------------
type Graph = [(Vertex,[(Vertex,Cost)])]

-- http://mew.org/~kazu/material/2012-psq.pdf

sample :: Graph
sample = [
    (A, [(B, 10), (D, 4)])
  , (B, [(A, 10), (C, 2), (E, 2)])
  , (C, [(B, 2),  (E, 1)])
  , (D, [(A, 4),  (E, 3)])
  , (E, [(B, 2),  (C, 1), (D, 3)])
]

----------------------------------------------------------------

vertices :: Graph -> [Vertex]
vertices = map fst

adjacent :: Graph -> Vertex -> [(Vertex,Cost)]
adjacent g v = fromJust $ lookup v g

----------------------------------------------------------------

data Priority = Priority Cost Vertex deriving (Eq,Show)

instance Ord Priority where
    Priority c1 _ `compare` Priority c2 _ = c1 `compare` c2

type Queue = PSQ Vertex Priority
type Mapping = (Vertex,Priority)

----------------------------------------------------------------

relax :: Mapping -> Queue -> Queue
relax (k,p@(Priority c _)) = PSQ.adjust update k
  where
    update p0@(Priority c0 _)
        | c < c0    = p
        | otherwise = p0

relaxList :: Queue -> [Mapping] -> Queue
relaxList = foldr relax

----------------------------------------------------------------

dijkstra :: Graph -> Vertex -> [(Vertex,Cost,Vertex)]
dijkstra g s = unfoldr (spf g) $ relax (s,Priority 0 s) q
  where
    q = PSQ.fromList $ map (\k -> k :-> Priority maxBound s) (vertices g)

spf :: Graph -> Queue -> Maybe ((Vertex,Cost,Vertex), Queue)
spf g q = extractMin g <$> PSQ.minView q

extractMin :: Graph 
           -> (Binding Vertex Priority, Queue)
           -> ((Vertex, Cost, Vertex), Queue)
extractMin g (u :-> Priority c n, q') = ((u,c,n), relaxList q' adj)
  where
    adj = [(v,Priority (c + w) u) | (v,w) <- adjacent g u]
