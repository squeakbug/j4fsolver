module Types where

import Data.Map (Map)

import LibSolver.Search
import LibSolver.Types.WeightedGraph (WeightedGraph)

-- |Data structure to hold a graph (edge weights correspond to the distance
--  between nodes) and a map of graph nodes to locations.
data GraphMap a = G
    { getGraph     :: WeightedGraph a Cost
    , getLocations :: Map a Location
    } deriving (Show,Read)

-- |Type synonym for a pair of doubles, representing a location in cartesian
--  coordinates.
type Location = (Double,Double)