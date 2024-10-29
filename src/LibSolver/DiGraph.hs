{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.DiGraph 
    ( DiGraph(..)
    ) where

import LibSolver.Vertex (Vertex(..))

class DiGraph dg a where
    -- Список смежных вершин (вычисление допускает расхождение)
    giVertexNeighbors :: dg -> Vertex a -> [Vertex a]
