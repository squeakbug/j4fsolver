{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.DiGraph 
    ( DiGraph(..)
    ) where

import LibSolver.Vertex (Vertex(..))

class DiGraph dg where
    -- Список вершин (вычисление допускает расхождение)
    giVertices :: dg -> [Vertex a]
    -- Список смежных вершин (вычисление допускает расхождение)
    giVertexNeighbors :: dg -> Vertex a -> [Vertex a]
