{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.Vertex 
    ( Vertex(..)
    , VertexLabel
    ) where

import Data.Text (Text)

import LibSolver.Search.SearchState (SearchState)

type VertexLabel = Text

data Vertex a where
    Vertex :: (SearchState a, Read a) => 
        { vertexLabel :: VertexLabel
        , vertexState :: a
        } -> Vertex a

deriving instance Show a => Show (Vertex a)
deriving instance Eq a => Eq (Vertex a)
deriving instance Ord a => Ord (Vertex a)
