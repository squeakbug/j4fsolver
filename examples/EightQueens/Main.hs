{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Relude

import LibSolver.Search.SearchState (SearchState)

type Placement = (Int, Int)

data Board = Board
    { placements :: [Placement]
    }

instance SearchState Board where
    produce (Board placements) = 

main :: IO ()
main = do
    putStrLn "Hello world!"
