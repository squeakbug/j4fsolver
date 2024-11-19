module Main (main) where

import Data.Maybe

import Test.Hspec

import LibSolver.Search
import LibSolver.Search.Core

data IntProblem s a = IntProblem
    { pStart   :: Int
    , pGoal    :: Int
    }

instance Problem IntProblem Int Int where
    initial (IntProblem start _) = start
    goal (IntProblem _ goal') = goal'
    successor (IntProblem _ _) cur = [(-1, cur - 1), (1, cur + 1)]

main :: IO ()
main = hspec $ do
  describe "graphSearch" $ do

    it "finds the goal in simple case" $ do
        let problem :: IntProblem Int Int = IntProblem { pStart = 0, pGoal = 5 }
        let initialQueue = []
        let result = graphSearch initialQueue problem
        let goal' :: Maybe Int = fmap state result

        goal' `shouldBe` Just 5
