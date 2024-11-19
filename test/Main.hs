module Main (main) where

import Test.Hspec

import LibSolver.Search
import LibSolver.Search.Uninformed

data IntProblem s a = IntProblem
    { pStart   :: Int
    , pRadius    :: Int
    }

instance Problem IntProblem Int Int where
    initial (IntProblem start _) = start
    goalTest (IntProblem _ rad) coord = abs coord >= rad
    successor (IntProblem _ _) cur = [(-1, cur - 1), (1, cur + 1)]

main :: IO ()
main = hspec $ do
    describe "breadthFirstGraphSearch" $ do

        it "finds the goal in simple case" $ do
            let problem :: IntProblem Int Int = IntProblem { pStart = 0, pRadius = 5 }
            let result = breadthFirstGraphSearch problem
            let goal' :: Maybe Int = fmap (abs . state) result

            goal' `shouldBe` Just 5

    describe "depthFirstGraphSearch" $ do

        it "finds the goal in simple case" $ do
            let problem :: IntProblem Int Int = IntProblem { pStart = 0, pRadius = 5 }
            let result = depthFirstGraphSearch problem
            let goal' :: Maybe Int = fmap (abs . state) result

            goal' `shouldBe` Just 5
