module Main (main) where

import Test.QuickCheck

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

prop_mylast :: [Int] -> Property
prop_mylast xs = not (null xs) ==> myLast xs == last xs

some_test :: [Int] -> Property
    let inGraph = Graph [
          Vertex "a" ["b", "c"          ] 0 ""
        , Vertex "b" ["a", "d", "e"     ] 0 ""
        , Vertex "c" ["a", "d"          ] 0 ""
        , Vertex "d" ["b", "c", "e"     ] 0 ""
        , Vertex "e" ["b", "d", "f", "g"] 0 ""
        , Vertex "f" ["e", "g", "h"     ] 0 ""
        , Vertex "g" ["e", "f", "i"     ] 0 ""
        , Vertex "h" ["f", "i"          ] 0 ""
        , Vertex "i" ["g", "h"          ] 0 ""
    ]

    let queue = graphVertexes inGraph ["e"]
    let outGraph = Graph queue
    let seen = queue
    let graph = bfs inGraph outGraph queue seen

main :: IO ()
main = quickCheck prop_mylast