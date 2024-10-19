module Executor where

import Data.List

{- (lhs, rhs, isTerminating) -}
type Rule = (String, String, Bool)
type Algor = [Rule]

contains :: String -> String -> Bool
contains s@(x:xs) sub = sub `isPrefixOf` s || xs `contains` sub
contains [] _ = False

findRule :: Algor -> String -> Maybe Rule
findRule a w = find (\(l,_,_) -> w `contains` l) a

applyRule :: Rule -> String -> String
applyRule (l,r,b) s@(x:xs) | l `isPrefixOf` s = r ++ (s \\ l)
                           | otherwise        = x : applyRule (l,r,b) xs

applyAlg :: Algor -> String -> Maybe (String, Bool)
applyAlg a w = case findRule a w of 
               Just r@(_,_,b) -> Just (applyRule r w, b)
               Nothing        -> Nothing

run :: Algor -> String -> [String]
run a w = case applyAlg a w of
          Just (w', False) -> w : run a w'
          Just (w', True)  -> [w, w']
          Nothing          -> [w]