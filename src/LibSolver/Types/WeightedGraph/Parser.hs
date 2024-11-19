{-# LANGUAGE OverloadedStrings #-}

module LibSolver.Types.WeightedGraph.Parser where

import Control.Monad.Combinators

import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import LibSolver.Util.Parser
import LibSolver.Types.WeightedGraph

parseNegNode :: Parser String
parseNegNode = var

parseWeight :: Parser Int
parseWeight = L.decimal

parsePosNode :: Parser (String, Int)
parsePosNode = do
    n <- var 
    v <- between "(" ")" parseWeight
    return (n, v)

parsePosNodeList :: Parser ((String, Int), [(String, Int)])
parsePosNodeList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- parsePosNode
      return (L.IndentMany Nothing (return . (header, )) parsePosNode)

type PNode = (String, [(String, Int)])

parseNode :: Parser PNode
parseNode = do 
    v <- parseNegNode
    string ":"
    (h, t) <- parsePosNodeList
    return (v, h:t)

parseWeightedGraphHelper :: Parser (PNode, [PNode])
parseWeightedGraphHelper = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- parseNode
      return (L.IndentMany Nothing (return . (header, )) parseNode)

-- |Parse an weighted graph from a string. Example:
--
--  A: B(1) C(1);\n\r
--  B: C(1) D(1);\n\r
--  C: D(1)
--
parseWeightedGraph :: Parser (WeightedGraph String Int)
parseWeightedGraph = do
    (h, t) <- parseWeightedGraphHelper
    return $ toGraph (h:t)
