{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.XML.HXT.Core

import LibSolver.Search
import LibSolver.Search.Uninformed
import LibSolver.Types.WeightedGraph (WeightedGraph)
import LibSolver.Util hiding (no)

import Types

-- Function to parse the GraphML file and return arcs and nodes
parseGraph :: FilePath -> IO ([(String, [(String, Cost)])], [(String, Location)])
parseGraph filePath = do
    runX $ readDocument [withValidate no] filePath >>>
        processNodes >>> processEdges

-- Process nodes to extract locations
processNodes :: ArrowXml a => a XmlTree [(String, Location)]
processNodes = 
    deep (isElem >>> hasName "node") >>>
    getChildren >>> 
    arr (\children -> 
        let nodeId = getAttrValue "id" children
            xCoord = read $ getAttrValue "x" children :: Int
            yCoord = read $ getAttrValue "y" children :: Int
        in (nodeId, (xCoord, yCoord)))

-- Process edges to extract arcs
processEdges :: ArrowXml a => a XmlTree [(String, [(String, Cost)])]
processEdges =
    deep (isElem >>> hasName "edge") >>>
    getChildren >>>
    arr (\children ->
        let sourceNode = getAttrValue "source" children
            targetNode = getAttrValue "target" children
            weight = read $ getAttrValue "weight" children :: Int
        in (sourceNode, [(targetNode, weight)]))

-- Helper to extract attributes from XML nodes
-- getAttrValue :: String -> [XmlTree] -> String
-- getAttrValue attrName children =
--     case [v | (n, v) <- concatMap getAttributes children, n == attrName] of
--         (x:_) -> x
--         []    -> ""
