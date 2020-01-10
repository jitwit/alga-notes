module Main where

import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.AdjacencyMap
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Gabow

piG :: AdjacencyMap Int
piG = 3*1*4*1*5

-- take slice of search state make diagram.
-- need to make diagram for vertices
-- need to make diagram for edges
-- need to decide placements


main :: IO ()
main = mapM_ print $ traceGabow piG
