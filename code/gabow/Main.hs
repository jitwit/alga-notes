{-# language FlexibleContexts, GADTs, LambdaCase #-}

module Main where

import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.AdjacencyMap
import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Data.Map as Map
import Data.List

import Gabow

piG :: AdjacencyMap Int
piG = 3*1*4*1*5

data VertexView a
  = V'unknown a
  | V'entered a Int
  | V'assigned a Int Int
  deriving (Show,Eq)

data EdgeView a
  = E'unknown (a,a)
--   | E'inside (a,a)
  | E'outside (a,a)

viewVertex :: Ord a => a -> StateSCC a -> VertexView a
viewVertex v st = case Map.lookup v (preorders st) of
  Nothing -> V'unknown v
  Just p -> case Map.lookup v (components st) of
    Nothing -> V'entered v p
    Just s -> V'assigned v p s

viewEdge :: Ord a => (a,a) -> StateSCC a -> EdgeView a
viewEdge e st | e `elem` outer_edges st = E'outside e
              | otherwise = E'unknown e

draw'vertex :: (IsName a, Show a) => VertexView a -> Diagram B
draw'vertex = \case
  V'unknown v -> text (show v <> ",?,?") # font'
                 <> circ' v # fc gray
  V'entered v p -> text (show v <> "," <> show p <> ",?") # font'
                   <> circ' v # fc red
  V'assigned v p s -> text (show v <> "," <> show p <> "," <> show s) # font'
                    <> circ' v # fc blue
  where font' = fc white # fontSize 6
        circ' v = circle 0.4 # named v # lw thin

-- style'edge :: EdgeView a -> ArrowOpts
style'edge = \case
  E'unknown e -> with
    & headLength .~ local 0.2
--    & gaps .~ local 0.05
    & shaftStyle %~ lc green
  _ -> with
    & headLength .~ local 0.2
--    & gaps .~ local 0.05
    & shaftStyle %~ lc black

pi'slice :: StateSCC Int -> Diagram B
pi'slice st =
  let zs = p2 <$> [(-1,0),(0,1),(0,-1),(1,0)]
      vs = [3,1,4,5]
      es = edgeList piG
      vsD = [ draw'vertex (viewVertex v st) | v <- vs ]
      connE e@(u,v) d = connectOutside' (style'edge $ viewEdge e st) u v d
      pthD = text ("path stack: " <> show (dfsPath st)) # fontSize 9
      bndD = text ("boundary stack: " <> show (map snd $ boundary st)) # fontSize 9
      stackD = translate (V2 0 0.7) $ vcat' (with & sep .~ 0.5) [pthD,bndD]
   in pad 1.2 $ hcat' (with & sep .~ 2) $ [foldr connE (atPoints zs vsD) es,stackD]

slices = nub $ traceGabow piG
sections n = unfoldr spl where
  spl xs = case splitAt n xs of
    ([],_) -> Nothing
    (ys,zs) -> Just (ys,zs)

pi'diagram :: [StateSCC Int] -> Diagram B
pi'diagram = vcat' (with & sep .~ 2) . map (hcat' (with & sep .~ 2)) . sections 4 . map pi'slice

main :: IO ()
main = do
  let f = id
  mapM_ (print . f) $ traceGabow piG
  renderSVG "gabow.svg" (mkSizeSpec (Just <$> V2 800 450)) (pi'diagram slices)
