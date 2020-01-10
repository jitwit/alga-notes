{-# language FlexibleContexts, LambdaCase, BangPatterns #-}

module Gabow (
    gabow
  , traceGabow
  , StateSCC (..)
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Maybe
import Data.Monoid
import Data.Tree
import GHC.Exts (toList,fromList)

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.Internal

import qualified Data.List                           as List
import qualified Data.Map.Strict                     as Map
import qualified Data.IntMap.Strict                  as IntMap
import qualified Data.Set                            as Set

traceGabow :: Ord a => AdjacencyMap a -> [StateSCC a]
traceGabow g = snd $ snd $ listen (evalStateT (gabow g) initialState) where
  initialState = C 0 0 [] [] Map.empty Map.empty IntMap.empty [] []

data StateSCC a
  = C { current       :: !Int
      , componentId   :: !Int
      , boundary      :: ![(Int,a)]
      , dfsPath       :: ![a]
      , preorders     :: !(Map.Map a Int)
      , components    :: !(Map.Map a Int)
      , inner_graphs  :: (IntMap.IntMap (List (AdjacencyMap a)))
      , inner_edges   :: ![(Int,(a,a))]
      , outer_edges   :: [(a,a)]
      } deriving (Show,Eq)

gabow :: (MonadWriter [StateSCC a] m, MonadState (StateSCC a) m, Ord a) => AdjacencyMap a -> m ()
gabow g =
  do let dfs u = do p_u <- enter u
                    setForEach_ (postSet u g) $ \v -> do
                      preorderId v >>= \case
                        Nothing  -> do
                          updated <- dfs v
                          if updated then outedge (u,v) else inedge (p_u,(u,v))
                        Just p_v -> do
                          scc_v <- hasComponent v
                          if scc_v
                            then outedge (u,v)
                            else popBoundary p_v >> inedge (p_u,(u,v))
                      logState
                    exit u
     forM_ (vertexList g) $ \v -> do
       assigned <- hasPreorderId v
       unless assigned $ void $ dfs v
  where
    logState = tell . (:[]) =<< get
    -- called when visiting vertex v. assigns preorder number to v,
    -- adds the (id, v) pair to the boundary stack b, and adds v to
    -- the path stack s.
    enter v = do C pre scc bnd pth pres sccs gs es_i es_o <- get
                 let !pre' = pre+1
                     !bnd' = (pre,v):bnd
                     !pth' = v:pth
                     !pres' = Map.insert v pre pres
                 put $! C pre' scc bnd' pth' pres' sccs gs es_i es_o
                 logState
                 return pre

    -- called on back edges. pops the boundary stack while the top
    -- vertex has a larger preorder number than p_v.
    popBoundary p_v = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         C pre scc (dropWhile ((>p_v).fst) bnd) pth pres sccs gs es_i es_o)

    -- called when exiting vertex v. if v is the bottom of a scc
    -- boundary, we add a new SCC, otherwise v is part of a larger scc
    -- being constructed and we continue.
    exit v = do newComponent <- ((v==).snd.head) <$> gets boundary
                when newComponent $ insertComponent v
                logState
                return newComponent

    insertComponent v = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         let gs' = IntMap.insert scc g_i gs
             sccs' = List.foldl' (\sccs x -> Map.insert x scc sccs) sccs curr
             scc' = scc + 1
             bnd' = tail bnd
             p_v = fst $ head bnd
             g_i = fromList (vertex <$> curr) <> fromList (uncurry edge.snd <$> es)
             (es,es_i') = span ((>=p_v).fst) es_i
             pth' = tail $ dropWhile (/=v) pth
             curr = v:takeWhile(/=v) pth
          in C pre scc' bnd' pth' pres sccs' gs' es_i' es_o)

    inedge uv = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         C pre scc bnd pth pres sccs gs (uv:es_i) es_o)

    outedge uv = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         C pre scc bnd pth pres sccs gs es_i (uv:es_o))

    hasPreorderId v = gets (Map.member v . preorders)
    preorderId    v = gets (Map.lookup v . preorders)
    hasComponent  v = gets (Map.member v . components)

condense :: Ord a => AdjacencyMap a -> StateSCC a -> AdjacencyMap (AdjacencyMap a)
condense g (C _ n _ _ _ assignment inner _ outer)
  | n == 1 = vertex g
  | otherwise = gmap (inner' IntMap.!) outer'
  where inner' = overlays . toList <$> inner
        outer' = overlays $ es ++ vs
        vs = vertex <$> [0..n-1]
        es = [ edge (sccid x) (sccid y) | (x,y) <- outer ]
        sccid v = assignment Map.! v

