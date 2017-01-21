module Bfs(
  bfs
) where

{- http://aleph.nz/post/search_in_haskell/ -}

import           Data.Hashable
import qualified Data.HashSet  as Set
import qualified Data.Sequence as Seq    
import           Data.List (unfoldr)

bfs :: (Hashable a, Eq a)
  => (a -> [a])
  -> a
  -> [(a,a)]
bfs adj start = bfs' adj seen queue
  where
    seen  = Set.singleton start
    queue = Seq.singleton (start, start)

bfs' :: (Hashable a, Eq a)
  => (a -> [a])
  -> Set.HashSet a
  -> Seq.Seq (a,a)
  -> [(a,a)]
bfs' adj seen queue = unfoldr (bfs_step adj) (seen, queue)

bfs_step :: (Hashable a, Eq a)
  => (a -> [a])
  -> (Set.HashSet a, Seq.Seq (a,a))
  -> Maybe ((a,a), (Set.HashSet a, Seq.Seq (a,a)))
bfs_step neighbours (seen, queue)
  | Seq.null queue = Nothing
  | otherwise      = Just ((current, parent), next)
    where
      ((current, parent) Seq.:< remaining) = Seq.viewl queue
      next = (seen', queue')
      seen' = Set.union seen descendents
      queue' = remaining Seq.>< (Seq.fromList . map (\x -> (x, current)) . Set.toList) descendents
      descendents = Set.difference (Set.fromList $ neighbours current) seen
