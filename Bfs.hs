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
  -> [a]
bfs adj start = bfs' adj seen queue
  where
    seen  = Set.singleton start
    queue = Seq.singleton start

bfs' :: (Hashable a, Eq a)
  => (a -> [a])
  -> Set.HashSet a
  -> Seq.Seq a
  -> [a]
bfs' adj seen queue = unfoldr (bfs_step adj) (seen, queue)

bfs_step :: (Hashable a, Eq a)
  => (a -> [a])
  -> (Set.HashSet a, Seq.Seq a)
  -> Maybe (a, (Set.HashSet a, Seq.Seq a))
bfs_step neighbours (seen, queue)
  | Seq.null queue = Nothing
  | otherwise      = Just (current, next)
    where
      (current Seq.:< remaining) = Seq.viewl queue
      next = (seen', queue')
      seen' = Set.union seen descendents
      queue' = remaining Seq.>< (Seq.fromList . Set.toList) descendents
      descendents = Set.difference (Set.fromList $ neighbours current) seen
