import State
import Bfs

board1 = [
    H 2 1
  , H 2 2
  , H 3 3
  , H 2 5
  , H 2 5
  , V 3 0
  , V 2 2
  , V 2 3
  , V 3 4
  , V 2 5
  , V 2 5
  ]
startState1 = [1,2,0,0,3,0,4,0,2,0,3]

board2 = [
    H 2 0
  , H 2 1
  , H 2 2
  , H 2 3
  , H 2 5
  , V 2 0
  , V 3 2
  , V 3 3
  , V 2 4
  , V 3 5
  ]
startState2 = [0,0,3,4,1,2,0,3,4,0]

allStates1 = bfs (nextState board1) startState1      -- length allStates1 = 1799
upToExit1 = takeWhile (\(x, _) -> x !! 1 /= 4) allStates1 -- length upToExit1  = 1721

allStates2 = bfs (nextState board2) startState2      -- length upToExit2 = 27462
upToExit2 = takeWhile (\(x, _) -> x !! 1 /= 4) allStates2 -- length upToExit2  = 3831


