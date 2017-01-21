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
    H 2 1
  , H 2 2
  , H 2 3
  , H 2 5
  , V 2 0
  , V 3 2
  , V 3 3
  , V 2 4
  , V 3 5
  ]
startState2 = [0,3,4,1,2,0,3,4,0]

board3 = [
    H 2 0
  , H 2 2
  , H 2 3
  , H 2 4
  , H 2 5
  , V 2 0
  , V 2 1
  , V 3 2
  , V 2 3
  , V 2 3
  , V 3 5
  ]
startState3 = [1,3,3,4,0,1,3,2,0,4,0]

allStates1 = bfs (nextState board1) startState1 -- length allStates1 = 1799
allStates2 = bfs (nextState board2) startState2 -- length allStates2 = 8859
allStates3 = bfs (nextState board3) startState3 -- length allStates3 = 15651

bt1 = bfsBacktrack (nextState board1) (\x -> x !! 1 == 4) startState1
bt2 = bfsBacktrack (nextState board2) (\x -> x !! 1 == 4) startState2
bt3 = bfsBacktrack (nextState board3) (\x -> x !! 1 == 4) startState3

display :: [State] -> IO ()
display = sequence_ . fmap (putStrLn . show)

-- display bt3
