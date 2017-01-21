module State(
  Block(..), Board, State, nextState
) where

data Block = H Int Int | V Int Int deriving (Show)
type Board = [Block]
type State = [Int]

nextState :: Board -> State -> [State]
nextState board state = freeStates state (freePositions board state)

-- [1,2,0,0,3,0,4,0,2,0,3]
-- [[],[1,3],[],[],[],[],[],[],[],[],[]]
-- [[1,1,0,0,3,0,4,0,2,0,3],[1,3,0,0,3,0,4,0,2,0,3]]

freeStates :: State -> [[Int]] -> [State]
freeStates s xss =
  let f1 (i, xs) = map (\x -> take i s ++ [x] ++ drop (i+1) s) xs
  in  f1 =<< zip [0..] xss

freePositions :: Board -> State -> [[Int]]
freePositions board state =
  let f1 (H w r, p) = f2 r (reverse [0 .. p-1]) ++ map (\x -> x + 1 - w) (f2 r [p + w .. 5])
      f1 (V h c, p) = f3 c (reverse [0 .. p-1]) ++ map (\x -> x + 1 - h) (f3 c [p + h .. 5])
      f2 r = takeWhile (\p -> isCellFree board state r p)
      f3 c = takeWhile (\p -> isCellFree board state p c)
  in  map f1 $ board `zip` state

isCellFree :: Board -> State -> Int -> Int -> Bool
isCellFree board state row col =
  let outside a x d = x < a || x > a + d - 1
      process (H w r, p) result | r == row = result && outside p col w
      process (V h c, p) result | c == col = result && outside p row h
      process _          result            = result
  in foldr process True (board `zip` state)
