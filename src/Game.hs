{- Game logic / win checking -}
module Game (module Game) where

import Data.List (find, intercalate)
import GHC.Base (join)

{- Board and counters definition -}
{- In our case, we represent a board as a function from column IDs to lists of tokens.
 - These are sparse and grow upwards. -}
 
{- Row indices again begin at the bottom left and grow upwards -}
type RowID = Int

type ColumnID = Int

type RowCount = Int

type ColCount = Int

data Player = Red | Yellow
  deriving (Eq)

data Board = MkBoard {board :: [[Player]], numRows :: Int, numCols :: Int}

{- Toggles the current player -}
togglePlayer :: Player -> Player
togglePlayer Red = Yellow
togglePlayer Yellow = Red

{- Board accessors / manipulation function -}

{- Q1(a): emptyBoard -}
emptyBoard :: RowCount -> ColCount -> Board
emptyBoard rs cs =
  let localBoard = map (const []) [1 .. cs]
   in MkBoard localBoard rs cs

-- >>>getCounter (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5) 1 0
-- Just Y
{- Q1(b): getCounter
 - Gets the counter at the given co-ordinates (or Nothing if there is no counter there).
 - Raises an error if co-ordinates are out-of-bounds. -}
getCounter :: Board -> RowID -> ColumnID -> Maybe Player
getCounter b r c =
  if r < 0 || r >= rowBound || c < 0 || c >= colBound
    then error "out of bounds"
    else
      let col = localBoard !! c
          counter =
            if r < length col
              then Just $ reverse col !! r
              else Nothing
       in counter
  where
    localBoard = board b
    rowBound = numRows b
    colBound = numCols b

-- >>>getRow (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5) 0
-- [Just R,Just R,Just R,Just R,Nothing]
{- Q1(c): getRow
 - Retrieves the list of counters on the given row -}
getRow :: Board -> RowID -> [Maybe Player]
getRow b r =
  if r < numRows b
    then map f (board b)
    else error $ "out of bounds " ++ (show $ numRows b) ++ (show r)
  where
    f :: [Player] -> Maybe Player
    f playerList =
      if r < length playerList
        then Just $ reverse playerList !! r
        else Nothing

-- >>>getColumn (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5) 0
-- [Nothing,Nothing,Nothing,Just Y,Just R]
{- Q1(d): getColumn
 - Retrieves the list of counters in the given column, from top-to-bottom -}
getColumn :: Board -> ColumnID -> [Maybe Player]
getColumn b c =
  if c < numCols b
    then
      let col = board b !! c
          diff = numCols b - length col
          playerList = map (const Nothing) [1 .. diff] ++ map Just col
       in playerList
    else error "out of bounds"

{- Q2: Show instance -}

{- Show instance for players -}
instance Show Player where
  show Red = "R"
  show Yellow = "Y"

-- >>>(MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5)
-- OOOOO
-- OOOOO
-- OYOOO
-- YYOOO
-- RRRRO
{- Instance -}
instance Show Board where
  show b =
    let len = numRows b
        rows = reverse $ map (getRow b) [0 .. len - 1]
        tmp = map (concatMap f) rows
        result = intercalate "\n" tmp
     in result
    where
      f :: Maybe Player -> String
      f (Just a) = show a
      f Nothing = "O"

{- Q3: Board update -}
-- >>> dropCounter (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5) 1 Red
-- Just OOOOO
-- OROOO
-- OYOOO
-- YYOOO
-- RRRRO
{- Drops a counter into the given column. If the move is legal, returns an updated
 - board. Otherwise returns Nothing. -}
dropCounter :: Board -> ColumnID -> Player -> Maybe Board
dropCounter b c p =
  if c < numCols b
    then
      let col = board b !! c
          result =
            if length col == numRows b
              then Nothing
              else Just $ MkBoard newBoard (numRows b) (numCols b)
            where
              oldBoard = board b
              newCol = p : col
              newBoard = take c oldBoard ++ [newCol] ++ drop (c + 1) oldBoard
       in result
    else Nothing

-- >>>getTLBRDiagonals (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5)
-- [[Nothing],[Nothing,Nothing],[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Just Y,Nothing,Just R],[Nothing,Just Y,Just R],[Just Y,Just R],[Just R]]

{- Q4: Diagonals -}
getTLBRDiagonals :: Board -> [[Maybe Player]]
getTLBRDiagonals b =
  let r = numRows b - 1
      c = numCols b - 1
      startPostions = getTLPositions r c
      positions = extendTLPath startPostions r c
      result = map (map (uncurry (getCounter b))) positions
   in result

getTLPositions :: Int -> Int -> [(Int, Int)]
getTLPositions r c =
  let topPosList = map (\x -> (r, x)) $ reverse [0 .. c]
      leftPosList = map (\x -> (x, 0)) $ reverse [0 .. r - 1]
      result = topPosList ++ leftPosList
   in result

extendTLPath :: [(Int, Int)] -> Int -> Int -> [[(Int, Int)]]
extendTLPath startPositions r c =
  map (f []) startPositions
  where
    f :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    f list (localR, localC)
      | localR < 0 || localR > r || localC < 0 || localC > c = reverse list
      | otherwise = f ((localR, localC) : list) (localR - 1, localC + 1)

-- >>>getBLTRDiagonals (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5)
-- [[Nothing],[Nothing,Nothing],[Nothing,Nothing,Nothing],[Just Y,Just Y,Nothing,Nothing],[Just R,Just Y,Nothing,Nothing,Nothing],[Just R,Nothing,Nothing,Nothing],[Just R,Nothing,Nothing],[Just R,Nothing],[Nothing]]

getBLTRDiagonals :: Board -> [[Maybe Player]]
getBLTRDiagonals b =
  let r = numRows b - 1
      c = numCols b - 1
      startPostions = getLBPositions r c
      positions = extendLBPath startPostions r c
      result = map (map (uncurry (getCounter b))) positions
   in result

getLBPositions :: Int -> Int -> [(Int, Int)]
getLBPositions r c =
  let leftPosList = map (\x -> (x, 0)) $ reverse [0 .. r]
      bottomPosList = map (\x -> (0, x)) [1 .. c]
      result = leftPosList ++ bottomPosList
   in result

extendLBPath :: [(Int, Int)] -> Int -> Int -> [[(Int, Int)]]
extendLBPath startPositions r c =
  map (f []) startPositions
  where
    f :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    f list (localR, localC)
      | localR < 0 || localR > r || localC < 0 || localC > c = reverse list
      | otherwise = f ((localR, localC) : list) (localR + 1, localC + 1)

{- Q5: Win checking -}
{- Checks if the given list has a subsequence of length 4, returning Just Player
 - if so, Nothing otherwise -}
hasFourInRow :: [Maybe Player] -> Maybe Player
hasFourInRow (Just Red : Just Red : Just Red : Just Red : _) = Just Red
hasFourInRow (Just Yellow : Just Yellow : Just Yellow : Just Yellow : _) = Just Yellow
hasFourInRow (_ : xs) = hasFourInRow xs
hasFourInRow _ = Nothing

{- Checks all rows, columns, and diagonals for any subsequences of length 4 -}
checkWin :: Board -> Maybe Player
checkWin b =
  let rows = getRows b
      cols = getCols b
      diagonals1 = getTLBRDiagonals b
      diagonals2 = getBLTRDiagonals b
      result = join $ find f $ map hasFourInRow $ rows ++ cols ++ diagonals1 ++ diagonals2
   in result
  where
    f (Just _) = True
    f Nothing = False

-- >>>getRows (MkBoard [[Yellow,Red],[Yellow,Yellow,Red],[Red],[Red],[]] 5 5)
-- [[Just R,Just R,Just R,Just R,Nothing],[Just Y,Just Y,Nothing,Nothing,Nothing],[Nothing,Just Y,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing]]
getRows :: Board -> [[Maybe Player]]
getRows b = map (getRow b) [0 .. numRows b - 1]

getCols :: Board -> [[Maybe Player]]
getCols b = map (getColumn b) [0 .. numCols b - 1]
