{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Render (module Render) where

import Data.Char (digitToInt)
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Up))
import Graphics.Gloss.Interface.IO.Interact (interactIO)

{- Viewport dimensions -}
windowWidth :: Float
windowWidth = 1024.0

windowHeight :: Float
windowHeight = 768.0

-- chunked
-- Q6 (Rendering the board)
render :: Board -> Picture
render b =
  let labeledPlayers = getLabeledPlayers b
      gridPicture = renderGrid labeledPlayers
      counterPicture = renderCounter labeledPlayers
      picture = pictures [gridPicture, counterPicture]
      res = Scale 0.1 0.1 picture
   in res

type CoordinatedPlayer = (Coordinate, Maybe Player)

getLabeledPlayers :: Board -> [CoordinatedPlayer]
getLabeledPlayers b =
  addCoordinate (getRows b) (numRows b - 1) (numCols b - 1)

type Coordinate = (Int, Int)

addCoordinate :: [[Maybe Player]] -> Int -> Int -> [(Coordinate, Maybe Player)]
addCoordinate rows r c =
  let coordinates = [[(r1, c1) | c1 <- [0 .. c]] | r1 <- [0 .. r]]
      res = zip (concat coordinates) (concat rows)
   in res

renderCounter :: [CoordinatedPlayer] -> Picture
renderCounter labelPlayers = pictures $ map f labelPlayers
  where
    f ((r, c), player) = toCounter r c player

toCounter :: Int -> Int -> Maybe Player -> Picture
toCounter r c (Just Red) = convertCoordinate r c redCircle
toCounter r c (Just Yellow) = convertCoordinate r c yellowCircle
toCounter _ _ Nothing = blank

renderGrid :: [(Coordinate, Maybe Player)] -> Picture
renderGrid grids = pictures $ map f grids
  where
    f ((r, c), _) = toGrid r c

toGrid :: Int -> Int -> Picture
toGrid r c =
  convertCoordinate r c grid

grid :: Picture
grid = color blue $ lineLoop $ rectanglePath edgeLength edgeLength

redCircle :: Picture
redCircle = color red $ circleSolid radius

yellowCircle :: Picture
yellowCircle = color yellow $ circleSolid radius

convertCoordinate :: Int -> Int -> Picture -> Picture
convertCoordinate r c = translate (fromIntegral c * edgeLength) (fromIntegral r * edgeLength)

edgeLength :: Float
edgeLength = 500

radius :: Float
radius = 200

-- Q7 (Game loop)
-- Example of how to use displayIO (similar to interactIO)
testPicture :: IO ()
testPicture =
  displayIO
    (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
    white
    (return (translate 100 100 $ color blue $ lineLoop $ rectanglePath 50 50))
    (const $ return ())

draw :: Picture -> IO ()
draw picture =
  displayIO
    (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
    white
    (return picture)
    (const $ return ())

-- Fill in gameLoop using interactIO to handle user input and display the game board.
gameLoop :: Int -> Int -> IO ()
gameLoop rows cols =
  let b = emptyBoard rows cols
   in loop b Red Nothing

loop :: Board -> Player -> Maybe Player -> IO ()
loop b p winner =
  interactIO
    (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
    white
    (b, p, winner)
    (\(b1, _, winner1) -> return $ pictures [render b1, renderWinner winner1])
    processEvent
    (const $ return ())

type World = (Board, Player, Maybe Player)

processEvent :: Event -> World -> IO World
processEvent event world@(b, p, winner) =
  case winner of
    Nothing -> case event of
      EventKey (Char key) Up _ _ ->
        return $ case dropCounter b (digitToInt key) p of
          Just newB -> case checkWin newB of
            Just winner -> (newB, togglePlayer p, Just winner)
            Nothing -> (newB, togglePlayer p, Nothing)
          Nothing -> world
      _ -> return world
    Just _ -> case event of
      EventKey (Char _) Up _ _ ->
        return (cleanBoard, togglePlayer p, Nothing)
        where
          cleanBoard = emptyBoard (numRows b) (numCols b)
      _ -> return world

renderWinner :: Maybe Player -> Picture
renderWinner Nothing = blank
renderWinner (Just winner) = scale 0.2 0.2 $ text $ "Winner is " ++ show winner

-- displayIO
--   (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
--   white
--   (return $)
--   (const $ return ())
