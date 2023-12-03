module Main (main) where

import Game
import qualified Render
import Data.Char
import Data.Maybe
import Options.Applicative

data Mode = REPL | GUI
data CmdArgs = MkArgs { mode :: Mode, rowCount :: Int, columnCount :: Int }

defaultRows :: Int
defaultRows = 8

defaultColumns :: Int
defaultColumns = 8

mkArgs :: Maybe Mode -> Maybe Int -> Maybe Int -> CmdArgs
mkArgs m r c = MkArgs { mode = m', rowCount = r', columnCount = c' }
    where m' = fromMaybe REPL m
          r'  = fromMaybe defaultRows r
          c'  = fromMaybe defaultColumns c

isInt :: String -> Bool
isInt = all isDigit

getIntLoop :: IO Int
getIntLoop = do
    strLine <- getLine
    if (isInt strLine) then
        return (read strLine :: Int)
    else
        putStrLn "Please enter a valid integer" >>
        getIntLoop

gameLoop :: Board -> Player -> IO ()
gameLoop b p = do
    putStrLn $ (show p) ++ "'s turn"
    putStrLn (show b)
    let cols = numCols b
    putStrLn (replicate cols '-')
    putStrLn (concatMap show [0..(cols - 1)])
    putStrLn $ "Enter a column (from 0 to " ++ (show (cols - 1)) ++ ")"
    idx <- getIntLoop
    case dropCounter b idx p of
        Just b' ->
            case checkWin b' of
                Just winner ->
                    putStrLn ((show winner) ++ " wins!") >>
                    putStrLn (show b')
                Nothing ->
                    gameLoop b' (togglePlayer p)
        Nothing ->
            putStrLn "Invalid move" >>
            gameLoop b p

{- Args:
 - --text: Runs REPL
 - --gui: Runs GUI renderer
 - --rows n: Sets row count to n
 - --columns n: Sets column count to n -}
rowsArg :: Parser (Maybe Int)
rowsArg = optional $ option auto
  (  long "rows"
  <> metavar "ROWS"
  <> help "Number of rows for the board" )

columnsArg :: Parser (Maybe Int)
columnsArg = optional $ option auto
  (  long "columns"
  <> metavar "COLUMNS"
  <> help "Number of columns for the board" )

gui :: Parser Mode
gui = flag' GUI
  (  long "gui"
  <> help "Render using GUI renderer" )

repl :: Parser Mode
repl = flag' REPL
  (  long "text"
  <> help "Render using read-eval-print-loop" )

modeArg :: Parser (Maybe Mode)
modeArg = optional (repl <|> gui)

opts :: Parser CmdArgs
opts = mkArgs <$> modeArg <*> rowsArg <*> columnsArg

optsInfo :: ParserInfo CmdArgs
optsInfo = info opts (
        fullDesc
            <> progDesc "Runs Connect 4"
            <> header "Connect 4"
    )

main :: IO ()
main = do
    args <- execParser optsInfo
    let b = emptyBoard (rowCount args) (columnCount args)
    case (mode args) of
          REPL -> gameLoop b Red
          GUI -> Render.gameLoop (rowCount args) (columnCount args)

