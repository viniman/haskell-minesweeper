-- Trabalho de Linguagem de Proramação
-- Campo Minado em Haskell
-- Vinícius Carlos de Oliveira
-- 201635025


{-
import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function
import Data.Array
-}
import Data.List

import Foreign.Marshal.Unsafe

import System.Random
import Data.Array.IO
import Control.Monad

--import System.Random (RandomGen)
import Data.Char (intToDigit)
--import Data.Set (Set, member)

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

ioToA :: IO [a] -> [a]
ioToA xs = unsafeLocalState xs

member :: Eq a => a -> [a] -> Bool
member x = maybe False (const True) . find (== x)

-- References
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- https://hackage.haskell.org/package/CheatSheet-1.11/src/CheatSheet.pdf
-- https://stackoverflow.com/questions/6000511/better-way-to-define-an-enum-in-haskell
-- https://stackoverflow.com/questions/4846974/haskell-check-if-int-is-in-a-list-of-ints
-- https://hoogle.haskell.org/?hoogle=IO%20a%20-%3E%20a
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Foreign-Marshal-Unsafe.html#v:unsafeLocalState
-- https://wiki.haskell.org/Random_shuffle
-- https://wiki.haskell.org/Converting_numbers
-- https://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:truncate
-- https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign
-- https://stackoverflow.com/questions/8529814/get-a-sublist-in-haskell
-- https://stackoverflow.com/questions/22151625/using-defined-data-types-in-other-data-types
-- https://stackoverflow.com/questions/12860798/haskell-how-to-create-a-matrix
-- http://openhaskell.com/lectures/adts.html
-- http://zvon.org/other/haskell/Outputchar/intToDigit_f.html


data Dificulty = Easy | Medium | Hard deriving (Eq, Ord, Show, Read, Bounded, Enum)  

{-numMinesCalculate :: Dificulty -> Int -> Num
numMinesCalculate dificulty numCells
    | dificulty == Easy = (truncate (numCells*10*0.2))
    | dificulty == Medium = (truncate (numCells*10*0.4))
    | dificulty == Hard = (truncate (numCells*10*0.55))
-}

data StateCell = Closed | Opened | Marked deriving(Enum, Eq, Show)

data StateGame = Win | GameOver | InGame deriving(Enum, Eq, Show)

data MatrixCell = MatrixCell [[Cell]] --deriving(Show)

-- record syntax
data Cell = Cell {isMine :: Bool, stateCell :: StateCell, countNeighborhoodMines :: Int} deriving(Show, Eq)

data Board = Board {matrixCell :: MatrixCell, nRows :: Int, nColumns :: Int, stateGame :: StateGame, sizeBoard :: Int, numMines :: Int, openedCells :: Int, markedPositions :: Int} deriving(Show)



getString :: String -> IO String
getString str = do
            putStr str
            res <- getLine
            return res


initBoardMinesweeper :: Int -> Int -> Int -> Board
initBoardMinesweeper m n nMines = board
                where board = Board cells m n InGame (m*n) nMines 0 0 --(truncate (m * n)) (truncate (m * n*0.4)) 0 0 -- Cell --[[State Closed]] 4 4
                      cells = initCells m n nMines


initCells :: Int -> Int -> Int -> MatrixCell
initCells m n numMines = MatrixCell $ foldr appendRow [] [0..m-1]
                                  where appendRow i acc = (foldr (appendCell i) [] [0..n-1]) : acc
                                        appendCell i j acc = (newCell i j) : acc
                                        --newCell i j = Cell False Closed 0
                                        newCell i j = Cell (isMine i j) Closed $ neighborsMines i j
                                        neighborsMines i j = numNeighborhoodMines (i*n+j) mines n 
                                        isMine i j = member (i*n+j) mines
                                        mines = take numMines $ ioToA $ shuffle [0..(m*n-1)]

numNeighborhoodMines :: Int -> [Int] -> Int -> Int
numNeighborhoodMines idCell listOfMines numColumns = contMines idCell listOfMines numColumns
                                          where  
                                              contMines id mines cols = isMineFromList (idCell+1) listOfMines
                                                                 + isMineFromList (idCell-1) listOfMines
                                                                 + isMineFromList (idCell+cols) listOfMines
                                                                 + isMineFromList (idCell-cols) listOfMines


isMineFromList :: Int -> [Int] -> Int
isMineFromList idCell listOfMines
    | member idCell listOfMines = 1
    | otherwise = 0



cellToChar :: Cell -> Char
cellToChar (Cell isMine stateCell neighborsMines)
    | stateCell == Closed = '*'
    | stateCell == Marked = 'B'
    | stateCell == Opened && isMine = 'B'
    | stateCell == Opened = (intToDigit neighborsMines)


showRow :: MatrixCell -> [Cell] -> String -> String
showRow b (c:cs) s = s ++ rowNumber ++ "  " ++ showCells ++ "\n"
                      where rowNumber = "" ++ numCol --[row $ position c]
                            showCells = foldl showCell "" (c:cs)
                            showCell = \acc c -> acc ++ [cellToChar c] ++ " "
                            numCol = "1"

{-printBoardMatrix :: Board -> IO()
printBoardMatrix (Board matrixCell numRows numColumns _ _ _ _ _)
    | numRows == 4 = putStrLn "Estamos indo bem"
    | otherwise = putStrLn "bem mesmo"-}

instance Show MatrixCell where
    show (MatrixCell matrix) = showRows ++ "   " ++ enumerate ++ "\n\n"
                               where showRows = foldr (showRow $ MatrixCell matrix) "" matrix
                                     enumerate = foldr (\x acc -> x:' ':acc) "" letters
                                     letters = take numberOfColumns ['A'..]
                                     numberOfColumns = length $ head matrix


data Matrix a = Matrix [[a]]
              deriving (Eq)

--type Pos = (Int,Int)
data Pos = Pos { row :: Int, column :: Int }

instance Show a => Show (Matrix a)
  where
    show (Matrix a) = intercalate "\n" $ map (intercalate " " . map show) a

main :: IO()
main = do
    putStrLn "CAMPO MINADO"
    putStr "\ESC[2J"--  limpa a tela (dando espaço)

    putStrLn "----------------- Campo Minado -------------------- Campo Minado -----------------"
    putStrLn "Campo Minado -------------------------------------------------------- Campo Minado"
    putStrLn "---------------------------------- Campo Minado ----------------------------------"
    putStrLn "Campo Minado -------------------------------------------------------- Campo Minado"
    putStrLn "----------------- Campo Minado -------------------- Campo Minado -----------------\n"

    m <- getString "Informe o numero de linhas do jogo: "
    n <- getString "Informe o numero de colunas do jogo: "
    let numRows = read n::Int
    let numCols = read m::Int
    let boardGame = initBoardMinesweeper numRows numCols 6  --round m*n*0.4 --$ truncate (m*n*0.4)


    putStrLn "---------------------------------- Campo Minado ----------------------------------\n"
    putStrLn "\nAs posições que possuem * estão fechados, ou seja, ainda nao foram abertos."
    putStrLn "\nInstrucoes de jogo:"
    putStrLn "     - Para marcar como mina a linha 1 coluna 0, informe \'+10\'"
    putStrLn "     - Para desmarcar como mina a linha 2 coluna 2, informe \'-22\'"
    putStrLn "     - Para abrir posição da linha 2 coluna 0, informe \'20\'\n\n"
    putStrLn "---------------------------------- Campo Minado ----------------------------------\n"

    putStrLn $ show (matrixCell boardGame)
    runGame boardGame
    return ()


runGame :: Board -> IO()
runGame boardGame = do
    inputCommand <- getString "Informe o comando da sua jogada: "
    --putStrLn inputCommand
    let newBoardGame = winGameTest $ makeCommandInBoard inputCommand boardGame
    let stateGame = getStateGame newBoardGame
    if(stateGame == InGame)
    then (do putStrLn $ "Número de marcações feitas: " ++ show (markedPositions newBoardGame)
             putStrLn $ "Restante de marcações: " ++ show  ((numMines newBoardGame)-(markedPositions newBoardGame))
         )
    else return()
    putStrLn $ show (matrixCell newBoardGame)
    if(stateGame == GameOver)
        then (do
            putStrLn "Game Over! Não fique triste! Muitas vezes a vida possui revira-voltas."
            return()
            )
    else if (stateGame == Win)
        then (do
            putStrLn "Você venceu! Seja Feliz! Voce venceu um jogo! Vá viver."
            return()
            )
        else
            runGame newBoardGame



openCellCommand :: Board -> Int -> Int -> Board
openCellCommand boardGame i j = replaceCell (addOpenedCell (gameOverTest boardGame i j)) newCell i j
                            where newCell = openCell $ getCell (matrixCell boardGame) i j


markCellCommand :: Board -> Int -> Int -> Board
markCellCommand boardGame i j = if ((markedPositions boardGame) < (numMines boardGame))
	                            then replaceCell (addMarkPosition boardGame) newCell i j
	                            else boardGame
	                            where newCell = markCell $ getCell (matrixCell boardGame) i j

unmarkCellCommand :: Board -> Int -> Int -> Board
unmarkCellCommand boardGame i j = replaceCell (removeMarkPosition boardGame) newCell i j 
                            where newCell = unmarkCell $ getCell (matrixCell boardGame) i j

addMarkPosition :: Board -> Board
addMarkPosition (Board v1 v2 v3 v4 v5 v6 v7 v8) = Board v1 v2 v3 v4 v5 v6 v7 (v8+1)

removeMarkPosition :: Board -> Board
removeMarkPosition (Board v1 v2 v3 v4 v5 v6 v7 v8) = Board v1 v2 v3 v4 v5 v6 v7 (v8-1)

addOpenedCell :: Board -> Board
addOpenedCell (Board v1 v2 v3 v4 v5 v6 v7 v8) = Board v1 v2 v3 v4 v5 v6 (v7+1) v8


openCell :: Cell -> Cell
openCell (Cell isMine stateCell neighborsMines)
    | stateCell == Closed = Cell isMine Opened neighborsMines
    | otherwise = Cell isMine stateCell neighborsMines

markCell :: Cell -> Cell
markCell (Cell isMine stateCell neighborsMines)
    | stateCell == Closed = Cell isMine Marked neighborsMines
    | otherwise = Cell isMine stateCell neighborsMines

unmarkCell :: Cell -> Cell
unmarkCell (Cell isMine stateCell neighborsMines)
    | stateCell == Marked = Cell isMine Closed neighborsMines
    | otherwise = Cell isMine stateCell neighborsMines


makeCommandInBoard :: String -> Board -> Board
makeCommandInBoard ('+':j:i:"") boardGame = markCellCommand boardGame (read [i]) (read [j])
makeCommandInBoard ('-':j:i:"") boardGame = unmarkCellCommand boardGame (read [i]) (read [j])
makeCommandInBoard (j:i:"") boardGame = if(stateGame newBoardGame == GameOver )
	                                    then newBoardGame
	                                    else openCellCommand boardGame (read [i]) (read [j])
                                      where newBoardGame = (gameOverTest boardGame (read [i]) (read [j]))
makeCommandInBoard _ boardGame = boardGame

getStateGame :: Board -> StateGame
getStateGame (Board _ _ _ stateGame _ _ _ _) = stateGame

pos :: Int -> Int -> Pos
pos i j = (Pos i j)

getCell :: MatrixCell -> Int -> Int -> Cell
getCell (MatrixCell xss) i j = (xss !! i) !! j

getMatrixCells :: Board -> MatrixCell
getMatrixCells (Board matrixCell _ _ _ _ _ _ _) = matrixCell


replaceCellInRow :: [Cell] -> Cell -> Int -> [Cell]
replaceCellInRow cs c j = (take columnIndex cs) ++ (c : drop (columnIndex + 1) cs)
                        where columnIndex = j--charToColumn $ column (position c)

replaceCell :: Board -> Cell -> Int -> Int -> Board
replaceCell (Board (MatrixCell css) a b c d e f g) cell i j = Board (MatrixCell $ (take i css) ++ (newRow : (drop (i + 1) css))) a b c d e f g
                           where newRow = replaceCellInRow (css !! i) cell j


gameOverTest :: Board -> Int -> Int -> Board
gameOverTest (Board matrixCell m n stateGame a b c d) i j = if(isMine $ getCell matrixCell i j)
                             then (Board matrixCell m n GameOver a b c d)
                             else (Board matrixCell m n stateGame a b c d)

-- () retorno void
winGameTest :: Board -> Board
winGameTest (Board cells m n stateCell sizeBoard numMines openedCells markedPositions) = 
    if (openedCells+numMines == sizeBoard) 
    then Board cells m n Win sizeBoard numMines openedCells markedPositions
    else Board cells m n stateCell sizeBoard numMines openedCells markedPositions



-- openAllMines