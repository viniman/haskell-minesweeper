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

-- data Cell = IsBomb Bool | State StateCell | CountNeighborhoodMines Int deriving(Show)--deriving(Show, Eq)

-- data Board = BoardCell [[Cell]] Int Int | StateBoard StateGame | NumBombs Int | OpenedCells Int | MarkedPositions Int deriving(Show)

data MatrixCell = MatrixCell [[Cell]] --deriving(Show)

-- record syntax
data Cell = Cell {isMine :: Bool, stateCell :: StateCell, countNeighborhoodMines :: Int} deriving(Show, Eq)

data Board = Board {matrixCell :: MatrixCell, nRows :: Int, nColumns :: Int, stateGame :: StateGame, sizeBoard :: Int, numMines :: Int, openedCells :: Int, markedPositions :: Int} deriving(Show)

--data Board = Board {isMine :: [[Bool]], stateCell :: [[StateCell]], neighborsMines :: [[Int]], nRows :: Int, nColumns :: Int, stateGame :: StateGame, numMines :: Int, openedCells :: Int, markedPositions :: Int} deriving(Show)


testBool :: Bool -> String
testBool booleano
    | booleano = "Verdadeiro"
    | otherwise = "Falso"

testStateCell :: StateCell -> String
testStateCell chave
    | chave == Closed = "Fechado"
    | chave == Opened = "Aberto"
    | otherwise = "Marcado"



getString :: String -> IO String
getString str = do
            putStr str
            res <- getLine
            return res


initBoardMinesweeper :: Int -> Int -> Int -> Board
initBoardMinesweeper m n nMines = board
                where board = Board cells m n InGame (m*n) nMines 0 0 --(truncate (m * n)) (truncate (m * n*0.4)) 0 0 -- Cell --[[State Closed]] 4 4
                      cells = initCells m n nMines
                --sizeBoard = m * n
                --nMines = truncate (sizeBoard  0.4)
                --cells = [[Cell False Closed 0]]
                {-
                appendRow i acc = (foldr (appendCell i) [] [0 .. cols-1]) : acc
                  appendCell i j acc = (newCell i j) : acc
                newCell i j = Untouched (newCellPosition i j) (isMine i j)
                newCellPosition i j = Position (rowToChar i) (columnToChar j)
                isMine i j = member (i*cols + j) minePositions
                minePositions = generateNRandomNumbers (0, totalCells - 1) (min (max mines 1) maxMines) g
                sizeBoard = m * n
                maxMines = truncate (sizeBoard  0.4)-}

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
cellToChar (Cell _ stateCell neighborsMines)
    | stateCell == Closed = '*'
    | stateCell == Marked = 'B'
    | stateCell == Opened = (intToDigit neighborsMines)


showRow :: MatrixCell -> [Cell] -> String -> String
showRow b (c:cs) s = s ++ rowNumber ++ "  " ++ showCells ++ "\n"
                      where rowNumber = "\n1"--[row $ position c]
                            showCells = foldl showCell "" (c:cs)
                            showCell = \acc c -> acc ++ [cellToChar c] ++ " "

{-printBoardMatrix :: Board -> IO()
printBoardMatrix (Board matrixCell numRows numColumns _ _ _ _ _)
    | numRows == 4 = putStrLn "Estamos indo bem"
    | otherwise = putStrLn "bem mesmo"-}

instance Show MatrixCell where
    show (MatrixCell matrix) = showRows ++ "\n    " ++ enumerate ++ "\n\n"
                               where showRows = foldr (showRow $ MatrixCell matrix) "" matrix
                                     enumerate = foldr (\x acc -> x:' ':acc) "" letters
                                     letters = take numberOfColumns ['A'..]
                                     numberOfColumns = length $ head matrix


{-
test :: Board -> IO() --[[Bool]] -> [[StateCell]] -> [[Int]] -> Int -> Int -> StateGame -> Int -> Int -> Int -> IO()
test (Board matrixCell nRows nColumns _ _ _ _ _)
    | nRows == 4 = putStrLn "OLA"-}



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

    m <- getString "Infome o numero de linhas do jogo: "
    n <- getString "Infome o numero de colunas do jogo: "
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
    --test "ola"
    --let m = 4
    --let n = 4
    --let numCells = 4*4

    --runGame boardGame


    --let numMines = numMinesCalculate Easy numCells
    --test1 "ola"
    --op <- getChar --sizeBoard
    --test op
    -- BOARD INIT
    --getChar -- descarta o Enter
    --executarOpcao dados op -> init game
    --dowhile
    --testStateCell Closed
    --runGame
    
    --if(line == "carlos") then do
    --boardGame <- Board


    --let boardGame = initBoardMinesweeper m n 6  --round m*n*0.4 --$ truncate (m*n*0.4)
    --print boardGame
    -- putStrLn $ show boardGame
    --test boardGame
    --putStrLn show boardGame
    runGame boardGame
    return ()


runGame :: Board -> IO()
runGame boardGame = do
    inputCommand <- getString "Informe o comando da sua jogada: "
    --putStrLn inputCommand
    let newBoardGame = makeCommandInBoard inputCommand boardGame
    let stateGame = getStateGame newBoardGame
    if(stateGame == InGame)
    then putStrLn "Em jogo"
    else return()
    putStrLn $ show newBoardGame
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



openCell :: Board -> Int -> Int -> Board
openCell b _ _ = b

markCell :: Board -> Int -> Int -> Board
markCell b _ _ = b

unmarkCell :: Board -> Int -> Int -> Board
unmarkCell b _ _ = b

makeCommandInBoard :: String -> Board -> Board
makeCommandInBoard ('+':j:i:"") boardGame = markCell boardGame (read [i]) (read [j])
makeCommandInBoard ('-':j:i:"") boardGame = unmarkCell boardGame (read [i]) (read [j])
makeCommandInBoard (j:i:"") boardGame = openCell boardGame (read [i]) (read [j])
makeCommandInBoard _ boardGame = boardGame

getStateGame :: Board -> StateGame
getStateGame (Board _ _ _ stateGame _ _ _ _) = stateGame

pos :: Int -> Int -> Pos
pos i j = (Pos i j)

getCell :: MatrixCell -> Int -> Int -> Cell
getCell (MatrixCell xss) i j = (xss !! i) !! j

getMatrixCells :: Board -> MatrixCell
getMatrixCells (Board matrixCell _ _ _ _ _ _ _) = matrixCell