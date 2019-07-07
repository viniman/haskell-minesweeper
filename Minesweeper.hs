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


--data Closed = 1
--data Opened = 2
--data Marked = 3

-- data Board = Board [[Cell]]

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

{-initCells :: Int -> Int -> Int -> Int -> [[Cell isMine stateCell countNeighborhoodMines]]
initCells m n numMines numCells = [[Cell False Closed 0]]-}


cellToChar :: Cell -> Char
cellToChar (Cell _ stateCell neighborsMines)
    | stateCell == Closed = '*'
    | stateCell == Marked = 'B'
    | stateCell == Opened = (intToDigit neighborsMines)

{-
cellToChar c b = intToDigit $ foldr increaseIfMine 0 (neighbors c b)
                 where increaseIfMine = \x acc -> if (isMine x)
                                                  then acc + 1
                                                  else acc
-}
showRow :: MatrixCell -> [Cell] -> String -> String
showRow b (c:cs) s = s ++ rowNumber ++ "  " ++ showCells ++ "\n"
                      where rowNumber = "1"--[row $ position c]
                            showCells = foldl showCell "" (c:cs)
                            showCell = \acc c -> acc ++ [cellToChar c] ++ " "

printBoardMatrix :: Board -> IO()
printBoardMatrix (Board matrixCell numRows numColumns _ _ _ _ _)
    | numRows == 4 = putStrLn "Estamos indo bem"
    | otherwise = putStrLn "bem mesmo"

instance Show MatrixCell where
    show (MatrixCell matrix) = showRows ++ "\n    " ++ enumerate ++ "\n\n"
                               where showRows = foldr (showRow $ MatrixCell matrix) "" matrix
                                     enumerate = foldr (\x acc -> x:' ':acc) "" letters
                                     letters = take numberOfColumns ['A'..]
                                     numberOfColumns = length $ head matrix

{-		putStrLn ("\n" ++ "                              " ++
		(show (tabela !! (0*n+0))) ++ " | " ++ (show (tabela !! (0*n+1))) ++ " | " ++ (show (tabela !! (0*n+2))) ++
		"\n                              ---------------\n" ++ "                              " ++
		(show (tabela !! (1*n+0))) ++ " | " ++ (show (tabela !! (1*n+1))) ++ " | " ++ (show (tabela !! (1*n+2))) ++
		"\n                              ---------------\n" ++ "                              " ++
		(show (tabela !! (2*n+0))) ++ " | " ++ (show (tabela !! (2*n+1))) ++ " | " ++ (show (tabela !! (2*n+2))) ++
		"\n")-}

{-processInput :: String -> Board -> String--Board
processInput ('+':j:i:"") b = "Marcar i j"--checkCellOnBoard b (Position i j)
processInput ('-':j:i:"") b = "Desmarcar i j"uncheckCellOnBoard b (Position i j)
processInput (j:i:"") b = "abrir i j"openCellOnBoard b (Position i j)
processInput _ b = b
-}

--test
--initBoardMinesweeper :: Int -> Int -> Int -> Board
--initBoardMinesweeper m n nMines = l
--            where l = Board [[False]] [[Closed]] [[0]] m n InGame nMines 0 0 -- Cell --[[State Closed]] 4 4

test :: Board -> IO() --[[Bool]] -> [[StateCell]] -> [[Int]] -> Int -> Int -> StateGame -> Int -> Int -> Int -> IO()
test (Board matrixCell nRows nColumns _ _ _ _ _)
    | nRows == 4 = putStrLn "OLA"

{-
printBoardMatrix :: Board -> IO()
printBoardMatrix b = let putStrLn "bem mesmo"--s neighbors numRows numColumns _ _ _ _
    --| numRows == 4 = putStrLn "Estamos indo bem"
    --| otherwise = putStrLn "bem mesmo"

-}

{-
import Data.Char (intToDigit)

cellToChar :: Cell -> Board -> Char
cellToChar (Untouched _ _) _ = '*'
cellToChar (Checked _ _) _ = 'B'
cellToChar (Opened _ True) _ = 'B'
cellToChar c b = intToDigit $ foldr increaseIfMine 0 (neighbors c b)
                 where increaseIfMine = \x acc -> if (isMine x)
                                                  then acc + 1
                                                  else acc

showRow :: Board -> [Cell] -> String -> String
showRow b (c:cs) s = s ++ rowNumber ++ "  " ++ showCells ++ "\n"
                      where rowNumber = [row $ position c]
                            showCells = foldl showCell "" (c:cs)
                            showCell = \acc c -> acc ++ [cellToChar c b] ++ " "

instance Show Board where
  show (Board xss) = showRows ++ "\n   " ++ enumerate ++ "\n\n"
                     where showRows = foldr (showRow $ Board xss) "" xss
                           enumerate = foldr (\x acc -> x:' ':acc) "" letters
                           letters = take numberOfColumns ['A'..]
                           numberOfColumns = length $ head xss
-}

{-
printBoardMatrix :: Board -> IO()
printBoardMatrix (BoardCell [[State Closed]] rows cols)--boardMinesweeperGame
    | rows == 4 = putStrLn "Estamos indo bem"



surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

printTabuleiro :: Tabela -> Int -> Int -> Int -> Int -> IO ()
printTabuleiroAux tabela m n i j
    | j == n = putStrLn (tabela !! (0*n+0))
-}



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
    putStrLn "-------------------------------- Campo Minado --------------------------------"
    putStrLn "\nInstrucoes de jogo:"
    putStrLn "Para marcar como mina a linha 1 coluna 0, informe \'+10\'"
    putStrLn "Para desmarcar como mina a linha 2 coluna 2, informe \'-22\'"
    putStrLn "Para abrir posição da linha 2 coluna 0, informe \'20\'"
    putStr "Informe o tamanho do tabuleiro: "
    --test "ola"
    let m = 4
    let n = 4
    let numCells = 4*4
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
    --line <- getString "digite seu nome "
    --putStrLn line
    --if(line == "carlos") then do
    --boardGame <- Board


    let boardGame = initBoardMinesweeper m n 6  --round m*n*0.4 --$ truncate (m*n*0.4)
    print boardGame
    -- putStrLn $ show boardGame
    test boardGame
    return ()

testaStringIgual :: String -> String -> Bool
testaStringIgual str1 str2
    | (str1 == str2) = True
    | otherwise = False

--IOToString :: IO String ->String
--IOToString str

{-test1 :: String -> IO Char
test1 dados = do
    op <- getChar --sizeBoard
    test op
-}

{-
test :: String-- -> Char
test dados
    putStr dados
--test str
    --| str == "fechado" = 's'
    --| otherwise = 'n'

-}

pos :: Int -> Int -> Pos
pos i j = (Pos i j)

getCell :: MatrixCell -> Int -> Int -> Cell
getCell (MatrixCell xss) i j = (xss !! i) !! j

getMatrixCells :: Board -> MatrixCell
getMatrixCells (Board matrixCell _ _ _ _ _ _ _) = matrixCell



{-
(Pos (i j)) = if i >= 0 && i < numberOfRows &&
                                       j >= 0 && j < numberOfColumns
                                       then (xss !! i') !! j'
                                    where numberOfRows = length xss
                                          numberOfColumns = length $ xss !! i'
                                          -}              