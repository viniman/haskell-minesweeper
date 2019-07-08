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

import Graphics.UI.Gtk -- gtk2hs
import Data.IORef -- gtk2hs

import Data.List

import Foreign.Marshal.Unsafe

import System.Random
import Data.Array.IO
import Control.Monad

--import System.Random (RandomGen)
import Data.Char (intToDigit, isDigit)
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
-- https://stackoverflow.com/questions/2468410/convert-string-to-integer-float-in-haskell
-- https://pt.wikibooks.org/wiki/Haskell/Estruturas_de_controle
-- http://haskell.tailorfontela.com.br/higher-order-functions
-- https://www.schoolofhaskell.com/user/CKoenig/fold,%20accumulate
-- https://www.programming-idioms.org/idiom/55/convert-integer-to-string/955/haskell
-- https://stackoverflow.com/questions/49464632/convert-string-of-digits-to-int-in-haskell
-- http://www.muitovar.com/gtk2hs/chap6-1.html


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
data Cell = Cell {isMine :: Bool, stateCell :: StateCell, countNeighborhoodMines :: Int, rowNumber :: Int} deriving(Show, Eq)

data Board = Board {matrixCell :: MatrixCell, nRows :: Int, nColumns :: Int, stateGame :: StateGame, sizeBoard :: Int, numMines :: Int, openedCells :: Int, markedPositions :: Int} deriving(Show)

data Matrix a = Matrix [[a]] deriving (Eq)

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
initCells m n numMines = MatrixCell $ foldr appendRow [] [1..m]
                                  where appendRow i acc = (foldr (appendCell i) [] [0..n-1]) : acc
                                        appendCell i j acc = (newCell i j) : acc
                                        --newCell i j = Cell False Closed 0
                                        newCell i j = Cell (isMine i j) Closed (neighborsMines i j) i
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
cellToChar (Cell isMine stateCell neighborsMines _)
    | stateCell == Closed = '*'
    | stateCell == Marked = 'B'
    | stateCell == Opened && isMine = 'B'
    | stateCell == Opened = (intToDigit neighborsMines)


showRow :: MatrixCell -> [Cell] -> String -> String
showRow matrix (c:cs) s = s ++ show (rowNumber c) ++ "  " ++ showCells ++ "\n"
                      where --rowNumber = "" ++ (rowNumber c) --[row $ position c]
                            showCells = foldl showCell "" (c:cs)
                            showCell = \acc c -> acc ++ [cellToChar c] ++ " "

{-printBoardMatrix :: Board -> IO()
printBoardMatrix (Board matrixCell numRows numColumns _ _ _ _ _)
    | numRows == 4 = putStrLn "Estamos indo bem"
    | otherwise = putStrLn "bem mesmo"-}

instance Show MatrixCell where
    show (MatrixCell matrix) = showRows ++ "   " ++ enumerate ++ "\n\n"
                               where showRows = foldr (showRow $ MatrixCell matrix) "" matrix
                                     enumerate = foldr (\x acc -> x:' ':acc) "\n" letters
                                     letters = take numberOfColumns ['A'..]
                                     numberOfColumns = length $ head matrix
                                     --numbers = take numberOfRows ['1'..]
                                     --numberOfRows = length matrix
                                     --let numCol = 0
                                     --numFirst = if(numFirst == "0") then "0" else sumChar numCol "1" --if(numFirst == "0")
                                     --numCol = numFirst
                                     --let numFirst = sumChar numFirst "1"
                                     



incrementCharNumb :: [Char] -> [Char]
incrementCharNumb "0" = "1"
incrementCharNumb number = sumChar number "1"

{-
scanChar :: Char -> Int
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

scanString :: String -> Int
scanString x = if all isDigit x
                then read x :: Int
                else 0           
-}

sumChar :: [Char] -> [Char] -> [Char]
sumChar str1 str2 = show ((read str1::Int) + (read str2::Int))



{---type Pos = (Int,Int)
data Pos = Pos { row :: Int, column :: Int }

pos :: Int -> Int -> Pos
pos i j = (Pos i j)

instance Show a => Show (Matrix a)
  where
    show (Matrix a) = intercalate "\n" $ map (intercalate " " . map show) a
-}

main :: IO()
main = do
    putStrLn "CAMPO MINADO"
    putStr "\ESC[2J"--  limpa a tela (dando espaço)

    putStrLn "---------------- Campo Minado -------------------- Campo Minado ----------------"
    putStrLn "Campo Minado ------------------------------------------------------ Campo Minado"
    putStrLn "--------------------------------- Campo Minado ---------------------------------"
    putStrLn "Campo Minado ------------------------------------------------------ Campo Minado"
    putStrLn "---------------- Campo Minado -------------------- Campo Minado ----------------\n"

    m <- getString "Informe o numero de linhas do jogo: "
    n <- getString "Informe o numero de colunas do jogo: "
    let numRows = read n::Int
    let numCols = read m::Int
    let boardGame = initBoardMinesweeper numRows numCols 6  --round m*n*0.4 --$ truncate (m*n*0.4)


    putStrLn "--------------------------------- Campo Minado ---------------------------------\n"
    putStrLn "\nAs posições que possuem * estão fechados, ou seja, ainda nao foram abertos."
    putStrLn "\nInstrucoes de jogo:"
    putStrLn "     - Para marcar como mina a linha 1 coluna 0, informe \'+10\'"
    putStrLn "     - Para desmarcar como mina a linha 2 coluna 2, informe \'-22\'"
    putStrLn "     - Para abrir posição da linha 2 coluna 0, informe \'20\'\n\n"
    putStrLn "--------------------------------- Campo Minado ---------------------------------\n"

    if(False)
    	then (do mainGraphicUI boardGame
    		     --main10
    		)
    	else (do 
    		putStrLn $ show (matrixCell boardGame)
    		runGame boardGame
    		)
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
markCellCommand boardGame i j = if ((markedPositions boardGame) < (numMines boardGame) && (stateCell $ getCell (matrixCell boardGame) i j) == Closed)
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
openCell (Cell isMine stateCell neighborsMines row)
    | stateCell == Closed = Cell isMine Opened neighborsMines row
    | otherwise = Cell isMine stateCell neighborsMines row

markCell :: Cell -> Cell
markCell (Cell isMine stateCell neighborsMines row)
    | stateCell == Closed = Cell isMine Marked neighborsMines row
    | otherwise = Cell isMine stateCell neighborsMines row

unmarkCell :: Cell -> Cell
unmarkCell (Cell isMine stateCell neighborsMines row)
    | stateCell == Marked = Cell isMine Closed neighborsMines row
    | otherwise = Cell isMine stateCell neighborsMines row


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



-- openAllMinesForShow

































----------------------------------------------------------------------
--          INTERFACE GRAFICA
----------------------------------------------------------------------
---- EH ISSO QUE EU PRECISO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



mainGraphicUI :: Board -> IO ()
mainGraphicUI boardGame = do
     initGUI
     window <- windowNew

     --let m = (nRows boardGame)*5

     set window [ windowTitle := "Campo Minado em Haskell", 
                  windowDefaultWidth := ((nRows boardGame)*50), windowDefaultHeight := ((nColumns boardGame)*50+60)]
     mb <- vBoxNew False 0
     containerAdd window mb

     info <- labelNew (Just "Press \"New\" for a random number")
     boxPackStart mb info PackNatural 7
     sep1 <- hSeparatorNew
     boxPackStart mb sep1 PackNatural 7
     
     scrwin <- scrolledWindowNew Nothing Nothing
     boxPackStart mb scrwin PackGrow 0

     table <- tableNew (nRows boardGame) (nColumns boardGame) True
     scrolledWindowAddWithViewport scrwin table

     buttonlist <- sequence (map numButton [1..(sizeBoard boardGame)])
     let places = cross [0..(nRows boardGame)-1] [0..(nColumns boardGame)-1]
     sequence_ (zipWith (attachButton table) buttonlist places)

     sep2 <- hSeparatorNew
     boxPackStart mb sep2 PackNatural 7
     hb <- hBoxNew False 0
     boxPackStart mb hb PackNatural 0
     play <- buttonNewFromStock stockNew
     quit <- buttonNewFromStock stockQuit
     boxPackStart hb play PackNatural 0
     boxPackEnd hb quit PackNatural 0
     
     randstore <- newIORef 50
     randomButton info randstore play

     sequence_ (map (actionButton info randstore) buttonlist)  

     widgetShowAll window
     onClicked quit (widgetDestroy window)
     onDestroy window mainQuit
     mainGUI

numButton :: Int -> IO Button
numButton n = do
        button <- buttonNewWithLabel (show n)
        return button

cross :: [Int] -> [Int] -> [(Int,Int)]
cross row col = do 
        x <- row
        y <- col
        return (x,y)

attachButton :: Table -> Button -> (Int,Int) -> IO ()
attachButton ta bu (x,y) = 
              tableAttachDefaults ta bu y (y+1) x (x+1)

actionButton :: ButtonClass b => Label -> IORef Int -> b -> IO (ConnectId b)
actionButton inf rst b = 
  onClicked b $ do label <- get b buttonLabel
                   let num = (read label):: Int
                   rand <- readIORef rst
                   case compare num rand of
                     GT -> do set inf [labelLabel :=  "Too High"]
                              widgetModifyFg inf StateNormal (Color 65535 0 0)
                     LT -> do set inf [labelLabel := "Too Low"]
                              widgetModifyFg inf StateNormal (Color 65535 0 0)
                     EQ -> do set inf [labelLabel := "Correct"]
                              widgetModifyFg inf StateNormal (Color 0 35000 0)

randomButton :: ButtonClass b => Label -> IORef Int -> b -> IO (ConnectId b)
randomButton inf rst b = 
    onClicked b $ do rand <- randomRIO (1::Int, 100)
                     writeIORef rst rand  
                     set inf [labelLabel := "Ready"]
                     widgetModifyFg inf StateNormal (Color 0 0 65535)







main10 :: IO ()
main10 = do
     initGUI
     window <- windowNew
     set window [ windowTitle := "Guess a Number", 
                  windowDefaultWidth := 500, windowDefaultHeight := 500]
     mb <- vBoxNew False 0
     containerAdd window mb

     info <- labelNew (Just "Press \"New\" for a random number")
     boxPackStart mb info PackNatural 7
     sep1 <- hSeparatorNew
     boxPackStart mb sep1 PackNatural 7
     
     scrwin <- scrolledWindowNew Nothing Nothing
     boxPackStart mb scrwin PackGrow 0

     table <- tableNew 10 10 True
     scrolledWindowAddWithViewport scrwin table

     buttonlist <- sequence (map numButton [1..100])
     let places = cross [0..9] [0..9]
     sequence_ (zipWith (attachButton table) buttonlist places)

     sep2 <- hSeparatorNew
     boxPackStart mb sep2 PackNatural 7
     hb <- hBoxNew False 0
     boxPackStart mb hb PackNatural 0
     play <- buttonNewFromStock stockNew
     quit <- buttonNewFromStock stockQuit
     boxPackStart hb play PackNatural 0
     boxPackEnd hb quit PackNatural 0
     
     randstore <- newIORef 50
     randomButton info randstore play

     sequence_ (map (actionButton info randstore) buttonlist)  

     widgetShowAll window
     onClicked quit (widgetDestroy window)
     onDestroy window mainQuit
     mainGUI