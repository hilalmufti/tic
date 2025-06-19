{-# LANGUAGE ViewPatterns #-}

module Engine (Player(..), readMove, mark, GameState, mkGameState, validMove, emptyBoard, gameOver, winner, Winner(..), red, blue) where

import Control.Exception (assert)
import Data.List (intercalate)

-- utils

red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"

blue :: String -> String
blue s = "\x1b[34m" ++ s ++ "\x1b[0m"

sycle :: (Enum a, Bounded a, Eq a) => a -> a
sycle x
  | x == maxBound = minBound
  | otherwise = succ x

nrows :: [[a]] -> Int
nrows xss = length xss

ncols :: [[a]] -> Int
ncols [] = 0
ncols (xs:xss) = length xs

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

pad :: String -> String
pad s = " " ++ s ++ " "


-- player

data Player = X | O
  deriving (Bounded, Enum, Eq, Show)


-- cell

data Cell = Cell { playerCell :: Maybe Player }
  deriving (Eq)

instance Show Cell where
  show (playerCell -> Just X)  = blue "X"
  show (playerCell -> Just O)  = red "O"
  show (playerCell -> Nothing) = " "

mkCell :: Maybe Player -> Cell
mkCell p =  Cell p

isX :: Cell -> Bool
isX c = c == mkCell (Just X)

isO :: Cell -> Bool
isO c = c == mkCell (Just O)

isNothing :: Cell -> Bool
isNothing c = c == mkCell Nothing


-- row

data Row = Row
  { cellsRow      :: [Cell]
  , lengthRow     :: Int
  , ixsXRow       :: [Int]
  , ixsORow       :: [Int]
  , ixsNothingRow :: [Int]}

mkRow :: [Cell] -> Row
mkRow xs = assert (length ixsX + length ixsO + length ixsNothing == n) $
           Row xs n ixsX ixsO ixsNothing
  where
    n = length xs
    ixsX = [i | (i, x) <- zip [0..] xs, isX x]
    ixsO = [i | (i, x) <- zip [0..] xs, isO x]
    ixsNothing = [i | (i, x) <- zip [0..] xs, isNothing x]

setAtRow :: Row -> Int -> Cell -> Row
setAtRow (cellsRow -> cs) i c = mkRow (setAt cs i c)

lookupRow :: Row -> Int -> Cell
lookupRow (cellsRow -> cs) i = cs !! i

instance Show Row where
  show (cellsRow -> cs) = intercalate "|" $ map (pad . show) cs

myrow = mkRow [mkCell (Just X), mkCell Nothing, mkCell (Just O)]
row1  = mkRow [mkCell (Just O), mkCell (Just X), mkCell Nothing]
row2  = mkRow [mkCell Nothing, mkCell (Just O), mkCell (Just X)]


-- board

data Board = Board
  { rowsBoard       :: [Row]
  , nrowsBoard      :: Int
  , ncolsBoard      :: Int
  , ixsXBoard       :: [(Int, Int)]
  , ixsOBoard       :: [(Int, Int)]
  , ixsNothingBoard :: [(Int, Int)]}

instance Show Board where
  show b = intercalate sep rows
    where
      rs = rowsBoard b
      rows = (map show rs)
      ncols = 3 * (ncolsBoard b) + 2
      sep = "\n" <> replicate ncols '-' <> "\n"

mkBoard :: [Row] -> Board
mkBoard [] = Board [] 0 0 [] [] []
mkBoard rs = assert (length csX + length csO + length csN == nrows * ncols) $
             assert (all ((== ncols) . lengthRow) rs) $
             Board rs nrows ncols csX csO csN
  where
    nrows = length rs
    ncols = lengthRow (head rs)
    csX   = [(i, j) | (i, js) <- zip [0..] $ map ixsXRow rs, j <- js]
    csO   = [(i, j) | (i, js) <- zip [0..] $ map ixsORow rs, j <- js]
    csN   = [(i, j) | (i, js) <- zip [0..] $ map ixsNothingRow rs, j <- js]

lookupRowBoard :: Board -> Int -> Row
lookupRowBoard b i = (rowsBoard b) !! i

lookupBoard :: Board -> Int -> Int -> Cell
lookupBoard b i j = lookupRow r j
  where
    r = lookupRowBoard b i

setAtBoard :: Board -> (Int, Int) -> Cell -> Board
setAtBoard b (i, j) c = setRow b i r'
  where
    setRow :: Board -> Int -> Row -> Board
    setRow b i r
      | (lengthRow r) == (ncolsBoard b) = mkBoard (setAt (rowsBoard b) i r)
      | otherwise               = error "row has bad length"
    r  = lookupRowBoard b i
    r' = setAtRow r j c

emptyBoard :: Int -> Board
emptyBoard n = mkBoard [mkRow [Cell Nothing | _ <- [1..n]] | _ <- [1..n]]

myboard = mkBoard [myrow, row1, row2]
myempty = emptyBoard 3


-- game state

data GameState = GameState
  { boardGameState  :: Board
  , playerGameState :: Player
  }

instance Show GameState where
  show gs = "Player" ++ " = " ++ color (show p) ++ "\n" ++ show b
    where
      b = boardGameState gs
      p = playerGameState gs
      color = case p of
        X -> blue
        O -> red

mkGameState :: Board -> Player -> GameState
mkGameState b p = GameState b p

validMove :: GameState -> (Int, Int) -> Bool
validMove (boardGameState -> b) move  = move `elem` valid
  where
    valid = ixsNothingBoard b

mark :: GameState -> (Int, Int) -> GameState
mark gs (i, j) = assert (validMove gs (i, j)) $ mkGameState b' p'
  where
    b = boardGameState gs
    p = playerGameState gs
    c = Cell (Just p)
    b' = setAtBoard b (i, j) c
    p' = sycle p

data Winner = Tie | Winner Player

instance Show Winner where
  show Tie = "tie..."
  show (Winner p) = color $ "winner: " ++ show p
    where
      color = case p of
        X -> blue
        O -> red

winner :: GameState -> Maybe Winner
winner (boardGameState -> ixsNothingBoard -> []) = Just Tie
winner (boardGameState -> b) = if isXWin then Just (Winner X)
                               else if isOWin then Just (Winner O)
                                              else Nothing
  where
    isXWin = any ((== nrowsBoard b) . length . ixsXRow) (rowsBoard b) ||
             any ((== nrowsBoard b) . length . ixsXRow) (cols b) ||
             any ((== nrowsBoard b) . length . ixsXRow) (diags b)
    isOWin = any ((== nrowsBoard b) . length . ixsORow) (rowsBoard b) ||
             any ((== nrowsBoard b) . length . ixsORow) (cols b) ||
             any ((== nrowsBoard b) . length . ixsORow) (diags b)
    cols b = [mkRow [lookupBoard b i j | i <- [0..nrowsBoard b - 1]] | j <- [0..ncolsBoard b - 1]]
    diags b = [mkRow [lookupBoard b i i | i <- [0..nrowsBoard b - 1]], mkRow [lookupBoard b i (ncolsBoard b - 1 - i) | i <- [0..nrowsBoard b - 1]]]

gameOver :: GameState -> Bool
gameOver (boardGameState -> b) = (any isWin (rowsBoard b) || any isWin (cols b) || any isWin (diags b)) || filled
  where
    isWin r = length (ixsXRow r) == nrowsBoard b ||
              length (ixsORow r) == nrowsBoard b
    cols b = [mkRow [lookupBoard b i j | i <- [0..nrowsBoard b - 1]] | j <- [0..ncolsBoard b - 1]]
    diags b = [mkRow [lookupBoard b i i | i <- [0..nrowsBoard b - 1]], mkRow [lookupBoard b i (ncolsBoard b - 1 - i) | i <- [0..nrowsBoard b - 1]]]
    filled = null (ixsNothingBoard b)

state = mkGameState myboard O

-- others

readMove :: String -> Maybe (Int, Int)
readMove (words -> [read -> i, read -> j]) = Just (i, j)
readMove _ = Nothing

someFunc :: IO ()
someFunc = putStrLn "someFunc"
