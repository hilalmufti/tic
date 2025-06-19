module Main where

import Engine

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!"
  loop $ mkGameState (emptyBoard 3) O
  where
    loop :: GameState -> IO ()
    loop gs = do
      clearScreen
      print gs
      case winner gs of
        Nothing -> do
          move <- getMove
          loop (if validMove gs move then mark gs move else gs)
        Just w -> print w

    getMove :: IO (Int, Int)
    getMove = do
      input <- getLine
      case readMove input of
        Nothing -> do
          clearScreen
          getMove
        Just m -> return m

    clearScreen :: IO ()
    clearScreen = putStr "\ESC[2J\ESC[H"
