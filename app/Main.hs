module Main (main) where

import Chess
import Lib (generateGame, minimax)
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [_] -> do
      let game = generateGame
      let minimaxUpdate = minimax game 3 game.activePlayer
      putStrLn "Minimax update:"
      putStrLn $ show minimaxUpdate.command
    _ -> do
      name <- getProgName
      die $ "Usage: " ++ name ++ " <move>"