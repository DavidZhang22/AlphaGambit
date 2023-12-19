module Main (main) where

import Chess
import Functions.Search (minimaxPar)
import Lib (generateGame, minimax)
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["sequential", d] -> do
      let game = generateGame
      let minimaxUpdate = minimax game (read d :: Int) game.activePlayer
      putStrLn "Minimax update:"
      print minimaxUpdate.command
    ["parallel", d] -> do
      let game = generateGame
      let minimaxUpdate = minimaxPar game (read d :: Int) game.activePlayer
      putStrLn "Minimax update:"
      print minimaxUpdate.command
    _ -> do
      name <- getProgName
      die $ "Usage: " ++ name ++ " <sequential|parallel> <depth=1,2,3,4,5>"