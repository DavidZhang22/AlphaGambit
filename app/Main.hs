module Main (main) where

import Lib (generateGame, randomCommand)
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [move] -> do
      let game = generateGame
      let randomMove = randomCommand game
      print randomMove
    _ -> do
      name <- getProgName
      die $ "Usage: " ++ name ++ " <move>"