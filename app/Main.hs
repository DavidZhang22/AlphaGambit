module Main (main) where

import Chess
import Chess.Rulebook.Standard (standardRulebook)
import Functions.Search (minimaxPar, randomCommand)
import Lib (generateGame, minimax)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))

main :: IO ()
main = do
  args <- getArgs
  hSetBuffering stdout LineBuffering
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
    ["play"] -> do
      let game = generateGame
      playGame game
    _ -> do
      name <- getProgName
      die $ "Usage: " ++ name ++ " <sequential|parallel> <depth=1,2,3,4,5> OR " ++ name ++ " play"

playGame :: Game -> IO ()
playGame g = case standardRulebook.status g of
  Win _ -> putStrLn $ "Win for " ++ show g.activePlayer.color
  Draw -> putStrLn "Draw"
  Turn (Player White) -> do
    let (Update game command) = minimaxPar g 4 g.activePlayer
    print game.board
    playGame game
  Turn (Player Black) -> do
    let (Update game command) = randomCommand g
    print game.board
    playGame game