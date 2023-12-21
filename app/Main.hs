module Main (main) where

import Chess
import Chess.Rulebook.Standard (standardRulebook)
import Lib (generateGame, minimax, jamboree, alphaBeta, minimaxPar)
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
      putStrLn "Sequential Minimax update:"
      print minimaxUpdate.command
    ["parallel", d] -> do
      let game = generateGame
      let minimaxUpdate = minimaxPar game (read d :: Int) game.activePlayer
      putStrLn "Parallel Minimax update:"
      print minimaxUpdate.command
    ["jamboree", d] -> do
      let game = generateGame
      let minimaxUpdate = jamboree game (read d :: Int) game.activePlayer
      putStrLn "Jamboree update:"
      print minimaxUpdate.command
    ["alpha_beta", d] -> do
      let game = generateGame
      let minimaxUpdate = alphaBeta game (read d :: Int) game.activePlayer
      putStrLn "Alpha-Beta update:"
      print minimaxUpdate.command
    ["play"] -> do
      let game = generateGame
      playGame game
    _ -> do
      name <- getProgName
      die $ "Usage: " ++ name ++ " <sequential|parallel|jamboree|alpha_beta> <depth> OR " ++ name ++ " play"

playGame :: Game -> IO ()
playGame g = case standardRulebook.status g of
  Win player -> putStrLn $ "Win for " ++ show player.color
  Draw -> putStrLn "Draw"
  Turn (Player White) -> do
    let (Update game _) = minimaxPar g 4 g.activePlayer
    print game.board
    playGame game
  Turn (Player Black) -> do
    let (Update game _) = minimaxPar g 4 g.activePlayer
    print game.board
    playGame game