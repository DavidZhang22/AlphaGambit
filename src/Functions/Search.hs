module Functions.Search (randomCommand, minimax, jamboree, minimaxPar) where

import Chess
import Chess.Color
import Chess.Rulebook.Standard.Check (checked)
import Control.Parallel.Strategies
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Functions.Scoring (heuristic)
import Functions.States (nextStates)

randomCommand :: Game -> Update
randomCommand game = head $ nextStates game

findMinTuple :: [(Int, Update)] -> (Int, Update)
findMinTuple = minimumBy (compare `on` fst)

findMaxTuple :: [(Int, Update)] -> (Int, Update)
findMaxTuple = maximumBy (compare `on` fst)

minimax :: Game -> Int -> Player -> Update
minimax game depth player
  | player.color == Chess.Color.White = snd $ findMaxTuple $ map (\update -> (min_ (depth - 1) update.game, update)) (nextStates game)
  | player.color == Chess.Color.Black = snd $ findMinTuple $ map (\update -> (max_ (depth - 1) update.game, update)) (nextStates game)
  where
    max_ :: Int -> Game -> Int
    max_ 0 game = heuristic game
    max_ depth game = maximum (map (\update -> min_ (depth - 1) update.game) (nextStates game))

    min_ :: Int -> Game -> Int
    min_ 0 game = heuristic game
    min_ depth game = minimum (map (\update -> max_ (depth - 1) update.game) (nextStates game))

-- TODO: implement this

minimaxPar :: Game -> Int -> Player -> Update
minimaxPar game depth player
  | player.color == Chess.Color.White = snd $ findMaxTuple $ map (\update -> (min_ (depth - 1) update.game, update)) (nextStates game)
  | otherwise = snd $ findMinTuple $ map (\update -> (max_ (depth - 1) update.game, update)) (nextStates game)
  where
    max_ :: Int -> Game -> Int
    max_ 0 game = heuristic game
    max_ depth game = maximum (map (\update -> min_ (depth - 1) update.game) (nextStates game) `using` parList rseq)

    min_ :: Int -> Game -> Int
    min_ 0 game = heuristic game
    min_ depth game = minimum (map (\update -> max_ (depth - 1) update.game) (nextStates game) `using` parList rseq)

jamboree :: Game -> Int -> Int -> Player -> Update
jamboree game _ _ _ = randomCommand game -- TODO: implement this

-- https://maksmozolewski.co.uk/blog/min-max-alpha-beta-pruning-dfs-bfs-haskell/