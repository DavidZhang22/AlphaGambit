module Functions.Search (randomCommand, minimax, jamboree, minimaxPar) where

import Chess
import Chess.Color
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
    max_ 0 game1 = heuristic game1
    max_ d game2 = maximum (map (\update -> min_ (d - 1) update.game) (nextStates game2) `using` parList rseq)

    min_ :: Int -> Game -> Int
    min_ 0 game1 = heuristic game1
    min_ d game2 = minimum (map (\update -> max_ (d - 1) update.game) (nextStates game2) `using` parList rseq)




jamboree :: Game -> Int -> Player -> Update

jamboree game depth player
  | player.color == Chess.Color.White = snd $ findMaxTuple $ map (\update -> (jamboreee  update.game 10000 (-10000) (depth - 1), update)) (nextStates game)
  | otherwise = snd $ findMinTuple $ map (\update -> (jamboreee  update.game 10000 (-10000) (depth - 1), update)) (nextStates game)


jamboreee :: Game -> Int -> Int -> Int -> Int

jamboreee game _ _ 0 = heuristic game
jamboreee game a b depth   | firstVal >= b = firstVal
                          | otherwise = jamboree2 (max firstVal a) b firstVal
  where
    jamboree2 :: Int -> Int -> Int -> Int
    jamboree2 alpha beta bb = maximum (map (\update-> result (-jamboreee update.game (-alpha-1) (-alpha) (depth - 1))) possibleMoves)
      where
        result :: Int -> Int
        result res | res >= beta = res
                   | val >= beta = val
                   | otherwise = max (max val res) bb
          where
            val = maximum (map (\update->(-jamboreee update.game (-beta) (-alpha) (depth - 1))) possibleMoves)

    firstVal = -jamboreee (head possibleMoves).game (-a) (-b) (depth - 1)
    possibleMoves = nextStates game
