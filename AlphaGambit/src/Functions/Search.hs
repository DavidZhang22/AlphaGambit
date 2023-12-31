module Functions.Search (randomCommand, minimax, jamboree, minimaxPar, alphaBeta) where

import Chess
import Chess.Color
import Control.Parallel.Strategies
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Functions.Scoring (heuristic)
import Functions.States (nextStates)

randomCommand :: Game -> Update
randomCommand game =
  let states = nextStates game
   in states !! (length states `div` 2)

findMinTuple :: [(Int, Update)] -> (Int, Update)
findMinTuple = minimumBy (compare `on` fst)

findMaxTuple :: [(Int, Update)] -> (Int, Update)
findMaxTuple = maximumBy (compare `on` fst)

minimax :: Game -> Int -> Player -> Update
minimax game depth player
  | player.color == Chess.Color.White = snd $ findMaxTuple $ map (\update -> (min_ (depth - 1) update.game, update)) (nextStates game)
  | otherwise = snd $ findMinTuple $ map (\update -> (max_ (depth - 1) update.game, update)) (nextStates game)
  where
    max_ :: Int -> Game -> Int
    max_ 0 game_ = heuristic game_
    max_ depth_ game_ = maximum (map (\update -> min_ (depth_ - 1) update.game) (nextStates game_))

    min_ :: Int -> Game -> Int
    min_ 0 game_ = heuristic game_
    min_ depth_ game_ = minimum (map (\update -> max_ (depth_ - 1) update.game) (nextStates game_))


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
  | player.color == Chess.Color.White = snd $ findMaxTuple $ map (\update -> (jamboreee  update.game (-10000) 10000 (depth - 1), update)) (nextStates game)
  | otherwise = snd $ findMinTuple $ map (\update -> (jamboreee  update.game (-10000) 10000 (depth - 1), update)) (nextStates game)

  

jamboreee :: Game -> Int -> Int -> Int -> Int

jamboreee game _ _ 0 = heuristic game
jamboreee game a b depth   | firstVal >= b = firstVal
                           | otherwise = jamboree2 (max firstVal a) b firstVal
  where
    jamboree2 :: Int -> Int -> Int -> Int
    jamboree2 alpha beta bb = maximum (map (\update-> result (-jamboreee update.game (-alpha-1) (-alpha) 1)) possibleMoves `using` parList rseq )
      where
        result :: Int -> Int
        result res | res >= beta = res
                   | val >= beta = val
                   | otherwise = max (max val res) bb
          where
            val = maximum (map (\update->(-jamboreee update.game (-beta) (-alpha) (depth - 1))) possibleMoves `using` parList rseq)

    firstVal = -jamboreee (head possibleMoves).game (-a) (-b) (depth - 1)
    possibleMoves = nextStates game


-- Adapted from https://maksmozolewski.co.uk/blog/min-max-alpha-beta-pruning-dfs-bfs-haskell/
alphaBeta :: Game -> Int -> Player -> Update
alphaBeta game depth player
  | player.color == Chess.Color.White = snd $ findMaxTuple $ map (\update -> (minValue update.game (depth-1) (-2) 2, update)) (nextStates game)
  | otherwise = snd $ findMinTuple $ map (\update -> (maxValue update.game (depth-1) (-2) 2, update)) (nextStates game)
  where
    maxValue :: Game -> Int -> Int -> Int -> Int
    maxValue g 0 _ _ = heuristic g
    maxValue g d a b =
      let states = reverse $ nextStates g

          getMinimaxAndAlpha :: (Int, Int) -> Update -> (Int, Int)
          getMinimaxAndAlpha (bestMinimaxVal, _) update =
            let newMinimax = max bestMinimaxVal (minValue update.game (d - 1) a b)
             in (newMinimax, max a newMinimax)

          (bestMinimax, _) = takeFirstWithOrLastElem (\(v, _) -> v >= b) $ scanl getMinimaxAndAlpha (-2, a) states
       in bestMinimax
    minValue :: Game -> Int -> Int -> Int -> Int
    minValue _ 0 _ _ = heuristic game
    minValue g d a b =
      let states = reverse $ nextStates g

          getMinimaxAndBeta :: (Int, Int) -> Update -> (Int, Int)
          getMinimaxAndBeta (bestMinimaxVal, b_) update =
            let newMinimax = min bestMinimaxVal (maxValue update.game (d - 1) a b_)
             in (newMinimax, min b_ newMinimax)

          (bestMinimax, _) =
            takeFirstWithOrLastElem (\(v, _) -> v <= a) $
              scanl getMinimaxAndBeta (2, b) states
       in bestMinimax

-- will take the first element satisfying the condition, or the last element if none do (last wont be checked)
takeFirstWithOrLastElem :: (a -> Bool) -> [a] -> a
takeFirstWithOrLastElem _ [x] = x
takeFirstWithOrLastElem cond (x : xs) = if cond x then x else takeFirstWithOrLastElem cond xs
takeFirstWithOrLastElem _ [] = error "Empty list given"