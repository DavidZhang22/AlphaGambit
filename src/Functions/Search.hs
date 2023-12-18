module Functions.Search (randomCommand, minimax, jamboree) where

import Chess
import Functions.States (nextStates)

randomCommand :: Game -> Update
randomCommand game = fst $ head $ nextStates game

minimax :: Game -> Int -> Player -> Update
minimax game _ _ = randomCommand game -- TODO: implement this

jamboree :: Game -> Int -> Int -> Player -> Update
jamboree game _ _ _ = randomCommand game -- TODO: implement this
