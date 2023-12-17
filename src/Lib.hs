module Lib
  ( generateGame,
    jamboree,
    minimax,
    randomCommand,
  )
where

-- https://hackage.haskell.org/package/hchesslib-0.2.0.0/docs/Chess.html

-- import Chess.Rulebook.Standard (standardRulebook)
-- import Chess.Rulebook (Rulebook (..))

import Chess.Board (piecesOf)
import Chess.Game (Game (..))
import Chess.Game.Command (Command)
import Chess.Player (Player (..))
import Chess.Rulebook (Rulebook (..))
import Chess.Rulebook.Standard (standardRulebook)
import Chess.Rulebook.Standard.Movement (movements)
import Chess.Some (Some (Some))

generateGame_ :: Rulebook -> Game
generateGame_ (Rulebook newGameFunc _ _) = newGameFunc

generateGame :: Game
generateGame = generateGame_ standardRulebook

-- generateGame = newGame standardRulebook

allMoves :: Game -> [Command]
allMoves game@(Game board (Player color) _ _) = result
  where
    pieces = piecesOf color board
    result = concat [movements piece game | Some piece <- pieces]

randomCommand :: Game -> Command
randomCommand game = head $ allMoves game

minimax :: Game -> Int -> Player -> Command
minimax game depth player = randomCommand game -- TODO: implement this

jamboree :: Game -> Int -> Int -> Player -> Command
jamboree game _ _ _ = randomCommand game -- TODO: implement this
