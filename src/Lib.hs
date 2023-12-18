module Lib
  ( generateGame,
    jamboree,
    minimax,
    randomCommand,
    heuristic,
    nextStates,
  )
where

-- https://hackage.haskell.org/package/hchesslib-0.2.0.0/docs/Chess.html

-- import Chess.Rulebook.Standard (standardRulebook)
-- import Chess.Rulebook (Rulebook (..))

-- import Chess.Board (PlacedPiece (PlacedPiece), piecesOf)
-- import Chess.Game (Game (..), Update)
-- import Chess.Player (Player (..))
-- import Chess.Rulebook (Rulebook (..))
import Chess
import Chess.Rulebook.Standard (standardRulebook)
-- import Chess.Some (Some (Some))
import Functions.Scoring (heuristic)

generateGame_ :: Rulebook -> Game
generateGame_ (Rulebook newGameFunc _ _) = newGameFunc

generateGame :: Game
generateGame = generateGame_ standardRulebook

-- generateGame = newGame standardRulebook

allUpdates :: Game -> Rulebook -> [Update]
allUpdates game rulebook =
  let sameColor (Some (PlacedPiece _ piece)) = piece.color == game.activePlayer.color
      potentialUpdates (Some (PlacedPiece position _)) = rulebook.updates position game
   in concatMap potentialUpdates $ filter sameColor $ pieces game.board

nextStates :: Game -> [(Update, Int)]
nextStates game =
  let updates = allUpdates game standardRulebook
   in map (\update -> (update, heuristic update.game)) updates

randomCommand :: Game -> Update
-- get the middle element of the list of all moves
randomCommand game = head $ allUpdates game standardRulebook

minimax :: Game -> Int -> Player -> Update
minimax game _ _ = randomCommand game -- TODO: implement this

jamboree :: Game -> Int -> Int -> Player -> Update
jamboree game _ _ _ = randomCommand game -- TODO: implement this
