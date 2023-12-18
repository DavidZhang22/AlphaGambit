module Functions.States (generateGame, nextStates) where

import Chess
import Chess.Rulebook.Standard (standardRulebook)
import Functions.Scoring (heuristic)

generateGame_ :: Rulebook -> Game
generateGame_ (Rulebook newGameFunc _ _) = newGameFunc

generateGame :: Game
generateGame = generateGame_ standardRulebook

allUpdates :: Game -> Rulebook -> [Update]
allUpdates game rulebook =
  let sameColor (Some (PlacedPiece _ piece)) = piece.color == game.activePlayer.color
      potentialUpdates (Some (PlacedPiece position _)) = rulebook.updates position game
   in concatMap potentialUpdates $ filter sameColor $ pieces game.board

nextStates :: Game -> [(Update, Int)]
nextStates game =
  let updates = allUpdates game standardRulebook
   in map (\update -> (update, heuristic update.game)) updates
