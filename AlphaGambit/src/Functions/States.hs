module Functions.States (generateGame, nextStates) where

import Chess
import Chess.Rulebook.Standard (standardRulebook)

generateGame :: Game
generateGame = standardRulebook.newGame

allUpdates :: Game -> Rulebook -> [Update]
allUpdates game rulebook =
  let sameColor (Some (PlacedPiece _ piece)) = piece.color == game.activePlayer.color
      potentialUpdates (Some (PlacedPiece position _)) = rulebook.updates position game
   in concatMap potentialUpdates $ filter sameColor $ pieces game.board

nextStates :: Game -> [Update]
nextStates game
  | null updates = [Update game endTurn]
  | otherwise = updates
  where updates = allUpdates game standardRulebook


