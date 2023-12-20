module Functions.States (generateGame, nextStates) where

import Chess
import Chess.Rulebook.Standard (standardRulebook)

generateGame :: Game
generateGame = standardRulebook.newGame


build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
build g = g (:) []

concatMap_ :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap_ f xs = build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
{-# INLINE concatMap_ #-}

allUpdates :: Game -> Rulebook -> [Update]
allUpdates game rulebook =
  let sameColor (Some (PlacedPiece _ piece)) = piece.color == game.activePlayer.color
      potentialUpdates (Some (PlacedPiece position _)) = rulebook.updates position game
   in concatMap_ potentialUpdates $ filter sameColor $ pieces game.board

nextStates :: Game -> [Update]
nextStates game
  | null updates = [Update game endTurn]
  | otherwise = updates
  where updates = allUpdates game standardRulebook


