{-# LANGUAGE GADTs #-}

module Functions.Scoring (heuristic) where
import Chess
import Chess.Rulebook.Standard.Threat


foldl' _ z []    = z
foldl' f z (x:xs) = let z' = z `f` x 
                     in seq z' $ foldl' f z' xs

-- Heuristic returns a positive score if white is winning, and a negative score if black is winning.
heuristic :: Game -> Int
heuristic game = value (piecesOf White game.board) - value (piecesOf Black game.board)
  where
    value pieces_ = foldl' (+) 0 [pieceValue piece | piece <- pieces_] + threatScore pieces_
    pieceValue :: Some PlacedPiece -> Int
    pieceValue (Some (PlacedPiece _ (Piece piece _))) =
      case piece of
        Pawn -> 10
        Knight -> 30
        Bishop -> 30
        Rook -> 50
        Queen -> 90
        King -> 1000

    threatScore :: [Some PlacedPiece] -> Int
    threatScore pieces_ = foldl' (+) 0  [length (threats p game.board) | (Some p) <- pieces_]