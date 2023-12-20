{-# LANGUAGE GADTs #-}

module Functions.Scoring (heuristic) where
import Chess
import Chess.Rulebook.Standard.Threat

-- Heuristic returns a positive score if white is winning, and a negative score if black is winning.
heuristic :: Game -> Int
heuristic game = value (piecesOf White game.board) - value (piecesOf Black game.board)
  where
    value pieces_ = sum [pieceValue piece | piece <- pieces_] + threatScore pieces_
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
    threatScore pieces_ = sum [length (threats p game.board) | (Some p) <- pieces_]