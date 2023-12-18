{-# LANGUAGE GADTs #-}

module Functions.Scoring (heuristic) where

import Chess

-- Heuristic returns a positive score if white is winning, and a negative score if black is winning.
heuristic :: Game -> Int
heuristic (Game board _ _ _) = values (piecesOf White board) (piecesOf Black board)
  where
    values p1 p2 = value p1 - value p2
    value pieces = sum [pieceValue piece | piece <- pieces]
    pieceValue :: Some PlacedPiece -> Int
    pieceValue (Some (PlacedPiece _ (Piece piece _))) = case piece of
      Pawn -> 1
      Knight -> 3
      Bishop -> 3
      Rook -> 5
      Queen -> 9
      King -> 100
