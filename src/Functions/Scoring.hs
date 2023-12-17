{-# LANGUAGE GADTs #-}

module Functions.Scoring (heuristic) where

import Chess.Board (PlacedPiece (..), piecesOf)
import Chess.Game (Game (..))
import Chess.Piece (Piece (..), PieceType (..))
import Chess.Player (Player (..))
import Chess.Some (Some (Some))

heuristic :: Game -> Int
heuristic (Game board (Player c1) (Player c2) _) = values (piecesOf c1 board) (piecesOf c2 board)
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
