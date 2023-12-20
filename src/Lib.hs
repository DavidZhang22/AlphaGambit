{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib
  ( generateGame,
    jamboree,
    minimax,
    minimaxPar,
    alphaBeta,
    randomCommand,
    heuristic,
    nextStates,
  )
where

import Chess
import Functions.Scoring (heuristic)
import Functions.Search (jamboree, minimax, minimaxPar, randomCommand, alphaBeta)
import Functions.States (generateGame, nextStates)

instance Show Board where
  show board = unlines [showRow r | r <- [0 .. 7]]
    where
      showRow r = unwords [showPosition (boundedPosition r c) | c <- [0 .. 7]]
      showPosition pos = maybe "." showPiece (Chess.lookup pos board)
      showPiece (Some (PlacedPiece _ piece)) = unicodeChess piece

unicodeChess :: Piece t -> String
unicodeChess (Piece piece color) = case (piece, color) of
  (King, White) -> "♔"
  (Queen, White) -> "♕"
  (Rook, White) -> "♖"
  (Bishop, White) -> "♗"
  (Knight, White) -> "♘"
  (Pawn, White) -> "♙"
  (King, Black) -> "♚"
  (Queen, Black) -> "♛"
  (Rook, Black) -> "♜"
  (Bishop, Black) -> "♝"
  (Knight, Black) -> "♞"
  (Pawn, Black) -> "♟︎"