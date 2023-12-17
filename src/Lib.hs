module Lib
  ( someFunc,
  )
where

import qualified Chess.Board
import Chess.Color
import Chess.Piece

someFunc :: IO ()
someFunc = print piece
  where
    -- x = Chess.Board.empty
    -- position = Chess.Board.mkPosition 1 1
    piece = Piece Pawn Black

-- result = case position of
--   Just p -> Chess.Board.place p piece
--   Nothing -> Nothing
