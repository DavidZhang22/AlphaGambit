import Lib (generateGame, heuristic, randomCommand)
import Chess.Game (Game(..), execute, destroy)
import Chess.Game.Command (Command)

import Chess.Board (piecesOf, PlacedPiece (piece))
import Chess.Color (Color(White))
import Chess.Some (Some(Some))


nextStateM :: Game -> IO Game
nextStateM g = do
  let y = case execute (randomCommand g) g of
        Left _ -> g
        Right x -> x
  return y

main :: IO ()
main = do
  let game = generateGame

  let (Game board _ _ _) = game
  print $ piecesOf White board

  let nextState g = case execute (randomCommand g) g of
        Left _ -> g
        Right x -> x

  -- apply nextState 20 times
  -- TODO: do we need to call endTurn as well?
  let loop20 = iterate nextState game !! 100

  let score = heuristic loop20
  let (Game board _ _ _) = loop20
  print $ piecesOf White board
  print score
