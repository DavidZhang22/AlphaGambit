import Chess
import Lib (generateGame, heuristic, randomCommand)


main :: IO ()
main = do
  let game = generateGame

  let (Game board _ _ _) = game
  print $ piecesOf White board

  let nextState g = case randomCommand g of
        Update game command -> game

  let loop20 = iterate nextState game !! 100

  let score = heuristic loop20
  let (Game board _ _ _) = loop20
  print $ piecesOf White board
  print score
