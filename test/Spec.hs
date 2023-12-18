import Chess
import Lib (generateGame, heuristic, randomCommand)

main :: IO ()
main = do
  let game = generateGame
  let nextState g = case randomCommand g of
        Update game command -> game

  -- game state after 100 moves.
  let loop100 = iterate nextState game !! 100

  let score = heuristic loop100
  --   let (Game board _ _ _) = loop20
  --   print $ piecesOf White board

  -- this is actually deterministic so score should not be zero.
  putStrLn $ if score /= 0 then "OK" else "FAIL!"
