import Chess
import Data.List (intercalate)
import Lib (generateGame, heuristic, nextStates, randomCommand, minimax)

main :: IO ()
main = do
  -- TEST 1: Heuristic is non-zero when pieces change.

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

  -- TEST 2: Next States function works
--   let potentialNextStates = nextStates loop100
--   putStrLn $ intercalate "\n" $ map (\(update) -> show update.command) potentialNextStates

  -- Test 3: MinMax function
  let futureUpdate = minimax game 5 game.activePlayer
  putStrLn "Minimax update:"
  putStrLn $ show futureUpdate.command


