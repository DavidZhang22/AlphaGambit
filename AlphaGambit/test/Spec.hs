import Chess
import Lib (generateGame, heuristic, minimax, nextStates, randomCommand)

main :: IO ()
main = do
  -- TEST 1: Heuristic is non-zero when pieces change.

  let game = generateGame
  let nextState g =
        let Update game1 _ = randomCommand g
         in game1

  -- game state after 100 moves.
  let loop100 = iterate nextState game !! 100
  let score = heuristic loop100

  -- this is actually deterministic so score should not be zero.
  putStrLn "TEST 1: Heuristic is non-zero when pieces change."
  putStrLn $ "Score: " ++ show score
  putStrLn $ if score /= 0 then "OK" else "FAIL!"

  -- TEST 2: Next States function works
  putStrLn "TEST 2: Next States function finds >0 states"
  let potentialNextStates = nextStates loop100
  let numStates = length potentialNextStates
  putStrLn $ "Found " ++ show (length potentialNextStates) ++ " potential updates"
  putStrLn $ if numStates /= 0 then "OK" else "FAIL!"

  -- Test 3: MinMax function
  putStrLn "TEST 3: Min Max function runs successfully."
  let futureUpdate = minimax game 4 game.activePlayer
  putStrLn "Minimax update:"
  print futureUpdate.command
  putStrLn "OK"
