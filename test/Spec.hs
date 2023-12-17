import Lib (generateGame, randomCommand)

main :: IO ()
main = do
  game <- generateGame
  randomMove <- randomCommand game
  putStrLn randomMove
