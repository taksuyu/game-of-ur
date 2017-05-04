import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

-- Tests
import Test.Game.Ur as Game.Ur

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
    Game.Ur.tests
    ]

  pure ()
