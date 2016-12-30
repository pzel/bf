module Main where
import BF
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename:_) <- getArgs
  source <- readFile filename
  _ <- (parseAndRun source)
  return ()
