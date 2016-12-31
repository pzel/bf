module Main where

import Data.Either (isLeft)
import Test.HUnit
import BF

main = runTestTT allTests

allTests = test
  [ parseSource "." ~?= Right [O]
  , parseSource "><+-.," ~?= Right [R,L,Inc,Dec,O,I]
  , parseSource ",[.,]" ~?= Right [I, Loop [O,I]]
  , parseSource ",[.[-],]" ~?= Right [I, Loop [O, Loop [Dec], I]]
  , parseSource "[-][+]" ~?= Right [Loop [Dec], Loop [Inc]]

  , parseSource "." ~?= Right [O]
  , isLeft (parseSource "[") ~? "Error is raised on unbalanced source 1"
  , isLeft (parseSource "[[[]][[]]") ~? "unbalanced source 2"
  ]


