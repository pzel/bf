module Main where

import Data.Either (isLeft)
import Test.HUnit
import BF

main = runTestTT allTests

allTests = test (parseTests ++ evalTests)
parseTests =
  [ parseSource "." ~?= Right [O]
  , parseSource "><+-.," ~?= Right [R,L,Inc,Dec,O,I]
  , parseSource ",[.,]" ~?= Right [I, Loop [O,I]]
  , parseSource ",[.[-],]" ~?= Right [I, Loop [O, Loop [Dec], I]]
  , parseSource "[-][+]" ~?= Right [Loop [Dec], Loop [Inc]]

  , parseSource "." ~?= Right [O]
  , isLeft (parseSource "[") ~? "Error is raised on unbalanced source 1"
  , isLeft (parseSource "[[[]][[]]") ~? "unbalanced source 2"
  ]

evalTests =
  let parse' s = case parseSource s of (Right x) -> x ; (Left _) -> error "no parse"
      run src = eval (parse' src) mkVm
  in [ TestCase $ do { vm <- run "+"; assertEqual "" 1 (focused vm) }
     , TestCase $ do { vm <- run "+++-"; assertEqual "" 2 (focused vm) }
     , TestCase $ do { vm <- run ">+<"; assertEqual "" 0 (focused vm) }
     , TestCase $ do { vm <- run "-"; assertEqual "" 255 (focused vm) }
  ]
