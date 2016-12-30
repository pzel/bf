module Main where

import Test.HUnit
import BF

main = runTestTT allTests

allTests =
  test [ toAST "." ~?= [O]
       , toAST "><+-.," ~?= [R,L,Inc,Dec,O,I]
       , toAST ",[.,]" ~?= [I, Loop [O,I]]
       , toAST ",[.[-],]" ~?= [I, Loop [O, Loop [Dec], I]]

       ,parseSource "." ~?= Right [O]
       ,parseSource "[" ~?= Left "Mismatched brackets"
       ]
