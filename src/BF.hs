{-# LANGUAGE RecordWildCards #-}
module BF where
import Control.Concurrent
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)
import Data.Word
import Data.Int
import Text.Regex (Regex, mkRegex, matchRegexAll)

type Cell = Word8
data AST = L | R | Inc | Dec | I | O | Loop [AST]
           deriving (Eq, Show)
data VM = VM [Cell] [Cell]

left (VM (x:ls) rs) = VM ls (x:rs)
right (VM ls (x:rs)) = VM (x:ls) rs
focused (VM _ (x:_)) = x
swap f (VM ls (x:rs)) = VM ls ((f x):rs)
inc = swap (+1)
dec = swap (\x -> x - 1)

parseAndRun :: String -> IO VM
parseAndRun s =
  case parseSource s of
    Left e -> error e
    Right ast -> run ast

run :: [AST] -> IO VM
run cmds = run' cmds (VM (repeat 0) (repeat 0))

run' :: [AST] -> VM -> IO VM
run' [] vm = return vm
run' (Inc:cmds) vm = run' cmds (inc vm)
run' (Dec:cmds) vm = run' cmds (dec vm)
run' (R:cmds) vm = run' cmds (right vm)
run' (L:cmds) vm = run' cmds (left vm)
run' (O:cmds) vm = output (focused vm) >> run' cmds vm
run' (I:cmds) vm = getInput vm >>= \newVm -> run' cmds newVm
run' l@((Loop cs):next) vm =
  case focused vm of
    0 -> run' next vm
    nonzero -> -- (putStrLn $ "loop: " ++ (show nonzero)) >>
               run' cs vm >>= \newVm -> run' l newVm

output c = putStr [chr $ fromIntegral c]
getInput vm = getChar >>= \c ->
              return $ swap (const (fromIntegral $ ord c)) vm

parseSource :: String -> Either String [AST]
parseSource s =
  if length (filter (== '[') s) == length (filter (== ']') s)
  then Right (toAST (clean s))
  else Left "Mismatched brackets"
  where
    clean = concat . lines

parseCmd :: Char -> Maybe AST
parseCmd = flip lookup
           [('>', R), ('<', L),
            ('+', Inc), ('-', Dec),
            ('.', O), (',', I)]

toAST :: String -> [AST]
toAST s =
  case matchRegexAll (mkRegex "\\[(.*)\\]") s of
    Nothing -> mapMaybe parseCmd s
    Just (before,_,after,[match]) ->
      toAST before ++ [Loop (toAST match)] ++ toAST after
    x -> error ("PARSE ERROR AT: " ++ show x)
