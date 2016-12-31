{-# LANGUAGE RecordWildCards #-}
module BF where
import Control.Concurrent
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)
import Data.Word
import Data.Int
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

type Cell = Word8
data Expr = L | R | Inc | Dec | I | O | Loop [Expr]
           deriving (Eq, Show)
data VM = VM [Cell] [Cell] deriving (Eq, Show)

left (VM (x:ls) rs) = VM ls (x:rs)
right (VM ls (x:rs)) = VM (x:ls) rs
focused (VM _ (x:_)) = x
swap f (VM ls (x:rs)) = VM ls ((f x):rs)
inc = swap (+1)
dec = swap (\x -> x - 1)
mkVm :: VM
mkVm = VM (repeat 0) (repeat 0)

snapshot :: VM -> String
snapshot (VM l (x:rs)) =
  show (reverse $ take 5 l) ++ (show [x]) ++ (show (take 5 rs))

parseAndRun :: String -> IO VM
parseAndRun s =
  case parseSource s of
    Left e -> error (show e)
    Right ast -> run ast

run :: [Expr] -> IO VM
run cmds = run' cmds mkVm

stepDbg :: [Expr] -> VM -> IO VM
stepDbg cmds vm = do
  putStrLn (show $ take 5 cmds)
  putStrLn (snapshot vm)
  run' cmds vm

step = run'

run' :: [Expr] -> VM -> IO VM
run' [] vm = return vm
run' (Inc:cmds) vm = step cmds (inc vm)
run' (Dec:cmds) vm = step cmds (dec vm)
run' (R:cmds) vm = step cmds (right vm)
run' (L:cmds) vm = step cmds (left vm)
run' (O:cmds) vm = output (focused vm) >> step cmds vm
run' (I:cmds) vm = input vm >>= \newVm -> step cmds newVm
run' l@((Loop cs):next) vm =
  case focused vm of
    0 -> step next vm
    nonzero -> step cs vm >>= \newVm -> step l newVm

output c = putStr [chr $ fromIntegral c]
input vm = getChar >>= \c ->
              return $ swap (const (fromIntegral $ ord c)) vm

parseSource :: String -> Either ParseError [Expr]
parseSource s = parse parseBf "BF syntax error" (clean s)
  where clean = concat . lines . filter (`elem` "><+-.,[]")

parseBf :: Parser [Expr]
parseBf = many expr

expr :: Parser Expr
expr = choice [char ',' >> return I
              ,char '.' >> return O
              ,char '+' >> return Inc
              ,char '-' >> return Dec
              ,char '<' >> return L
              ,char '>' >> return R
              ,loop ]
  where
    loop = do
      _ <- char '['
      es <- many1 expr
      _ <- char ']'
      return $ Loop es
