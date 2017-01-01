module BF where

import Control.Monad (foldM)
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)

type Cell = Word8
data Expr = L | R | Inc | Dec | I | O | Loop [Expr]
           deriving (Eq, Show)
data VM = VM [Cell] [Cell] deriving (Eq)

left (VM (x:ls) rs) = VM ls (x:rs)
right (VM ls (x:rs)) = VM (x:ls) rs
focused (VM _ (x:_)) = x
swap f (VM ls (x:rs)) = VM ls ((f x):rs)
inc = swap (+1)
dec = swap (\x -> x - 1)
mkVm = VM (repeat 0) (repeat 0)

parseAndRun :: String -> IO VM
parseAndRun s =
  case parseSource s of
    Left e -> error (show e)
    Right ast -> eval ast mkVm

eval :: [Expr] -> VM -> IO VM
eval cmds vm = foldM (flip evalStep) vm cmds

evalStep :: Expr -> VM -> IO VM
evalStep Inc = return . inc
evalStep Dec = return . dec
evalStep R = return . right
evalStep L = return . left
evalStep O = evalOutput
evalStep I = evalInput
evalStep l@(Loop cs) = evalLoop l

evalLoop l@(Loop cs) vm =
  if 0 == focused vm
  then return vm
  else (eval cs vm) >>= evalStep (Loop cs)
evalOutput vm = putStr [chr $ fromIntegral (focused vm)] >> return vm
evalInput vm = getChar >>= \c -> return $ swap (const (fromIntegral $ ord c)) vm

parseSource :: String -> Either ParseError [Expr]
parseSource s = parse (many expr) "BF syntax error" (clean s)
  where clean = filter (`elem` "><+-.,[]")

expr :: Parser Expr
expr = choice
       [char ',' >> return I, char '.' >> return O
       ,char '+' >> return Inc, char '-' >> return Dec
       ,char '<' >> return L, char '>' >> return R
       ,loop ]
  where
    loop = do
      _ <- char '['
      es <- many1 expr
      _ <- char ']'
      return $ Loop es
