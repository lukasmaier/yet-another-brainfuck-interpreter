module Interpreter where

import qualified Data.Sequence as S

data Tape = TP (S.Seq Int) Int

interpret :: String -> IO Tape
interpret code = interpret' code (TP (S.singleton 0) 0)

interpret' :: String -> Tape -> IO Tape
interpret' code@(c:cs) tape@(TP seq i) = case c of 
                                    '>' ->  if i == (S.length seq - 1) then
                                                interpret' cs (TP (seq S.|> 0) (i + 1))
                                            else
                                                interpret' cs (TP seq (i + 1))
                                    '<' ->  if i == 0 then
                                                interpret' cs (TP (0 S.<| seq) i)
                                            else 
                                                interpret' cs (TP seq (i-1))
                                    '+' -> interpret' cs (TP (S.update i (S.index seq i + 1) seq) i)
                                    '-' -> interpret' cs (TP (S.update i (S.index seq i - 1) seq) i)
                                    '[' -> loop 0 [] cs tape
                                    ']' -> error "'[' needed before ']'"
                                    '.' -> putChar (toEnum (S.index seq i) :: Char) >> interpret' cs (TP seq i)
                                    ',' -> getChar >>= \q -> interpret' cs (TP (S.update i (fromEnum q) seq) i)
                                    _   -> interpret' cs (TP seq i)
interpret' _ tape = return tape


loop :: Int -> [Char] -> String -> Tape -> IO Tape
loop depth accu code@(c:cs) tape@(TP seq i) | c == ']' && depth == 0 = loop' accu cs tape
                                            | c == ']' = loop (depth - 1) (accu ++ [c]) cs tape
                                            | c == '[' = loop (depth + 1) (accu ++ [c]) cs tape
                                            | otherwise = loop depth (accu ++ [c]) cs tape
                                                where
                                                    loop' :: [Char] -> String -> Tape -> IO Tape
                                                    loop' accu code tape@(TP seq i) | S.index seq i /= 0 = interpret' accu tape >>= loop' accu code 
                                                                                    | otherwise = interpret' code tape
loop _ _ _ _ = error "']' needed after '['"


