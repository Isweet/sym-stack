module Main (main) where

import Prelude hiding (LT, GT, EQ)

import Instr
import SymStackN

{-
 - int x = read()
 - int y = read()
 - int z = read()
 - if (z == 73) then
 -     return y / x;
 - else
 -     return y * x;
 -}


simple :: [Instr]
simple   =  [ READ
            , READ
            , DIV
            , STOP
            ]

simpleJmp :: [Instr]
simpleJmp = [ PUSH 3
            , PUSH 5
            , PUSH 2
            , READ
            , MOD
            , PUSH 9
            , ADD
            , JUMP
            , ADD
            , STOP
            ]

simpleIf :: [Instr]
simpleIf =  [ READ
            , READ
            , READ
            , PUSH 73
            , EQ
            , PUSH 10
            , JUMPI
            , MUL
            , PUSH 11
            , JUMP
            , DIV
            , STOP
            ]

brokenIf :: [Instr]
brokenIf =  [ READ
            , READ
            , READ
            , PUSH 73
            , EQ
            , PUSH 10
            , JUMPI
            , DIV
            , PUSH 11
            , JUMP
            , DIV
            , STOP
            ]

runExample :: [Instr] -> IO ()
runExample c = mapM_ putStrLn $ runSymStack c 1000

main :: IO ()
main = runExample simpleJmp
