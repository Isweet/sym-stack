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


simple :: Code
simple   =  [ READ
            , READ
            , DIV
            , STOP
            ]

simpleIf :: Code
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

brokenIf :: Code
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

defaultControl :: Ctl
defaultControl = Ctl { code = [], counter = 0 }

main :: IO ()
main = mapM_ putStrLn $ runSymStack (defaultControl { code = simple }) 1000
