module Main (main) where

import Prelude hiding (LT, GT, EQ)

import SymStack

exCode :: Code
exCode =    [ PUSH 0
            , READ
            , PUSH 5
            , LT
            , DIV
            , STOP
            ]

main :: IO ()
main = eval State { code = exCode, counter = 0, stack = [], pc = SymBool True }
