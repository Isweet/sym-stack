module Main (main) where

import Prelude hiding (LT, GT, EQ)

import SymStack

{-
 - int x = read()
 - int y = read()
 - int z = read()
 - if (z == 73) then
 -     return y / x;
 - else
 -     return y * x;
 -}

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

defaultState :: State
defaultState = State { code = [], counter = 0, stack = [], pc = SymBool True }

testRunner :: [(String, Code)] -> IO ()
testRunner = mapM_ (\ (name, test) -> putStrLn ((underline (name ++ ":")) ++ "\n") >>= (\ _ -> eval $ defaultState { code = test }))
    where
        underline str = str ++ "\n" ++ (replicate (length str) '-')

main :: IO ()
main = testRunner [("simpleIf", simpleIf), ("brokenIf", brokenIf)]

