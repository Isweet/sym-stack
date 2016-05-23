module Instr where

import Prelude hiding (LT, GT, EQ)

data Instr =
    STOP
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LT
  | GT
  | EQ
  | ISZERO
  | AND
  | OR
  | PUSH Int
  | POP
  | DUPN Int
  | SWAPN Int
  | JUMP
  | JUMPI
  | READ deriving ( Show, Eq, Ord )
