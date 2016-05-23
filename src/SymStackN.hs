module SymStackN where

import Prelude hiding (LT, GT, EQ, div, mod, and, or, read, log)
import Control.Monad

import Control.Monad.Writer.Lazy
import Pipes hiding (next)
import Control.Monad.State.Lazy

import Symbol
import Instr

{- Types -}

type Code = [Instr]
type Stack = [Symbol]
data Cond = Cond { next :: Integer, pc :: Symbol } deriving ( Show, Eq, Ord )

data St = St { stack :: Stack, cond :: Cond } deriving ( Show, Eq, Ord )

type SymStack a = StateT St (ListT (Writer [String])) a

data Ctl = Ctl { code :: Code, counter :: Int } deriving ( Show, Eq, Ord )

{- Runner -}
defaultSt :: St
defaultSt = St { stack = [], cond = Cond { next = 0, pc = SymBool True } }

eval :: Ctl -> Int -> SymStack Ctl
eval ctl 0 = return ctl
eval ctl n = do
  ctl' <- step ctl
  eval ctl' (n - 1)

runSymStack :: Ctl -> Int -> [String]
runSymStack init depth = (execWriter . runListT . evalStateT (eval init depth)) defaultSt

{- Generic Helpers -}

alterArgsBin :: (a -> a) -> (a -> a) -> (a -> a -> a) -> a -> a -> a
alterArgsBin change1 change2 f a b = f (change1 a) (change2 b)

{- SymStack Helpers -}

-- Increment program counter
tick :: Ctl -> Ctl
tick c = c { counter = (counter c) + 1 }

-- Turn an integer into its boolean representation
sIntToBool :: Symbol -> Symbol
sIntToBool i = Not (Equal i (SymInt 0))

-- Transform a function over booleans into a function over integers
sIntToBoolBin :: (Symbol -> Symbol -> Symbol) -> Symbol -> Symbol -> Symbol
sIntToBoolBin f = alterArgsBin sIntToBool sIntToBool f

-- Perform effect then tick
incr :: Ctl -> SymStack a -> SymStack Ctl
incr ctl eff = do
  eff
  return (tick ctl)

fresh :: String -> SymStack Symbol
fresh prefix = do
  st <- get
  let prevCond = cond st
  let prev = next prevCond
  put (st { cond = (cond st) { next = prev + 1 } })
  return (Atom (prefix ++ (show prev)))

addPC :: Symbol -> SymStack ()
addPC sym = do
  st <- get
  let prevCond = cond st
  let prev = pc prevCond
  put (st { cond = (cond st) { pc = And (prev) sym } })

log :: Ctl -> SymStack Ctl
log ctl = do
  st <- get
  (lift . lift . tell) (["CONTROL: \n" ++ (show ctl) ++ "\nSTATE: \n" ++ (show st) ++ "\n"])
  return ctl

-- Effect for arithmetic instructions
arith :: (Symbol -> Symbol -> Symbol) -> SymStack ()
arith f = do
  x1 <- pop
  x2 <- pop
  x' <- fresh "x"
  addPC (Equal x' (f x1 x2))
  push x'

-- Effect for binary boolean instructions
boolBin :: (Symbol -> Symbol -> Symbol) -> SymStack ()
boolBin f = do
  x1 <- pop
  x2 <- pop
  x' <- fresh "x"
  addPC (Equal x' (Ite (f x1 x2) (SymInt 1) (SymInt 0)))
  push x'

-- Effect for unary boolean instructions
boolUn :: (Symbol -> Symbol) -> SymStack ()
boolUn f = do
  x <- pop
  x' <- fresh "x"
  addPC (Equal x' (Ite (f x) (SymInt 1) (SymInt 0)))
  push x'

{- Instruction effects -}

stop :: SymStack Ctl
stop = mzero
  
add :: SymStack ()
add = arith Add

sub :: SymStack ()
sub = arith Sub
  
mul :: SymStack ()
mul = arith Mul

div :: SymStack ()
div = arith Div

mod :: SymStack ()
mod = arith Mod

lt  :: SymStack ()
lt  = boolBin Less

gt  :: SymStack ()
gt  = boolBin Greater

eq  :: SymStack ()
eq  = boolBin Equal

iszero :: SymStack ()
iszero = boolUn (\sym -> Equal sym (SymInt 0))

and :: SymStack ()
and = boolBin (sIntToBoolBin And)

or :: SymStack ()
or  = boolBin (sIntToBoolBin Or)

push :: Symbol -> SymStack ()
push sym = do
  st <- get
  let stack' = sym : (stack st)
  put (st { stack = stack' })

pop :: SymStack Symbol
pop = do
  st <- get
  let ret : stack' = (stack st)
  put (st { stack = stack' })
  return ret

dupn :: Int -> SymStack ()
dupn idx = mzero

swapn :: Int -> SymStack ()
swapn idx = mzero

jump :: SymStack Ctl
jump = mzero

jumpi :: SymStack Ctl
jumpi = mzero

read :: SymStack ()
read = do
  x' <- fresh "p"
  push x'
  
step :: Ctl -> SymStack Ctl
step ctl = case (code ctl) !! (counter ctl) of
  STOP      -> do
    log ctl
    stop
  ADD       -> incr ctl add
  SUB       -> incr ctl sub
  MUL       -> incr ctl mul
  DIV       -> incr ctl div
  MOD       -> incr ctl mod
  LT        -> incr ctl lt
  GT        -> incr ctl gt
  EQ        -> incr ctl eq
  ISZERO    -> incr ctl iszero
  AND       -> incr ctl and
  OR        -> incr ctl or
  PUSH n    -> incr ctl (push (SymInt . fromIntegral $ n))
  POP       -> incr ctl pop
  DUPN idx  -> incr ctl (dupn idx)
  SWAPN idx -> incr ctl (swapn idx)
  JUMP      -> jump
  JUMPI     -> jumpi
  READ      -> incr ctl read
