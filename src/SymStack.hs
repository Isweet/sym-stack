module SymStack where

import Prelude hiding (LT, GT, EQ)

import qualified Data.Set as S
import qualified Data.Map as M

import Z3.Monad hiding (Symbol, Model, check)

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

data Symbol =
    Atom String
  | SymInt Integer
  | SymBool Bool
  | Add Symbol Symbol
  | Sub Symbol Symbol
  | Mul Symbol Symbol
  | Div Symbol Symbol
  | Mod Symbol Symbol
  | Less Symbol Symbol
  | Greater Symbol Symbol
  | Equal Symbol Symbol
  | And Symbol Symbol
  | Or Symbol Symbol
  | Not Symbol deriving ( Eq, Ord )

instance Show Symbol where
    show (Atom name) = name
    show (SymInt n) = show n
    show (SymBool b) = show b
    show (Add s1 s2) = "(" ++ (show s1) ++ " + " ++ (show s2) ++ ")"
    show (Sub s1 s2) = "(" ++ (show s1) ++ " - " ++ (show s2) ++ ")"
    show (Mul s1 s2) = "(" ++ (show s1) ++ " * " ++ (show s2) ++ ")"
    show (Div s1 s2) = "(" ++ (show s1) ++ " / " ++ (show s2) ++ ")"
    show (Mod s1 s2) = "(" ++ (show s1) ++ " % " ++ (show s2) ++ ")"
    show (Less s1 s2) = "(" ++ (show s1) ++ " < " ++ (show s2) ++ ")"
    show (Greater s1 s2) = "(" ++ (show s1) ++ " > " ++ (show s2) ++ ")"
    show (Equal s1 s2) = "(" ++ (show s1) ++ " = " ++ (show s2) ++ ")"
    show (And s1 s2) = "(" ++ (show s1) ++ " && " ++ (show s2) ++ ")"
    show (Or s1 s2) = "(" ++ (show s1) ++ " || " ++ (show s2) ++ ")"
    show (Not s) = "~" ++ (show s)

type Code = [Instr]
type Counter = Int
type Stack = [Symbol]
type Condition = Symbol

data State = State { code :: Code, counter :: Counter, stack :: Stack, pc :: Condition } deriving ( Show, Eq, Ord )

type Model = M.Map String Integer

{- Helpers -}
fresh :: Stack -> Symbol -> Symbol
fresh stk pc = Atom . head $ filter (not . (dirty stk pc)) (map (\idx -> "x" ++ (show idx)) [1..])
    where
        dirty stk pc curr = (used curr pc) || (any (used curr) stk)
        used curr s = case s of
            Atom name -> curr == name
            SymInt _ -> False
            SymBool _ -> False
            Add s1 s2 -> (used curr s1) || (used curr s2)
            Sub s1 s2 -> (used curr s1) || (used curr s2)
            Mul s1 s2 -> (used curr s1) || (used curr s2)
            Div s1 s2 -> (used curr s1) || (used curr s2)
            Mod s1 s2 -> (used curr s1) || (used curr s2)
            Less s1 s2 -> (used curr s1) || (used curr s2)
            Greater s1 s2 -> (used curr s1) || (used curr s2)
            Equal s1 s2 -> (used curr s1) || (used curr s2)
            And s1 s2 -> (used curr s1) || (used curr s2)
            Or s1 s2 -> (used curr s1) || (used curr s2)
            Not s1 -> (used curr s1)

symToZ3 :: Condition -> Z3 AST
symToZ3 (Atom name) = (mkStringSymbol name) >>= mkIntVar
symToZ3 (SymInt n) = mkInteger n
symToZ3 (SymBool b) = mkBool b
symToZ3 (Add s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkAdd [z1, z2]
symToZ3 (Sub s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkSub [z1, z2]
symToZ3 (Mul s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkMul [z1, z2]
symToZ3 (Div s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkDiv z1 z2
symToZ3 (Mod s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkMod z1 z2
symToZ3 (Less s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkLt z1 z2
symToZ3 (Greater s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkGt z1 z2
symToZ3 (Equal s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkEq z1 z2
symToZ3 (And s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkAnd [z1, z2]
symToZ3 (Or s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkOr [z1, z2]
symToZ3 (Not s) = (symToZ3 s) >>= mkNot

sat :: Condition -> IO (Maybe String)
sat pc = evalZ3 $ do
    pcZ3 <- symToZ3 pc
    assert pcZ3
    m <- getModel >>= return . snd
    case m of
        Just m' -> do
            mStr <- showModel m'
            return $ Just mStr
        Nothing -> 
            return Nothing

arith :: State -> (Symbol -> Symbol -> Symbol) -> S.Set State
arith s f = S.singleton $ s { counter = c', stack = s', pc = pc' }
    where
        c' = (counter s) + 1
        x1 : x2 : ss = (stack s)
        x' = fresh (stack s) (pc s)
        s' = x' : ss
        pc' = And (pc s) (Equal x' (f x1 x2))

boolBin :: State -> (Symbol -> Symbol -> Symbol) -> IO (S.Set State)
boolBin s f = do
    t <- sat pc't
    f <- sat pc'f
    case (t, f) of
        (Nothing, Nothing) -> return S.empty
        (Just _, Nothing) -> return . S.singleton $ sTrue
        (Nothing, Just _) -> return . S.singleton $ sFalse
        (Just _, Just _) -> return . S.fromList $ [sTrue, sFalse]
        where
            c' = (counter s) + 1
            x1 : x2 : ss = (stack s)
            x' = fresh (stack s) (pc s)
            stk' = x' : ss
            pc't = And (pc s) (f x1 x2)
            pc'f = And (pc s) (Not (f x1 x2))
            s' = s { counter = c', stack = stk' }
            sTrue = s' { pc = And pc't (Equal x' (SymInt 1)) }
            sFalse = s' { pc = And pc'f (Equal x' (SymInt 0)) }

boolUn :: State -> (Symbol -> Symbol) -> IO (S.Set State)
boolUn s f = do
    t <- sat pc't
    f <- sat pc'f
    case (t, f) of
        (Nothing, Nothing) -> return S.empty
        (Just _, Nothing) -> return . S.singleton $ sTrue
        (Nothing, Just _) -> return . S.singleton $ sFalse
        (Just _, Just _) -> return . S.fromList $ [sTrue, sFalse]
        where
            c' = (counter s) + 1
            x1 : ss = (stack s)
            x' = fresh (stack s) (pc s)
            stk' = x' : ss
            pc't = And (pc s) (f x1)
            pc'f = And (pc s) (Not (f x1))
            s' = s { counter = c', stack = stk' }
            sTrue = s' { pc = And pc't (Equal x' (SymInt 1)) }
            sFalse = s' { pc = And pc'f (Equal x' (SymInt 0)) }

check :: State -> Symbol -> IO Bool
check s cond = do
    check <- sat (And (pc s) cond)
    case check of
        Just m -> do
            putStrLn "--- ERROR STATE ---\n"
            print s
            putStrLn ""
            print cond
            putStrLn ""
            putStrLn m
            return True
        Nothing ->
            return False

eval :: State -> IO ()
eval s = eval' $ S.singleton s
    where
        eval' ss =
            if S.null ss then
                return ()
            else do
                res <- S.unions <$> mapM step (S.toList ss)
                eval' res

step :: State -> IO (S.Set State)
step s = case (code s) !! (counter s) of
    STOP -> return S.empty
    ADD -> return $ arith s Add
    SUB -> return $ arith s Sub
    MUL -> return $ arith s Mul
    DIV -> do
        let x1 : x2 : ss = (stack s)
        let cond = Equal x2 (SymInt 0)
        div0 <- check s cond
        if div0 then
            return S.empty
        else do
            let cand = arith s Div
            return . (S.map (\c -> c { pc = And (pc c) (Not cond) })) $ cand
    MOD -> do
        let x1 : x2 : ss = (stack s)
        let cond = Equal x2 (SymInt 0)
        div0 <- check s cond
        if div0 then
            return S.empty
        else do
            let cand = arith s Mod
            return . (S.map (\c -> c { pc = And (pc c) (Not cond) })) $ cand
    LT -> boolBin s Less
    GT -> boolBin s Greater
    EQ -> boolBin s Equal
    ISZERO -> boolUn s (\ s -> Equal s (SymInt 0))
    AND -> boolBin s (\ s1 s2 -> And (Not (Equal s1 (SymInt 0))) (Not (Equal s2 (SymInt 0))))
    OR -> boolBin s (\ s1 s2 -> Or (Not (Equal s1 (SymInt 0))) (Not (Equal s2 (SymInt 0))))
    PUSH n -> return . S.singleton $ s { counter = (counter s) + 1, stack = (SymInt . fromIntegral $ n) : (stack s) }
    POP -> return . S.singleton $ s { counter = (counter s) + 1, stack = s' }
        where
            _ : s' = (stack s)
    DUPN idx -> return . S.singleton $ s { counter = (counter s) + 1, stack = ((stack s) !! (idx - 1)) : (stack s) }
    SWAPN idx -> return . S.singleton $ s { counter = (counter s) + 1, stack = s' }
        where
            stk = (stack s)
            s' = map (\ (i, x) -> if i == 0 then stk !! idx else if i == idx then stk !! 0 else x) (zip [0..] stk)
    READ -> return . S.singleton $ s { counter = (counter s) + 1, stack = (fresh (stack s) (pc s)) : (stack s) }
