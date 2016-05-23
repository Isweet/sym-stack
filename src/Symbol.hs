module Symbol where

import System.IO.Unsafe

import Z3.Monad hiding (Symbol)

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
  | Ite Symbol Symbol Symbol
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
    show (Ite b s1 s2) = "(if " ++ (show b) ++ " then " ++ (show s1) ++ " else " ++ (show s2) ++ ")"
    show (And s1 s2) = "(" ++ (show s1) ++ " && " ++ (show s2) ++ ")"
    show (Or s1 s2) = "(" ++ (show s1) ++ " || " ++ (show s2) ++ ")"
    show (Not s) = "~" ++ (show s)
    
symToZ3 :: Symbol -> Z3 AST
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
symToZ3 (Ite b s1 s2) = do
    b' <- symToZ3 b
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkIte b' z1 z2
symToZ3 (And s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkAnd [z1, z2]
symToZ3 (Or s1 s2) = do
    z1 <- symToZ3 s1
    z2 <- symToZ3 s2
    mkOr [z1, z2]
symToZ3 (Not s) = (symToZ3 s) >>= mkNot

sat :: Symbol -> Maybe (String)
sat cond = unsafePerformIO . evalZ3 $ do
  condZ3 <- symToZ3 cond
  assert condZ3
  m <- getModel >>= return . snd
  traverse showModel m
