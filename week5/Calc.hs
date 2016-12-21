{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Week5.Calc where

import Week5.ExprT
import Week5.Parser
import qualified Week5.StackVM as StackVM

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr string =
    case parseExp Lit Add Mul string of
        Just exp ->
            Just $ eval exp
        Nothing ->
            Nothing

class Expr a where
    add :: a -> a -> a
    lit :: Integer -> a
    mul :: a -> a -> a

instance Expr ExprT where
    add = Add
    lit = reify . Lit
    mul = Mul

reify :: ExprT -> ExprT
reify = id

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr Integer where
    add = (+)
    mul = (*)
    lit = id

instance Expr Bool where
    add = (||)
    lit = (> 0)
    mul = (&&)

instance Expr MinMax where
    add = max
    lit = MinMax
    mul = min

instance Expr Mod7 where
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    lit a = Mod7 (a `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

instance Expr StackVM.Program where
    add a b = a ++ b ++ [StackVM.Add]
    lit a = [StackVM.PushI a]
    mul a b = a ++ b ++ [StackVM.Mul]

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger =
    testExp :: Maybe Integer
testBool =
    testExp :: Maybe Bool
testMM =
    testExp :: Maybe MinMax
testSat =
    testExp :: Maybe Mod7

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul
