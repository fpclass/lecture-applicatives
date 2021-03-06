--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Applicative functors                                              --
--------------------------------------------------------------------------------

module Lecture where

import Prelude hiding (Applicative(..), (<*), (*>), lookup)

--------------------------------------------------------------------------------
-- Applicatives

infixl 4 <*>
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure = Just

    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing

maybeEx0 :: (a -> b) -> a -> Maybe b 
maybeEx0 f x = Just f <*> Just x 

maybeEx1 :: a -> Maybe b 
maybeEx1 x = Nothing <*> Just x

maybeEx2 :: (a -> b) -> Maybe b 
maybeEx2 f = Just f <*> Nothing 

instance Applicative (Either e) where 
    pure = Right 

    Left err <*> _ = Left err 
    Right f  <*> x = f <$> x

eitherEx0 :: (a -> b) -> a -> Either e b 
eitherEx0 f x = Right f <*> Right x 

eitherEx1 :: e -> a -> Either e b 
eitherEx1 err x = Left err <*> Right x

eitherEx2 :: (a -> b) -> e -> Either e b 
eitherEx2 f err = Right f <*> Left err 

--------------------------------------------------------------------------------
-- (<*) and (*>)

(<*) :: Applicative f => f a -> f b -> f a
a0 <* a1 = const <$> a0 <*> a1

(*>) :: Applicative f => f a -> f b -> f b
a0 *> a1 = flip const <$> a0 <*> a1

ex0 :: Num a => Maybe a
ex0 = Just 4 <* Just 8

ex1 :: Num a => Maybe a
ex1 = Just 4 <* Nothing

ex2 :: Num a => Maybe a
ex2 = Just 4 *> Just 8

ex3 :: Num a => Maybe a
ex3 = Nothing *> Just 8

--------------------------------------------------------------------------------
-- Example: Logging

-- the expression language and the instruction set for the stack-based machine
-- from one of the previous lectures
data Expr = Val Int | Plus Expr Expr deriving Show 
data Instr = PUSH Int | ADD deriving Show 
type Program = [Instr]

compWithLogging :: Expr -> (Program, [String])
compWithLogging (Val n)    = ([PUSH n], ["compiling a value"])
compWithLogging (Plus l r) = (pl++pr++[ADD], "compiling a plus" : (ml++mr))
    where (pl, ml) = compWithLogging l
          (pr, mr) = compWithLogging r 

--------------------------------------------------------------------------------
-- Writer type

data Writer w a = MkWriter (a,w) deriving Show

instance Functor (Writer w) where
    fmap f (MkWriter (x,o)) = MkWriter (f x, o)

ex4 :: Num b => Writer [a] b
ex4 = fmap (+5) $ MkWriter (4,[])

writeLog :: String -> Writer [String] ()
writeLog msg = MkWriter ((), [msg])

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,o1) <*> MkWriter (x,o2) = MkWriter (f x, o1 <> o2)

comp :: Expr -> Writer [String] Program
comp (Val n)    = writeLog "compiling a value" *> pure [PUSH n]
comp (Plus l r) = writeLog "compiling a plus" *>
    ((\p p' -> p ++ p' ++ [ADD]) <$> comp l <*> comp r)

compEx :: Writer [String] Program
compEx = comp (Plus (Val 4) (Val 8))

--------------------------------------------------------------------------------
