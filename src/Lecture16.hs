--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 16: Functors & Applicatives (cont.)                                --
--------------------------------------------------------------------------------

module Lecture16 where

import Prelude hiding (Either(..), (<*), (*>), lookup)

--------------------------------------------------------------------------------
-- Initial Functors + Applicatives examples

ex0 :: (Int -> Int -> Int -> a) -> Maybe a
ex0 f = f <$> Just 1 <*> Just 2 <*> Just 3

ex1 :: (Int -> Int -> Int -> a) -> Maybe a
ex1 f = f <$> Just 1 <*> Nothing <*> Just 3

ex2 :: (a -> b -> c) -> a -> a -> b -> b -> b -> [c]
ex2 g x y a b c = g <$> [x,y] <*> [a,b,c]

ex3 :: Num a => a -> a -> a -> a -> a -> [a]
ex3 u v a b c = (\x y z -> x+y+z) <$> [u,v] <*> [a,b,c] <*> [1,2]

--------------------------------------------------------------------------------
-- Mystery function

mystery :: Applicative f => Int -> f a -> f [a]
mystery 0 _ = pure []
mystery n f = (:) <$> f <*> mystery (n-1) f

ex4 :: Applicative f => f a -> f [a]
ex4 f = mystery 0 f

ex5 :: Applicative f => f a -> f [a]
ex5 f = mystery 1 f

ex6 :: Applicative f => f a -> f [a]
ex6 f = mystery 2 f

ex7 :: [[Char]]
ex7 = mystery 2 ['1', '2']

--------------------------------------------------------------------------------
-- (<*) and (*>)

(<*) :: Applicative f => f a -> f b -> f a
a0 <* a1 = const <$> a0 <*> a1

(*>) :: Applicative f => f a -> f b -> f b
a0 *> a1 = flip const <$> a0 <*> a1

ex8 :: Num a => Maybe a
ex8 = Just 4 <* Just 8

ex9 :: Num a => Maybe a
ex9 = Just 4 <* Nothing

ex10 :: Num a => Maybe a
ex10 = Just 4 *> Just 8

ex11 :: Num a => Maybe a
ex11 = Nothing *> Just 8

--------------------------------------------------------------------------------
-- Either type

data Either e a = Left e | Right a

instance Functor (Either e) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
    pure = Right

    (Left x)  <*> _ = Left x
    (Right f) <*> g = f <$> g

--------------------------------------------------------------------------------
-- Either examples

lookup :: (Show k, Eq k) => k -> [(k,v)] -> Either String v
lookup x [] = Left $ "Key " ++ show x ++ " not found!"
lookup x ((y,v):xs)
    | x==y      = Right v
    | otherwise = lookup x xs

ex12 :: Either String String
ex12 = lookup "address" [("name", "Duckmaster9000"), ("age", "27")]

data Academic = Academic String String String

fromRequestParams :: [(String, String)] -> Either String Academic
fromRequestParams req = Academic <$> lookup "name" req
                                 <*> lookup "office" req
                                 <*> lookup "witter" req

ex13 :: Either String Academic
ex13 = fromRequestParams [("name", "Leeky Boi"), ("office", "MB2.31")]

--------------------------------------------------------------------------------
-- Writer type

data Writer w a = MkWriter (a,w)

runWriter :: Writer w a -> (a,w)
runWriter (MkWriter m) = m

tell :: w -> Writer w ()
tell o = MkWriter ((), o)

instance Functor (Writer w) where
    fmap f (MkWriter (x,o)) = MkWriter (f x, o)

ex14 :: Num b => Writer [a] b
ex14 = fmap (+5) $ MkWriter (4,[])

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,o1) <*> MkWriter (x,o2) = MkWriter (f x, o1 `mappend` o2)

ex15 :: Num a => Writer [a] ()
ex15 = (\_ _ -> ()) <$> tell [1,2] <*> tell [3,4]

--------------------------------------------------------------------------------
-- Logging example

-- | Represents log messages comprised of a source (represented as a `String`)
-- and a message (also represented as a `String`).
data LogMessage = LogM String String deriving Show

logM :: String -> String -> Writer [LogMessage] ()
logM source message = tell [LogM source message]

-- data types from our compiler example
data Expr = Val Int | Plus Expr Expr deriving Show
data Instr = PUSH Int | ADD deriving Show
type Program = [Instr]

comp :: Expr -> Writer [LogMessage] Program
comp (Val n)    = logM "comp" "compiling a value" *> pure [PUSH n]
comp (Plus l r) = logM "comp" "compiling a plus" *>
    ((\p p' -> p ++ p' ++ [ADD]) <$> comp l <*> comp r)

ex16 :: Writer [LogMessage] Program
ex16 = comp (Plus (Val 4) (Val 8))

--------------------------------------------------------------------------------
