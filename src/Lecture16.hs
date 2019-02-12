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
