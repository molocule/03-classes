{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module MonoidFoldable where

import qualified Data.List as List
import Test.HUnit
import Prelude hiding (all, and, any, or)

foldList :: Monoid b => [b] -> b
foldList = List.foldr (<>) mempty

tm0 :: Test
tm0 = foldList ["C", "I", "S", "5", "5", "2"] ~?= "CIS552"

newtype And = And {getAnd :: Bool} deriving (Eq, Show)

newtype Or = Or {getOr :: Bool} deriving (Eq, Show)

anyT1 :: Test
anyT1 = getOr (foldList (fmap Or [True, False, True])) ~?= True

allT2 :: Test
allT2 = getAnd (foldList (fmap And [True, False, True])) ~?= False

instance Semigroup And where
  (<>) = undefined

instance Monoid And where
  mempty = undefined

instance Semigroup Or where
  (<>) = undefined

instance Monoid Or where
  mempty = undefined

monoidAnd :: Test
monoidAnd =
  TestList
    [ And False <> (And False <> And False) ~?= (And False <> And False) <> And False,
      And False <> (And False <> And True) ~?= (And False <> And False) <> And True,
      And False <> (And True <> And False) ~?= (And False <> And True) <> And False,
      And False <> (And True <> And True) ~?= (And False <> And True) <> And True,
      And True <> (And False <> And False) ~?= (And True <> And False) <> And False,
      And True <> (And False <> And True) ~?= (And True <> And False) <> And True,
      And True <> (And True <> And False) ~?= (And True <> And True) <> And False,
      And True <> (And True <> And True) ~?= (And True <> And True) <> And True,
      And True <> mempty ~?= And True,
      And False <> mempty ~?= And False,
      mempty <> And True ~?= And True,
      mempty <> And False ~?= And False
    ]

monoidOr :: Test
monoidOr =
  TestList
    [ Or False <> (Or False <> Or False) ~?= (Or False <> Or False) <> Or False,
      Or False <> (Or False <> Or True) ~?= (Or False <> Or False) <> Or True,
      Or False <> (Or True <> Or False) ~?= (Or False <> Or True) <> Or False,
      Or False <> (Or True <> Or True) ~?= (Or False <> Or True) <> Or True,
      Or True <> (Or False <> Or False) ~?= (Or True <> Or False) <> Or False,
      Or True <> (Or False <> Or True) ~?= (Or True <> Or False) <> Or True,
      Or True <> (Or True <> Or False) ~?= (Or True <> Or True) <> Or False,
      Or True <> (Or True <> Or True) ~?= (Or True <> Or True) <> Or True,
      Or True <> mempty ~?= Or True,
      Or False <> mempty ~?= Or False,
      mempty <> Or True ~?= Or True,
      mempty <> Or False ~?= Or False
    ]

and :: Foldable t => t Bool -> Bool
and = getAnd . foldMap And

or :: Foldable t => t Bool -> Bool
or = undefined

all :: Foldable t => (a -> Bool) -> t a -> Bool
all f = undefined

any :: Foldable t => (a -> Bool) -> t a -> Bool
any f = undefined

tf0 :: Test
tf0 = or [True, False, False, True] ~?= True

tf1 :: Test
tf1 = all (> 0) [1 :: Int, 2, 4, 18] ~?= True

tf2 :: Test
tf2 = all (> 0) [1 :: Int, -2, 4, 18] ~?= False

tf3 :: Test
tf3 = any (> 0) [1 :: Int, 2, 4, 18] ~?= True

tf4 :: Test
tf4 = any (> 0) [-1 :: Int, -2, -4, -18] ~?= False

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Functor)

t1 :: Tree String
t1 = Branch "d" (Branch "b" (l "a") (l "c")) (Branch "f" (l "e") (l "g"))
  where
    l x = Branch x Empty Empty

instance Foldable Tree where
  foldMap = undefined

tt1 :: Test
tt1 = all ((== 1) . length) t1 ~?= True

tt2 :: Test
tt2 = undefined

main :: IO ()
main = do
  _ <- runTestTT $ TestList [tm0, anyT1, allT2, monoidAnd, monoidOr, tf0, tf1, tf2, tf3, tf4, tt1]
  return ()
