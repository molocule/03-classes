module Classes where

import Data.Char
import Test.HUnit
import Text.Read
import Prelude hiding (lookup)

fancySeven :: Int
fancySeven = 3 + 4

fancyEight :: Float
fancyEight = 3.2 + 4.8

data PrimaryColor = Red | Green | Blue

instance Eq PrimaryColor where
  Red == Red = True
  Blue == Blue = True
  Green == Green = True
  _ == _ = False

  Red /= Red = False
  Blue /= Blue = False
  Green /= Green = False
  _ /= _ = True

fancyTrue :: Bool
fancyTrue = Red == Red

data Tree a = Empty | Branch a (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
  t1 == t2 = undefined

tree1, tree2 :: Tree Int
tree1 = Branch 3 (Branch 2 Empty Empty) (Branch 1 Empty Empty)
tree2 = Branch 3 Empty Empty

testTreeEq :: Test
testTreeEq =
  TestList
    [ "tree1 == tree1" ~: tree1 == tree1 ~?= True,
      "tree1 /= tree2" ~: tree1 == tree2 ~?= False,
      "tree1 /= Empty" ~: tree1 == Empty ~?= False
    ]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup a ((a', b) : ps) =
  if a == a'
    then Just b
    else lookup a ps

lookupDefault x xs def = case lookup x xs of
  Just y -> y
  Nothing -> def

data Point = Point Double Double
  deriving (Eq)

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Eq)

data IntFunctions
  = OneArg (Int -> Int)
  | TwoArg (Int -> Int -> Int)

data SadColors = Black | Brown | Grey
  deriving (Eq, Show, Read)

instance Num a => Num [a] where
  fromInteger = repeat . fromInteger
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  negate = map negate
  abs = map abs
  signum = map signum

one :: [Int]
one = 1 -- the fromInteger instance converts the Integer 1 into a list
-- by first converting it to an Int and then repeating it
-- as an infinite list.

seven :: [Int]
seven = 3 + 4

tenToThirty :: [Int]
tenToThirty = [10 .. 30]

abcde :: [Char]
abcde = ['a' .. 'e']

biggestInt :: Int
biggestInt = maxBound

instance Functor Tree where
  fmap = treeMap
    where
      treeMap f Empty = Empty
      treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)

data Two a = MkTwo a a deriving (Eq, Show, Read, Ord)

instance Functor Two where
  fmap = undefined

main :: IO ()
main = do
  putStrLn "This is the Classes lecture. What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
  putStrLn "Now running tests."
  _ <- runTestTT testTreeEq
  return ()

nop :: IO ()
nop = do
  return ()
