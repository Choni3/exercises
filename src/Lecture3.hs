{-# LANGUAGE InstanceSigs #-}

{- |
Module                  : Lecture3
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 3 of the Haskell Beginners course.

In this module you're going to practice standard Haskell typeclasses:

  * Deriving instances
  * Using typeclasses methods
  * Implementing instances manually
  * Becoming friends with Semigroup, Monoid, Foldable and Functor typeclasses!

-}

module Lecture3
    ( Weekday (..)
    , toShortString
    , next
    , daysTo

    , Gold (..)
    , Reward (..)
    , List1 (..)
    , Treasure (..)

    , appendDiff3
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

-- $setup
-- >>> import Data.Semigroup

{- | Let's define a simple enumeration data type for representing days
of the week.
-}
data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq, Enum, Bounded)

{- | Write a function that will display only the first three letters
of a weekday.

>>> toShortString Monday
"Mon"
-}
toShortString :: Weekday -> String
toShortString = take 3 . show

{- | Write a function that returns next day of the week, following the
given day.

>>> next Monday
Tuesday

♫ NOTE: Implement this function without pattern matching on every
  constructor! Use standard typeclasses instead (you may need to derive
  them first).

🕯 HINT: Check 'Enum' and 'Bounded' typeclasses.

🆙 Bonus challenge 1: Could you implement this function in a such way
  that it'll still work even if you change constructor names and their
  order in the 'Weekday' type?

🆙 Bonus challenge 2: Now, could you improve the implementation so it
  would work for **any** enumeration type in Haskell (e.g. 'Bool',
  'Ordering') and not just 'Weekday'?
-}

next :: (Enum a, Bounded a, Eq a) => a -> a
next x
  | x == maxBound = minBound
  | otherwise     = succ x


prev :: (Enum a, Bounded a, Eq a) => a -> a
prev x
  | x == minBound = maxBound
  | otherwise     = succ x

{- | Implement a function that calculates number of days from the first
weekday to the second.

>>> daysTo Monday Tuesday
1
>>> daysTo Friday Wednesday
5
-}
daysTo :: Weekday -> Weekday -> Int
daysTo x y | x == y = 0
daysTo a b | next a == b = 1
daysTo a b = 1 + daysTo (next a) b

{-

In the following block of tasks you need to implement 'Semigroup'
instances for all types and 'Monoid' instances if it's possible to
have a lawful 'Monoid' instance.

-}

newtype Gold = Gold
    { unGold :: Int
    } deriving (Show, Eq)

-- | Addition of gold coins.
instance Semigroup Gold where
    Gold x <> Gold y = Gold (x + y)

instance Monoid Gold where
    mempty = Gold 0


{- | A reward for completing a difficult quest says how much gold
you'll receive and whether you'll get a special reward.

If you combine multiple rewards, the final reward will contain a
special prize if at least one of the rewards is special.
-}
data Reward = Reward
    { rewardGold    :: Gold
    , rewardSpecial :: Bool
    } deriving (Show, Eq)

instance Semigroup Reward where
    (Reward x a) <> (Reward y b) = Reward (x <> y) (a || b)

instance Monoid Reward where
    mempty = Reward (Gold 0) False

{- | 'List1' is a list that contains at least one element.
-}
data List1 a = List1 a [a]
    deriving (Show, Eq)

-- | This should be list append.
instance Semigroup (List1 a) where
    (List1 a as) <> (List1 b bs) = List1 a (as ++ [b] ++ bs)


{- | Does 'List1' have the 'Monoid' instance? If no then why?

instance Monoid (List1 a) where
-}

{- | When fighting a monster, you can either receive some treasure or
don't.
-}
data Treasure a
    = NoTreasure
    | SomeTreasure a
    deriving (Show, Eq)

{- | When you append multiple treasures for fighting multiple
monsters, you should get a combined treasure and not just the first
(or last one).

🕯 HINT: You may need to add additional constraints to this instance
  declaration.
-}
instance Semigroup a => Semigroup (Treasure a) where
    (<>) x y = case x of 
        NoTreasure -> y
        SomeTreasure m -> case y of 
            NoTreasure -> SomeTreasure m
            SomeTreasure n -> SomeTreasure (m <> n)


instance Semigroup a => Monoid (Treasure a) where
    mempty = NoTreasure



{- | Abstractions are less helpful if we can't write functions that
use them!

Implement a polymorphic function that takes three elements and appends
together only different elements.

>>> appendDiff3 [1] [3, 2] [0, 5]
[1,3,2,0,5]
>>> appendDiff3 [4] [2, 2] [2, 2]
[4,2,2]
>>> appendDiff3 [1 .. 5] [1 .. 5] [1 .. 5]
[1,2,3,4,5]
>>> appendDiff3 (Product 2) (Product 3) (Product 3)
Product {getProduct = 6}

-}
appendDiff2 :: (Eq a, Semigroup a) => a -> a -> a
appendDiff2 m n | m == n = m
appendDiff2 m n = m <> n

appendDiff3 :: (Eq a, Semigroup a) => a -> a -> a -> a
appendDiff3 m n p | m == p = appendDiff2 m n
                  | n == p = appendDiff2 m n
                  | m == n = appendDiff2 m p
appendDiff3 m n p = appendDiff2 m (appendDiff2 n p)


{-

In the next block of tasks, implement 'Foldable' instances for all
types that can have such an instance.

♫ NOTE: Implement both 'foldr' and 'foldMap' methods. On the one hand,
  'Foldable' is a big typeclass but lets focus on its small part to get
  the main idea. On the other hand, these two methods are quite
  different so it's a good practice.

🕯 HINT: Check kinds of types to see whether it's possible to implement
  an instance of 'Foldable'.

🕯 HINT: If you don't feel comfortable with kinds yet, alternatively
  you can try uncommenting each instance one by one and see the GHC
  error. The compiler will "kindly" tell you if it's impossible to have
  such an instance.

🕯 HINT: Write explicit type signature of methods using InstanceSigs
  (already enabled in this module).

♫ NOTE: Since the instances are commented, the tests are also commented.
  To run tests for your instances, go to the "test/Test/Lecture3.hs" file
  and uncomment all commented tests. But do this only after you
  implement instances! No spoilers :)
-}

-- instance Foldable t where
-- foldr :: (a -> b -> b) -> b -> t a-> b
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- 
-- return ->  :: a -> m a 
-- >>= :: m a -> (a -> m b) -> m b
-- 

-- instance Foldable Reward where
instance Foldable List1 where
    foldr :: (a -> b -> b) -> b -> List1 a -> b
    foldr f start (List1 x []) = f x start 
    foldr f start (List1 x y) = foldr f (f x start) y
    -- foldMap :: Monoid m => (a -> m) -> List1 a -> m
    -- foldMap (\(List1 x y) -> if y == [] then x)
instance Foldable Treasure where
    foldr :: (a -> b -> b) -> b -> Treasure a -> b
    foldr _ ini NoTreasure = ini
    foldr f ini (SomeTreasure x) = f x ini

instance Functor List1 where
    fmap :: (a -> b) -> List1 a -> List1 b
    fmap f (List1 x []) = List1 (f x) []
    fmap f (List1 x y) = List1 (f x) (fmap f y)


instance Functor Treasure where
    fmap :: (a -> b) -> Treasure a -> Treasure b
    fmap _ NoTreasure = NoTreasure
    fmap f (SomeTreasure x) = SomeTreasure (f x)
    