module Tuple
( Triplet (..)
, first
, second
, third
, rotateLeft
) where
  
newtype Triplet a = Triplet { getTriplet :: (a, a, a) } deriving Show

instance Functor Triplet where
  fmap f (Triplet (a0, a1, a2)) = Triplet (f a0, f a1, f a2)

first :: Triplet a -> a
first (Triplet (a, _, _)) = a

second :: Triplet a -> a
second (Triplet (_, a, _)) = a

third :: Triplet a -> a
third (Triplet (_, _, a)) = a
  
rotateLeft :: Triplet a -> Triplet a
rotateLeft (Triplet (a0, a1, a2)) = Triplet (a1, a2, a0)
