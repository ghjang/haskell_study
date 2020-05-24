module Problem_1_2 () where

-- by using a recursion
accSum :: (Num a) => [a] -> [a]
accSum xs = impl 0 xs
  where
    impl _ [] = []
    impl acc (x:xs) = let s = (acc + x)
                      in s : impl s xs

-- by using the 'foldl' function
accSum' :: (Num a) => [a] -> [a]
accSum' xs = snd $ foldl f (0, []) xs
  where
    f (acc, ss) n = let newAcc = (acc + n)
                    in (newAcc, ss ++ [newAcc])
                    
----------------------------------------------------------
-- by using the 'foldl' function and difference list idiom
----------------------------------------------------------

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

-- associativity is required by Semigroup.
instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

{-
  identity value is required by Monoid.
  'mappend' is implemented automatically in terms of the Semigroup's '<>' function.
 -}
instance Monoid (DiffList a) where
  mempty = DiffList id

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

accSum'' :: (Num a) => [a] -> [a]
accSum'' xs = fromDiffList . snd $ foldl f (0, mempty) xs
  where
    f (acc, ss) n = let newAcc = (acc + n)
                    in (newAcc, ss `mappend` toDiffList [newAcc])
