module Set
(
) where


{--
powerSet :: [a] -> [[a]]
powerSet (x:xs) = xs' ++ map (x:) xs'
  where xs' = powerSet xs
--}
