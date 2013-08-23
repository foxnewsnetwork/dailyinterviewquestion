module Solution where
-- My solution to the selection problem... by my solution, I really mean
-- Professor Richard Bird's solution which I merely shamefully copied

kth :: (Ord x) => [x] -> Int -> Maybe x
kth [] _      = Nothing
kth (x:_) 0   = Just x
kth (_:xs) k  = kth xs (k-1)

smallestK :: (Ord x) => [x] -> [x] -> Int -> Maybe x
smallestK [] ys k = ys `kth` k
smallestK xs [] k = xs `kth` k

-- smallestK xs ys k = union (xs1 ++ [x] ++ xs2) (ys1 ++ [y] ++ ys2) `kth` k
-- let (xs2_1 xs2_2) = splitWith ys1 xs2
-- where splitWith (y:_) xs2 