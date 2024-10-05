import Debug.Trace
import Control.Exception

linearSearch list = linearSearch' (zip [0..] list)

linearSearch' :: (Eq a) => [(i, a)] -> a -> Maybe i
linearSearch' (x:xs) target
  | snd x == target = Just (fst x)
  | null xs = Nothing
  | otherwise = linearSearch' xs target

-- two sum
twoSum list target = twoSum' list target 0 1

twoSum' :: [Int] -> Int -> Int -> Int -> Maybe (Int, Int)
twoSum' list target y x
  | null list || (y+1) >= length list = Nothing
  | x >= length list = twoSum' list target (y + 1) (y + 2)
  | otherwise = case i of
      Nothing -> twoSum' list target y (x + 1) -- keep going
      Just i -> Just (y, x) -- happy case
  where
    i = linearSearch (drop (y+1) list) (target - list !! y)

-- Testing

tc1 = twoSum [2,7,11,15] 9
tc2 = twoSum [3,2,4] 6
tc3 = twoSum [3,3] 6

test = do
    assert( tc1 == Just (0,1) ) "ok"
    assert( tc2 == Just (1,2) ) "ok"
    assert( tc3 == Just (0,1) ) "ok"
