import Control.Exception (assert)

firstLast :: [a] -> [a]
firstLast [] = []
firstLast [x] = []
firstLast (x : xs) = init xs

palNum :: Int -> Bool
palNum i = palNum' (show i)

palNum' :: String -> Bool
palNum' s
  | length s <= 1 = True
  | head s == last s = palNum' (firstLast s)
  | otherwise = False

-- Testing
tc1 = palNum 121
tc2 = palNum (-121)
tc3 = palNum 10
tc4 = palNum 123456789987654321
tc5 = palNum 12345678987654321

test = do
    assert tc1  "ok";
    assert (not tc2) "ok";
    assert (not tc3) "ok";
    assert tc4 "ok";
    assert tc5 "ok";
