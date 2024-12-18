import Data.Function.Memoize

digits :: Int -> Int
digits = length . show

upper :: Int -> Int
upper x = x `div` (10 ^ (digits x `div` 2))

lower :: Int -> Int
lower x = x `mod` (10 ^ (digits x `div` 2))

-- length of blink, fast
-- generations, num -> length
lbf :: Int -> Int -> Int
lbf 0 _ = 1
lbf gen x
  | x == 0 = lbfMemo (gen - 1) 1
  | even (digits x) = lbfMemo (gen - 1) (upper x) + lbfMemo (gen - 1) (lower x)
  | otherwise = lbfMemo (gen - 1) (x * 2024)

lbfMemo = memoize2 lbf

part1 :: String -> Int
part1 inp = sum (map (lbfMemo 25 . read) (words inp))

part2 :: String -> Int
part2 inp = sum (map (lbfMemo 75 . read) (words inp))

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)