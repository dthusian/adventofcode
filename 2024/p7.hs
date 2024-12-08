import Data.List.Split

parse :: String -> [(Integer, [Integer])]
parse x = map (\x ->
  let s = splitOn ": " x
  in (read (s !! 0), map read (words (s !! 1)))) (lines x)

resolve1 :: Integer -> [Integer] -> Bool
resolve1 y [] = False
resolve1 y (x:[]) = x == y
resolve1 y (x:(x2:xs)) = resolve1 y ((x + x2) : xs) || resolve1 y ((x * x2) : xs)

part1 :: String -> Integer
part1 inp =
  let parsed = parse inp
  in (sum . map fst . filter (uncurry resolve1)) parsed

iconcat :: Integer -> Integer -> Integer
iconcat a b = b + a * 10 ^ (toInteger . length . show) b

resolve2 :: Integer -> [Integer] -> Bool
resolve2 y [] = False
resolve2 y (x:[]) = x == y
resolve2 y (x:(x2:xs)) = any (\op -> resolve2 y ((x `op` x2) : xs)) [(+), (*), iconcat]

part2 :: String -> Integer
part2 = sum . map fst . filter (uncurry resolve2) . parse

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)