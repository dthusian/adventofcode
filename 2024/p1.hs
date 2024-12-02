import System.IO
import Data.List
import Debug.Trace

part1 x =
  let linez = lines x
      pairs = map words linez
      list2 = transpose pairs
      a = map (read :: String -> Integer) (list2 !! 0)
      b = map (read :: String -> Integer) (list2 !! 1)
      as = sort a
      bs = sort b
      diff = map (\ab -> abs (ab !! 0 - ab !! 1)) (transpose [as, bs])
      sumdiff = sum diff
      ans = sumdiff
  in ans
part1 :: String -> Integer

part1b x = (sum . map (\ab -> abs (ab!!0 - ab!!1)) . transpose . map sort . transpose) (map (map read . words) (lines x))
part1b :: String -> Integer

part2 x = 
  let ab = transpose (map (map read . words) (lines x))
  in (sum . map (\a -> a * ((toInteger . length . filter (\b -> b == a)) (ab !! 1)))) (ab !! 0)
part2 :: String -> Integer

main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  print (part1b contents)
  print (part2 contents)
  hClose file
