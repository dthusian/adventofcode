import Data.Bifunctor
import Debug.Trace
import Data.List

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn x l =
  let recur :: Eq a => [a] -> [a] -> [a] -> [[a]]
      recur _ [] f = [reverse f]
      recur x l f = if take (length x) l == x then
        reverse f : recur x (drop (length x) l) []
      else
        recur x (tail l) (head l : f)
  in recur x l []

splitOnce :: Eq a => [a] -> [a] -> ([a], [a])
splitOnce x l =
  let s = splitOn x l
  in (s !! 0, s !! 1)

matchRule :: [Integer] -> (Integer, Integer) -> Bool
matchRule [] _ = True
matchRule (l:ls) (a, b)
  | l == a = True
  | l == b = notElem a ls
  | otherwise = matchRule ls (a, b)

middle :: [a] -> a
middle x = x !! div (length x) 2

parse :: String -> ([(Integer, Integer)], [[Integer]])
parse inp =
  let (rules_inp, pages_inp) = splitOnce "\n\n" inp
      rules = map (bimap read read . splitOnce "|") (lines rules_inp)
      pages = map (map read . splitOn ",") (lines pages_inp)
  in (rules, pages)

part1 :: String -> Integer
part1 inp =
  let (rules, pages) = parse inp
  in (sum . map middle . filter (\x -> all (matchRule x) rules)) pages

part2 :: String -> Integer
part2 inp =
  let (rules, pages) = parse inp
      failers = filter (\x -> not (all (matchRule x) rules)) pages
      sorter a b | elem (a, b) rules = LT
                 | elem (b, a) rules = GT
                 | otherwise = EQ
  in sum (map (middle . sortBy sorter) failers)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)