import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Debug.Trace

char2int :: Char -> Int
char2int '0' = 0
char2int '1' = 1
char2int '2' = 2
char2int '3' = 3
char2int '4' = 4
char2int '5' = 5
char2int '6' = 6
char2int '7' = 7
char2int '8' = 8
char2int '9' = 9
char2int _ = error "Not a digit"

parse :: String -> [Int]
parse inp = concat (parseInner 0 inp)

parseInner :: Int -> String -> [[Int]]
parseInner i [] = []
parseInner i [x] = [replicate (char2int x) i]
parseInner i (x:y:xs) = replicate (char2int x) i : replicate (char2int y) (-1) : parseInner (i + 1) xs

unsnoc :: [a] -> ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) ((fromJust . uncons .reverse) xs)

compress :: [Int] -> [Int]
compress [] = []
compress [x] = [x | x /= (-1)]
compress (x:xs) | x == -1 =
                  let (ys, y) = unsnoc xs
                  in if y == -1 then compress (x : ys)
                    else y : compress ys
                | otherwise = x : compress xs

part1 :: String -> Integer
part1 = sum . zipWith (*) [0..] . map toInteger . compress . parse

type State = ([(Int, Int, Int)], [(Int, Int)])

parse2 :: String -> State
parse2 = parseInner2 0 0

mapBoth :: (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth ma mb (a, b) = (ma a, mb b)

parseInner2 :: Int -> Int -> String -> State
parseInner2 id pos [] = ([], [])
parseInner2 id pos [x] = ([(id, pos, char2int x)], [])
parseInner2 id pos (x:y:t) =
  let cx = char2int x
      cy = char2int y
  in mapBoth ((id, pos, cx):) ((pos + cx, cy):) (parseInner2 (id + 1) (pos + cx + cy) t)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

compress2 :: State -> [(Int, Int, Int)]
compress2 ([], free) = []
compress2 (x:xs, free) =
  let (id, pos, len) = x
      i = findIndex (\(fpos, flen) -> flen >= len && fpos < pos) free
  in case i of
    Just i ->
      let (fpos, flen) = free !! i
          newFree = replaceNth i (fpos + len, flen - len) free
      in (id, fpos, len) : compress2 (xs, newFree)
    Nothing -> x : compress2 (xs, free)

part2 :: String -> Integer
part2 = sum .
  map (\(id, pos, len) -> id * (pos * len + len * (len + 1) `div` 2 - len)) .
  map (\(a, b, c) -> (toInteger a, toInteger b, toInteger c)) .
  compress2 .
  mapBoth reverse id .
  parse2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)