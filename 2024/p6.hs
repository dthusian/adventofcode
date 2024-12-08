import qualified Data.Vector as V
import Data.Vector((!), (//))
import Data.Maybe
import Data.List
import Debug.Trace;

type Grid a = V.Vector (V.Vector a)

data State = State (Int, Int) Int Int Dir (Grid Char)
data Dir = U | D | L | R deriving (Eq)

parse :: String -> Grid Char
parse = V.fromList . map V.fromList . lines

findStart :: Grid Char -> (Int, Int)
findStart x = head (mapMaybe (\i -> listToMaybe (mapMaybe (\j -> if x ! i ! j == '^' then Just (j, i) else Nothing) [0..length (x ! i) - 1] )) [0..length x - 1])

move :: (Int, Int) -> Dir -> (Int, Int)
move pos d | d == U = (fst pos, snd pos - 1)
           | d == D = (fst pos, snd pos + 1)
           | d == L = (fst pos - 1, snd pos)
           | d == R = (fst pos + 1, snd pos)

rotate :: Dir -> Dir
rotate U = R
rotate R = D
rotate D = L
rotate L = U

process :: Int -> State -> [(Int, Int)]
process depth (State pos w h dir grid) =
  let newPos = move pos dir
  in if depth == 0 || fst newPos < 0 || fst newPos >= w || snd newPos < 0 || snd newPos >= h then
      [pos]
    else if grid ! snd newPos ! fst newPos == '#' then
      pos : process (depth - 1) (State pos w h (rotate dir) grid)
    else
      pos : process (depth - 1) (State newPos w h dir grid)

part1 :: String -> Int
part1 inp = 
  let parsed = parse inp
      start = findStart parsed
      w = length (parsed ! 0)
      h = length parsed 
  in (length . nub . process 99999) (State start w h U parsed)

addBlock :: Grid Char -> (Int, Int) -> Grid Char
addBlock g (x, y) = g // [(y, (g ! y) // [(x, '#')])]

part2 :: String -> Int
part2 inp =
  let parsed = parse inp
      start = findStart parsed
      w = length (parsed ! 0)
      h = length parsed
      positions = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  in length (filter (\x -> length (process 100000 (State start w h U (addBlock parsed x))) == 100001) positions)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)