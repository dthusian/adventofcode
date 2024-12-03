import System.IO
import Data.Maybe
import Data.Char
import Debug.Trace

slices :: [a] -> [[a]]
slices [] = []
slices x = x : slices (tail x)

readMatch :: String -> String -> Maybe String
readMatch pattern s
  | length s < length pattern = Nothing
  | take (length pattern) s == pattern = Just (drop (length pattern) s)
  | otherwise = Nothing

readInt :: String -> Maybe (Integer, String)
readInt x =
  let frag1 = takeWhile isNumber x
      frag2 = dropWhile isNumber x
  in if null frag1 then Nothing else Just (read frag1, frag2)

part1 :: String -> Integer
part1 [] = 0
part1 x =
  let process x = do frag <- readMatch "mul(" x
                     (valA, frag) <- readInt frag
                     frag <- readMatch "," frag
                     (valB, frag) <- readInt frag
                     frag <- readMatch ")" frag
                     return (valA * valB)
  in (sum . mapMaybe process . slices) x

part2 :: String -> Integer
part2 x = 
  let process x = do frag <- readMatch "mul(" x
                     (valA, frag) <- readInt frag
                     frag <- readMatch "," frag
                     (valB, frag) <- readInt frag
                     frag <- readMatch ")" frag
                     return (valA * valB)
  in fst (foldl (\st x -> (
    fst st + if snd st then fromMaybe 0 (process x) else 0,
    if isJust (readMatch "do()" x) then True
    else if isJust (readMatch "don't()" x) then False
    else snd st
  )) (0, True) (slices x))

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  print (part1 contents)
  print (part2 contents)
  hClose file