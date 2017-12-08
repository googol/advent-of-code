module Main where

import Prelude (Unit, bind, map, otherwise, not, pure, show, (#), ($), (==), (-),(<<<), discard, (/=), mod, (/))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.MonadZero (guard)
import Data.List (List(..), filter, fromFoldable, head, snoc, zip, (:))
import Data.Traversable (sum, maximum, minimum)
import Data.Tuple (Tuple(..))
import Data.String (toCharArray, split, null, Pattern(..))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Either (fromRight, note)
import Data.Int (fromString)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Control.Bind (bindFlipped)
import Partial.Unsafe (unsafePartial)

rowValue :: List Int -> Int
rowValue x = (fromMaybe 0 (maximum x)) - (fromMaybe 0 (minimum x))

findEvenDivisions :: List Int -> List Int
findEvenDivisions xs = do
    x <- xs
    y <- xs
    guard (x /= y)
    guard (x `mod` y == 0)
    pure (x / y)

rowValue2 :: List Int -> Int
rowValue2 Nil = 0
rowValue2 xs = unsafePartial $ fromJust $ head $ findEvenDivisions xs

solution :: List (List Int) -> Int
solution = sum <<< (map rowValue)

solution2 :: List (List Int) -> Int
solution2 = sum <<< (map rowValue2)

lines :: String -> List String
lines = filter (not <<< null) <<< fromFoldable <<< split (Pattern "\n")

words :: String -> List String
words = fromFoldable <<< split (Pattern "\t")

wordsToInts :: List String -> List Int
wordsToInts = map (\x -> unsafePartial $ fromRight $ note x $ fromString x)

parseInput :: String -> List (List Int)
parseInput x = map (wordsToInts <<< words) $ lines x

main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
    input <- readTextFile UTF8 "input" 
    let values = parseInput input
    log $ show $ solution values
    log $ show $ solution2 values
