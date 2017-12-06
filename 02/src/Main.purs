module Main where

import Prelude (Unit, bind, map, otherwise, not, pure, show, (#), ($), (==), (-),(<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.List (List(..), filter, fromFoldable, snoc, zip, (:))
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

solution :: List (List Int) -> Int
solution = sum <<< (map rowValue)

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
    log $ show $ solution $ parseInput input
