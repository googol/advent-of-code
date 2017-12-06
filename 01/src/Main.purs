module Main where

import Prelude (Unit, bind, map, otherwise, pure, show, (#), ($), (==), (<>), (/), discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.List (List(..), fromFoldable, snoc, zip, (:), drop, take, length)
import Data.Traversable (sum)
import Data.Tuple (Tuple(..))
import Data.String (toCharArray)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Control.Bind (bindFlipped)

shiftByOne :: forall a. List a -> List a
shiftByOne Nil = Nil
shiftByOne (x:xs) = snoc xs x

shiftByHalf :: forall a. List a -> List a
shiftByHalf Nil = Nil
shiftByHalf xs = (drop (length xs / 2) xs) <> (take (length xs / 2) xs)

pairValue :: Tuple Int Int -> Int
pairValue (Tuple a b) | a == b = a
                      | otherwise = 0

solution :: List Int -> Int
solution Nil = 0
solution xs = sum $ map pairValue $ zip xs (shiftByOne xs)

solution2 :: List Int -> Int
solution2 xs = sum $ map pairValue $ zip xs (shiftByHalf xs)

charToInt :: Char -> Maybe Int
charToInt '0' = pure 0
charToInt '1' = pure 1
charToInt '2' = pure 2
charToInt '3' = pure 3
charToInt '4' = pure 4
charToInt '5' = pure 5
charToInt '6' = pure 6
charToInt '7' = pure 7
charToInt '8' = pure 8
charToInt '9' = pure 9
charToInt x = Nothing

parseInput :: String -> List Int
parseInput x = toCharArray x
    # fromFoldable
    # map charToInt
    # bindFlipped fromFoldable

main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
    input <- readTextFile UTF8 "input"
    let values = parseInput input
    log $ show $ solution values
    log $ show $ solution2 values
