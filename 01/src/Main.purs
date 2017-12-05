module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Data.List
import Data.Foldable (sum)
import Data.Traversable
import Data.Tuple
import Data.String
import Data.Maybe
import Node.FS
import Node.FS.Sync
import Node.Encoding
import Control.Bind

shiftByOne :: forall a. List a -> List a
shiftByOne Nil = Nil
shiftByOne (x:xs) = snoc xs x

pairValue :: Tuple Int Int -> Int
pairValue (Tuple a b) | a == b = a
                      | otherwise = 0

solution :: List Int -> Int
solution Nil = 0
solution xs = sum $ map pairValue $ zip xs (shiftByOne xs)

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

fromMaybe :: forall a. Maybe a -> List a
fromMaybe = fromFoldable

selectJusts :: forall a. List (Maybe a) -> List a
selectJusts = bindFlipped fromMaybe

parseInput :: String -> List Int
parseInput x = toCharArray x # fromFoldable # map charToInt # selectJusts

main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
    input <- readTextFile UTF8 "input" 
    log $ show $ solution $ parseInput input