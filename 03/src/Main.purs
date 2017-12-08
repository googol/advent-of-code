module Main where

import Prelude
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
import Data.List.Lazy as L
import Data.Unfoldable (unfoldr)
import Data.Bifunctor
import Data.Ord

sides :: Int -> Int
sides n = n * 2 + 1

size :: Int -> Int
size x | x < 0 = 0
       | x == 0 = 1
       | otherwise = (sides x) * 4 - 4

startingPos :: Int -> Tuple Int Int
startingPos x | x < 1 = Tuple 0 0
              | x == 1 = Tuple 1 0
              | otherwise = Tuple (1 + (x - 1)) (0 - (x - 1))

startingNumber :: Int -> Int
startingNumber x | x < 0 = 0
                 | otherwise = startingNumber (x - 1) + sides (x-1)

data Direction = Left | Right | Up | Down

shouldTurn Left  (Tuple x y) = x > 0 && y >  0 && (abs x) == (abs y)
shouldTurn Right (Tuple x y) = x < 0 && y <  0 && (abs x) == (abs y)
shouldTurn Up    (Tuple x y) = x > 0 && y <= 0 && (abs x) == (abs (y - 1))
shouldTurn Down  (Tuple x y) = x < 0 && y >  0 && (abs x) == (abs y)

changeDirection :: Tuple Int Int -> Maybe Direction
changeDirection t | shouldTurn Left  t = Just Left
                  | shouldTurn Right t = Just Right
                  | shouldTurn Up    t = Just Up
                  | shouldTurn Down  t = Just Down
                  | otherwise          = Nothing

advance :: Direction -> Tuple Int Int -> Tuple Int Int
advance Left  = lmap (_ - 1)
advance Right = lmap (_ + 1)
advance Up    = rmap (_ + 1)
advance Down  = rmap (_ - 1)

coord :: Int -> Tuple Int Int
coord initn = go (Tuple 0 0) Right initn
    where
          go c dir n | n <= 1 = c
                     | otherwise = let newCoord = advance dir c
                                   in go newCoord (fromMaybe dir (changeDirection newCoord)) (n-1)

distance :: Tuple Int Int -> Tuple Int Int -> Int
distance (Tuple x1 y1) (Tuple x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

solution :: Int -> Int
solution n = distance (Tuple 0 0) $ coord n

main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
    let input = 368078
    log $ show $ coord input
    log $ show $ solution input
