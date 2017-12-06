module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert
import Main (solution)
import Data.List

assertCase :: forall e. Int -> Int -> Array (Array Int) -> Eff (assert :: ASSERT | e) Unit
assertCase num expected values = do
    let result = solution $ fromFoldable $ map fromFoldable values
    assert' ("Case "<>show num<>" failed, expected "<>show expected<>", actual "<>show result) (result == expected)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
    assertCase 0 0 []
    assertCase 1 18 [[5,1,9,5],[7,5,3],[2,4,6,8]]
