module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert
import Main (solution)
import Data.List

assertCase :: forall e. Int -> Int -> Array Int -> Eff (assert :: ASSERT | e) Unit
assertCase num expected values = do
    let result = solution $ fromFoldable values
    assert' ("Case "<>show num<>" failed, expected "<>show expected<>", actual "<>show result) (result == expected)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
    assertCase 0 0 []
    assertCase 1 3 [1,1,2,2]
    assertCase 2 4 [1,1,1,1]
    assertCase 3 0 [1,2,3,4]
    assertCase 4 9 [9,1,2,1,2,1,2,9]
