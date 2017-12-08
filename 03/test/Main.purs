module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert
import Main (solution)
import Data.List

assertCase :: forall e. Int -> Int -> Int -> Eff (assert :: ASSERT | e) Unit
assertCase num expected values = do
    let result = solution $ values
    assert' ("Case "<>show num<>" failed, expected "<>show expected<>", actual "<>show result) (result == expected)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
    assertCase 0 0 1
    assertCase 1 3 12
    assertCase 2 2 23
    assertCase 3 31 1024
