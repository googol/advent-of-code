module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.MonadZero (guard)
import Data.List (List(..), filter, fromFoldable, head, snoc, zip, (:))
import Data.Traversable (maximum, traverse, sequence, scanl)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String (toCharArray, split, null, Pattern(..))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Either (Either(..), either, fromRight, note, hush)
import Data.Int (fromString)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Control.Bind (bindFlipped)
import Partial.Unsafe (unsafePartial)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Array ((!!))
import Data.Map
import Data.Foldable

runCommands :: List Command -> Tuple Int (Map String Int)
runCommands = foldl commandStep (Tuple 0 empty)
    where
          commandStep :: Tuple Int (Map String Int) -> Command -> Tuple Int (Map String Int)
          commandStep t@(Tuple highest registers) command = fromMaybe t $ do
             let registerValue = fromMaybe 0 $ lookup (getConditionRegister $ getCondition command) registers
             guard $ testCondition (getCondition command) registerValue
             let newRegisters = alter (applyCommand command) (getRegister command) registers
             pure $ Tuple (max highest $ getLargestValue newRegisters) newRegisters

applyCommand :: Command -> Maybe Int -> Maybe Int
applyCommand (Inc _ delta _) value = Just ((fromMaybe 0 value) + delta)
applyCommand (Dec _ delta _) value = Just ((fromMaybe 0 value) - delta)

getLargestValue :: Map String Int -> Int
getLargestValue map = fromMaybe 0 $ maximum $ values map

solution :: List Command -> Int
solution = getLargestValue <<< snd <<< runCommands

solution2 :: List Command -> Int
solution2 = fst <<< runCommands

lines :: String -> List String
lines = filter (not <<< null) <<< fromFoldable <<< split (Pattern "\n")

type Register = String

data Condition = Lt Register Int
    | Gt Register Int
    | Gte Register Int
    | Lte Register Int
    | Eq Register Int
    | Neq Register Int

testCondition :: Condition -> Int -> Boolean
testCondition (Lt  _ comparisonValue) value = value < comparisonValue
testCondition (Gt  _ comparisonValue) value = value > comparisonValue
testCondition (Lte _ comparisonValue) value = value <= comparisonValue
testCondition (Gte _ comparisonValue) value = value >= comparisonValue
testCondition (Eq  _ comparisonValue) value = value == comparisonValue
testCondition (Neq _ comparisonValue) value = value /= comparisonValue

getConditionRegister :: Condition -> Register
getConditionRegister (Lt register _) = register
getConditionRegister (Gt register _) = register
getConditionRegister (Lte register _) = register
getConditionRegister (Gte register _) = register
getConditionRegister (Eq register _) = register
getConditionRegister (Neq register _) = register

data Command = Inc Register Int Condition
    | Dec Register Int Condition

getCondition :: Command -> Condition
getCondition (Inc _ _ cond) = cond
getCondition (Dec _ _ cond) = cond

getRegister :: Command -> Register
getRegister (Inc register _ _) = register
getRegister (Dec register _ _) = register

parseCommand :: String -> Either String Command
parseCommand commandString = do
    reg <- regex "^([a-z]+)\\s+(inc|dec)\\s+(-?\\d+)\\s+if\\s+([a-z]+)\\s+(<|>|<=|>=|==|!=)\\s+(-?\\d+)$" noFlags
    matches' <- note "Could not match" $ match reg commandString
    matches <- traverse (note "One of the matches was Nothing") matches'
    reg1 <- note "No match at index 1" $ matches !! 1
    operation <- note "No match at index 2" $ matches !! 2
    amount <- note "No match at index 3" $ matches !! 3
    reg2 <- note "No match at index 4" $ matches !! 4
    compOp <- note "No match at index 5" $ matches !! 5
    compAmount <- note "No match at index 6" $ matches !! 6
    condition <- parseCondition compOp reg2 compAmount
    toCommand operation reg1 amount condition

parseCondition :: String -> String -> String -> Either String Condition
parseCondition "<"  reg amount = Lt  reg <$> (note "Condition amount wasn't an integer" $ fromString amount)
parseCondition ">"  reg amount = Gt  reg <$> (note "Condition amount wasn't an integer" $ fromString amount)
parseCondition "<=" reg amount = Lte reg <$> (note "Condition amount wasn't an integer" $ fromString amount)
parseCondition ">=" reg amount = Gte reg <$> (note "Condition amount wasn't an integer" $ fromString amount)
parseCondition "==" reg amount = Eq  reg <$> (note "Condition amount wasn't an integer" $ fromString amount)
parseCondition "!=" reg amount = Neq reg <$> (note "Condition amount wasn't an integer" $ fromString amount)
parseCondition op   _   _      = Left $ "Unknown operator " <> op

toCommand :: String -> String -> String -> Condition -> Either String Command
toCommand "inc" reg amount condition = Inc reg <$> (note "Amount wasn't an integer" $ fromString amount) <@> condition
toCommand "dec" reg amount condition = Dec reg <$> (note "Amount wasn't an integer" $ fromString amount) <@> condition
toCommand op    _   _      _         = Left $ "Unknown operation " <> op

parseInput :: String -> Either String (List Command)
parseInput x = traverse (parseCommand) $ lines x

main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
    input <- readTextFile UTF8 "input" 
    let parsed = parseInput input
    either (\err -> log $ "Error: " <> err) (log <<< show <<< solution) parsed
    either (\err -> log $ "Error: " <> err) (log <<< show <<< solution2) parsed
