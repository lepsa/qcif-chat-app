module Test.StateMachine where

import Test.Types
import Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

propApiTests :: TestEnv -> IO Bool -> Property
propApiTests env reset = withTests 100 . property $ do
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  worked <- evalIO reset
  if worked then pure () else fail "Couldn't reset the API"
  executeSequential initialState actions
  where
    commands =
      [
      ]