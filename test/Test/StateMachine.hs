module Test.StateMachine where

import Test.Types
import Hedgehog
import Test.Commands
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import qualified Data.Map as M

initialState :: TestState v
initialState = TestState M.empty M.empty M.empty

propApiTests :: TestEnv -> IO Bool -> Property
propApiTests env reset = withTests 100 . property $ do
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  worked <- evalIO reset
  if worked then pure () else fail "Couldn't reset the API"
  executeSequential initialState actions
  where
    commands = ($ env) <$>
      [ registerUser
      , loginUser
      , getAllMessages
      , postMessage
      , getUsers
      , getMessages
      ]