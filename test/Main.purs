module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (Either, isRight)
import Data.Foreign (MultipleErrors)
import Simple.JSON (class ReadForeign, readJSON)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Type.Proxy (Proxy(..))

handleJSON :: forall a. ReadForeign a => Proxy a -> String -> Either MultipleErrors a
handleJSON _ json = runExcept $ readJSON json

type MyTest =
  { a :: Int
  , b :: String
  , c :: Boolean
  , d :: Array String
  }

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "readJSON" do
    it "works with proper JSON" do
      let result = handleJSON (Proxy :: Proxy MyTest) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
      isRight result `shouldEqual` true
    it "fails with invalid JSON" do
      let result = handleJSON (Proxy :: Proxy MyTest) """
        { "c": 1, "d": 2}
      """
      isRight result `shouldEqual` false
