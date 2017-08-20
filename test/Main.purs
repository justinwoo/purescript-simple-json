module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), isRight)
import Data.Foreign (MultipleErrors)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Global.Unsafe (unsafeStringify)
import Simple.JSON (class ReadForeign, readJSON, writeJSON)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Util (equal)

type E a = Either MultipleErrors a

handleJSON :: forall a. ReadForeign a => String -> E a
handleJSON json = runExcept $ readJSON json

type MyTest =
  { a :: Int
  , b :: String
  , c :: Boolean
  , d :: Array String
  }

type MyTestNull =
  { a :: Int
  , b :: String
  , c :: Boolean
  , d :: Array String
  , e :: NullOrUndefined (Array String)
  }

type MyTestStrMap =
  { a :: Int
  , b :: StrMap Int
  }

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "readJSON" do
    it "works with proper JSON" do
      let result = handleJSON """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
      isRight (result :: E MyTest) `shouldEqual` true
    it "fails with invalid JSON" do
      let result = handleJSON """
        { "c": 1, "d": 2}
      """
      isRight (result :: E MyTest) `shouldEqual` false
    it "works with JSON lacking NullOrUndefined field" do
      let result = handleJSON """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
      isRight (result :: E MyTestNull) `shouldEqual` true
    it "works with JSON containing NullOrUndefined field" do
      let result = handleJSON """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"], "e": ["C", "D"]}
      """
      isRight (result :: E MyTestNull) `shouldEqual` true
    it "works with JSON containing StrMap field" do
      let result = handleJSON """
        { "a": 1, "b": {"asdf": 1, "c": 2} }
      """
      isRight (result :: E MyTestStrMap) `shouldEqual` true
  describe "writeJSON" do
    let
      original = { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"], "e": NullOrUndefined (Just ["C", "D"])} :: MyTestNull
      json = writeJSON original
    it "works with normal types" do
      case handleJSON json of
        Right (a :: MyTestNull) -> equal a original `shouldEqual` true
        Left e -> fail $ show e
    pending $ "orig: " <> unsafeStringify original
    pending $ "json: " <> json

