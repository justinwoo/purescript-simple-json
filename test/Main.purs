module Test.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (Either, either, isRight)
import Data.Foreign (MultipleErrors)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.StrMap (StrMap)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Type.Proxy (Proxy(..))

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
  let roundtrips :: forall a. ReadForeign a => WriteForeign a => Proxy a -> String -> Aff (RunnerEffects ()) Unit
      roundtrips _ enc0 = do
        let dec0 :: E a
            dec0 = handleJSON enc0
            enc1 = either (const "bad1") writeJSON dec0
            json0 :: Either String Json
            json0 = jsonParser enc0
            json1 :: Either String Json
            json1 = jsonParser enc1
            dec1 :: E a
            dec1 = handleJSON enc1
            enc2 = either (const "bad2") writeJSON dec1
        when (json0 /= json1) $ fail $ "\n\torig: " <> show json0 <> "\n\tenc: " <> show json1
        when (enc1 /= enc2) $ fail enc0
  describe "readJSON" do
    it "fails with invalid JSON" do
      let result = handleJSON """
        { "c": 1, "d": 2}
      """
      isRight (result :: E MyTest) `shouldEqual` false
  describe "roundtrips" do
    it "works with proper JSON" $ roundtrips (Proxy :: Proxy MyTest) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
    it "works with JSON lacking NullOrUndefined field" $ roundtrips (Proxy :: Proxy MyTest) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
    it "works with JSON containing NullOrUndefined field" $ roundtrips (Proxy :: Proxy MyTestNull) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"], "e": ["C", "D"]}
      """
    it "works with JSON containing StrMap field" $ roundtrips (Proxy :: Proxy MyTestStrMap) """
        { "a": 1, "b": {"asdf": 1, "c": 2} }
      """
