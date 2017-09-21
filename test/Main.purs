module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either, fromLeft, isRight)
import Data.Foreign (ForeignError(..), MultipleErrors)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.StrMap (StrMap)
import Partial.Unsafe (unsafePartial)
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

type MyTestMaybe = 
  { a :: Maybe String 
  }

type MyTestManyMaybe = 
  { a         :: Maybe String
  , aNull     :: Maybe String 
  , b         :: Maybe Int 
  , bNull     :: Maybe Int 
  , c         :: Maybe Boolean 
  , cNull     :: Maybe Boolean 
  , d         :: Maybe Number
  , dNull     :: Maybe Number 
  , e         :: Maybe (Array (Maybe String))
  , eNull     :: Maybe (Array (Maybe String))
  }


roundtrips :: forall a. ReadForeign a => WriteForeign a => Proxy a -> String -> Aff (RunnerEffects ()) Unit
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

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "readJSON" do
    it "fails with invalid JSON" do
      let result = handleJSON """
        { "c": 1, "d": 2}
      """
      (unsafePartial $ fromLeft result) `shouldEqual`
        (NonEmptyList (NonEmpty (ErrorAtProperty "a" (TypeMismatch "Int" "Undefined")) Nil))
      isRight (result :: E MyTest) `shouldEqual` false
    
    it "works with missing Maybe fields by setting them to Nothing" do
      let result = handleJSON "{}"
      (writeJSON <$> (result :: E MyTestMaybe)) `shouldEqual` (Right """{"a":null}""")


  describe "roundtrips" do
    it "works with proper JSON" $ roundtrips (Proxy :: Proxy MyTest) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
    it "works with JSON lacking NullOrUndefined field" $ roundtrips (Proxy :: Proxy MyTestNull) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
    it "works with JSON containing NullOrUndefined field" $ roundtrips (Proxy :: Proxy MyTestNull) """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"], "e": ["C", "D"]}
      """
    it "works with JSON containing StrMap field" $ roundtrips (Proxy :: Proxy MyTestStrMap) """
        { "a": 1, "b": {"asdf": 1, "c": 2} }
      """
    it "works with Maybe field and existing value" $ roundtrips (Proxy :: Proxy MyTestMaybe) """
        { "a": "foo" }
      """
    it "works with Maybe field and null value" $ roundtrips (Proxy :: Proxy MyTestMaybe) """
        { "a": null }
      """ 
    it "works with many Maybe fields" $ roundtrips (Proxy :: Proxy MyTestManyMaybe) """
      { "a": "str", "aNull": null, "b":1, "bNull": null, "c":true, "cNull":null, "d":1.1, "dNull":null, "e":["str1", "str2", null], "eNull": null }
    """ 
