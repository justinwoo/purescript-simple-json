module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromLeft, isRight)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign, ForeignError(..), MultipleErrors)
import Foreign.Object (Object)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, parseJSON, readJSON, writeJSON)
import Test.Assert (assertEqual)
import Test.EnumSumGeneric as Test.EnumSumGeneric
import Test.Generic as Test.Generic
import Test.Inferred as Test.Inferred
import Test.Quickstart as Test.Quickstart
import Type.Proxy (Proxy(..))

type E a = Either MultipleErrors a

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
  , e :: Maybe (Array String)
  }

type MyTestStrMap =
  { a :: Int
  , b :: Object Int
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

type MyTestNullable =
  { a :: Nullable String
  , b :: Nullable String
  }

type MyTestVariant = Variant
  ( a :: String
  , b :: Int
  )

roundtrips :: forall a. ReadForeign a => WriteForeign a => Proxy a -> String -> Effect Unit
roundtrips _ enc0 = do
  let parseJSON' = lmap show <<< runExcept <<< parseJSON
      dec0 :: E a
      dec0 = readJSON enc0
      enc1 = either (const "bad1") writeJSON dec0
      json0 :: Either String Foreign
      json0 = parseJSON' enc0
      json1 :: Either String Foreign
      json1 = parseJSON' enc1
      dec1 :: E a
      dec1 = readJSON enc1
      enc2 = either (const "bad2") writeJSON dec1
  when (enc1 /= enc2) $ throw enc0

shouldEqual :: forall a . Eq a => Show a => a -> a -> Effect Unit
shouldEqual a b =
  assertEqual { actual: a, expected: b}

main :: Effect Unit
main = do
  shouldEqual 1 1

  -- "fails with invalid JSON"
  let r1 = readJSON """{ "c": 1, "d": 2}"""
  (unsafePartial $ fromLeft r1) `shouldEqual`
    (NonEmptyList (NonEmpty (ErrorAtProperty "a" (TypeMismatch "Int" "Undefined")) ((ErrorAtProperty "b" (TypeMismatch "String" "Undefined")) : (ErrorAtProperty "c" (TypeMismatch "Boolean" "Number")) : (ErrorAtProperty "d" (TypeMismatch "array" "Number")) : Nil)))
  isRight (r1 :: E MyTest) `shouldEqual` false

  -- "works with missing Maybe fields by setting them to Nothing"
  let r2 = readJSON "{}"
  (writeJSON <$> (r2 :: E MyTestMaybe)) `shouldEqual` (Right """{}""")

  -- "fails with undefined for null with correct error message"
  let r3 = readJSON """
    { "a": "asdf" }
  """
  (unsafePartial $ fromLeft r3) `shouldEqual`
    (NonEmptyList (NonEmpty (ErrorAtProperty "b" (TypeMismatch "Nullable String" "Undefined")) Nil))
  (isRight (r3 :: E MyTestNullable)) `shouldEqual` false

  -- roundtrips
  -- "works with proper JSON"
  roundtrips (Proxy :: Proxy MyTest) """
    { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
  """

  -- "works with JSON lacking Maybe field"
  roundtrips (Proxy :: Proxy MyTestNull) """
    { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
  """

  -- "works with JSON containing Maybe field"
  roundtrips (Proxy :: Proxy MyTestNull) """
    { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"], "e": ["C", "D"]}
  """

  -- -- "works with JSON containing Object field"
  roundtrips (Proxy :: Proxy MyTestStrMap) """
    { "a": 1, "b": {"asdf": 1, "c": 2} }
  """

  -- "works with Maybe field and existing value"
  roundtrips (Proxy :: Proxy MyTestMaybe) """
    { "a": "foo" }
  """

  -- "works with Nullable"
  roundtrips (Proxy :: Proxy MyTestNullable) """
    { "a": null, "b": "a" }
  """

  -- "works with Variant"
  roundtrips (Proxy :: Proxy MyTestVariant) """
    { "type": "b", "value": 123  }
  """

  -- run examples
  Test.Generic.main
  Test.EnumSumGeneric.main
  Test.Inferred.main
  Test.Quickstart.main
