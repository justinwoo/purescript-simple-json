module Test.Inferred where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), isRight)
import Effect (Effect)
import Foreign (Foreign)
import Foreign as Foreign
import Record as Record
import Simple.JSON as JSON
import Test.Assert (assert)
import Type.Prelude (SProxy(..))

type RecordWithEither =
  { apple :: Int
  , banana :: Either Int String
  }

readEitherImpl
  :: forall a b
   . JSON.ReadForeign a
  => JSON.ReadForeign b
  => Foreign
  -> Foreign.F (Either a b)
readEitherImpl f
    = Left <$> JSON.readImpl f
  <|> Right <$> JSON.readImpl f

readRecordWithEitherJSON :: String -> Either Foreign.MultipleErrors RecordWithEither
readRecordWithEitherJSON s = runExcept do
  inter <- JSON.readJSON' s
  banana <- readEitherImpl inter.banana
  pure $ inter { banana = banana }

type RecordMisnamedField =
  { cherry :: Int
  }

readRecordMisnamedField :: String -> Either Foreign.MultipleErrors RecordMisnamedField
readRecordMisnamedField s = do
  inter <- JSON.readJSON s
  pure $ Record.rename grapeP cherryP inter
  where
    grapeP = SProxy :: SProxy "grape"
    cherryP = SProxy :: SProxy "cherry"

main :: Effect Unit
main = do
  assert <<< isRight $ readRecordWithEitherJSON """{"apple": 1, "banana": 1}"""
  assert <<< isRight $ readRecordWithEitherJSON """{"apple": 1, "banana": "yellow"}"""
