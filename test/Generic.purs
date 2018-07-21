module Test.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either, isRight)
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON
import Test.Assert (assert)

data IntOrBoolean
  = Int Int
  | Boolean Boolean
instance readForeign :: JSON.ReadForeign IntOrBoolean where
  readImpl f
      = Int <$> Foreign.readInt f
    <|> Boolean <$> Foreign.readBoolean f
instance showIntOrBoolean :: Show IntOrBoolean where
  show (Int i) = "(Int " <> show i <> ")"
  show (Boolean i) = "(Boolean " <> show i <> ")"

decodeToIntOrBoolean :: String -> Either Foreign.MultipleErrors IntOrBoolean
decodeToIntOrBoolean = JSON.readJSON

data IntOrBoolean2
  = Int2 Int
  | Boolean2 Boolean

derive instance genericIntOrBoolean2 :: GR.Generic IntOrBoolean2 _

instance showIntOrBoolean2 :: Show IntOrBoolean2 where
  show = genericShow
instance readForeignIntOrBoolean2 :: JSON.ReadForeign IntOrBoolean2 where
  readImpl f = GR.to <$> untaggedSumRep f

class UntaggedSumRep rep where
  untaggedSumRep :: Foreign -> Foreign.F rep

instance untaggedSumRepSum ::
  ( UntaggedSumRep a
  , UntaggedSumRep b
  ) => UntaggedSumRep (GR.Sum a b) where
  untaggedSumRep f
      = GR.Inl <$> untaggedSumRep f
    <|> GR.Inr <$> untaggedSumRep f

instance untaggedSumRepConstructor ::
  ( UntaggedSumRep a
  ) => UntaggedSumRep (GR.Constructor name a) where
  untaggedSumRep f = GR.Constructor <$> untaggedSumRep f

instance untaggedSumRepArgument ::
  ( JSON.ReadForeign a
  ) => UntaggedSumRep (GR.Argument a) where
  untaggedSumRep f = GR.Argument <$> JSON.readImpl f

decodeToIntOrBoolean2 :: String -> Either Foreign.MultipleErrors IntOrBoolean2
decodeToIntOrBoolean2 = JSON.readJSON

main :: Effect Unit
main = do
  assert <<< isRight $ decodeToIntOrBoolean "1" -- (Right (Int 1))
  assert <<< isRight $ decodeToIntOrBoolean "true" -- (Right (Boolean true))
  assert <<< isRight $ decodeToIntOrBoolean2 "1" -- (Right (Int2 1))
  assert <<< isRight $ decodeToIntOrBoolean2 "true" -- (Right (Boolean2 true))
