module Test.EnumSumGeneric where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

enumReadForeignLowercase :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeignLowercase f =
  to <$> enumReadForeignImpl f

-- type class for "enums", or nullary sum types
class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- JSON.readImpl f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< Foreign.ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)

data Fruit
  = Abogado
  | Boat
  | Candy
derive instance genericFruit :: Generic Fruit _
instance fruitReadForeign :: JSON.ReadForeign Fruit where
  readImpl = enumReadForeignLowercase
instance furitShow :: Show Fruit where
  show = genericShow

type MyThing =
  { fruit :: Fruit
  }

readFruit :: String -> Either Foreign.MultipleErrors Fruit
readFruit = JSON.readJSON

main :: Effect Unit
main = do
  logShow $ readFruit "\"Abogado\""
  logShow $ readFruit "\"Boat\""
  logShow $ readFruit "\"Candy\""
