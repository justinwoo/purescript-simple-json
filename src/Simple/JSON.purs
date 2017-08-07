module Simple.JSON where

import Prelude

import Data.Foreign (F, Foreign, readArray, readBoolean, readChar, readInt, readNumber, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined, readNullOrUndefined)
import Data.Record (insert)
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Type.Equality (class TypeEquals, to)
import Type.Row (class ListToRow, class RowLacks, class RowToList, Cons, Nil, RLProxy(RLProxy), RProxy(..), kind RowList)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON :: forall a
  .  ReadForeign a
  => String
  -> F a
readJSON = readImpl <=< parseJSON

read :: forall a
  .  ReadForeign a
  => Foreign
  -> F a
read = readImpl

-- | A class for reading foreign values to a type
class ReadForeign a where
  readImpl :: Foreign -> F a

instance readForeign :: ReadForeign Foreign where
  readImpl = pure

instance readChar :: ReadForeign Char where
  readImpl = readChar

instance readNumber :: ReadForeign Number where
  readImpl = readNumber

instance readInt :: ReadForeign Int where
  readImpl = readInt

instance readString :: ReadForeign String where
  readImpl = readString

instance readBoolean :: ReadForeign Boolean where
  readImpl = readBoolean

instance readArray :: ReadForeign a => ReadForeign (Array a) where
  readImpl = readElements <=< readArray
    where
      readElements xs = sequence $ readImpl <$> xs

instance readNullOrUndefined :: ReadForeign a => ReadForeign (NullOrUndefined a) where
  readImpl = readNullOrUndefined readImpl

instance readStrMap :: ReadForeign a => ReadForeign (StrMap.StrMap a) where
  readImpl = sequence <<< StrMap.mapWithKey (\_ -> readImpl) <=< readStrMap

instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList fields
  , ListToRow fieldList fields
  ) => ReadForeign (Record fields) where
  readImpl = getFields (RLProxy :: RLProxy fieldList) (RProxy :: RProxy fields)

-- | A class for reading foreign values from properties
class ReadForeignFields (xs :: RowList) (row :: # Type) where
  getFields :: RLProxy xs
    -> RProxy row
    -> Foreign
    -> F (Record row)

instance readFieldsCons ::
  ( IsSymbol name
  , ReadForeign ty
  , ReadForeignFields tail tailRow
  , RowLacks name tailRow
  , RowCons name ty tailRow row
  ) => ReadForeignFields (Cons name ty tail) row where
  getFields _ _ obj = do
    value <- readImpl =<< readProp name obj
    rest <- getFields tailP tailRowP obj
    pure $ insert nameP value rest
    where
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      tailRowP = RProxy :: RProxy tailRow
      name = reflectSymbol nameP

instance readFieldsNil ::
  ( TypeEquals {} (Record row)
  ) => ReadForeignFields Nil row where
  getFields _ _ _ =
    pure $ to {}
