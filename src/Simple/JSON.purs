module Simple.JSON where

import Prelude

import Data.Foreign (F, Foreign, readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined(NullOrUndefined), readNullOrUndefined, undefined)
import Data.Maybe (maybe)
import Data.Record (get, insert)
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Global.Unsafe (unsafeStringify)
import Type.Prelude (class TypeEquals, to)
import Type.Row (class ListToRow, class RowLacks, class RowToList, Cons, Nil, RLProxy(RLProxy), RProxy(..), kind RowList)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON :: forall a
  .  ReadForeign a
  => String
  -> F a
readJSON = readImpl <=< parseJSON

-- | Write a JSON string from a type `a`.
writeJSON :: forall a
  .  WriteForeign a
  => a
  -> String
writeJSON = unsafeStringify <<< writeImpl

write :: forall a
  .  WriteForeign a
  => a
  -> Foreign
write = writeImpl

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
  readImpl = sequence <<< StrMap.mapWithKey (const readImpl) <=< readStrMap

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

-- | A class for writing a value into JSON
-- | need to do this intelligently using Foreign probably, because of null and undefined whatever
class WriteForeign a where
  writeImpl :: a -> Foreign

instance writeForeignForeign :: WriteForeign Foreign where
  writeImpl = id

instance writeForeignString :: WriteForeign String where
  writeImpl = toForeign

instance writeForeignInt :: WriteForeign Int where
  writeImpl = toForeign

instance writeForeignChar :: WriteForeign Char where
  writeImpl = toForeign

instance writeForeignNumber :: WriteForeign Number where
  writeImpl = toForeign

instance writeForeignBoolean :: WriteForeign Boolean where
  writeImpl = toForeign

instance writeForeignArray :: WriteForeign a => WriteForeign (Array a) where
  writeImpl xs = toForeign $ writeImpl <$> xs

instance writeForeignNullOrUndefined :: WriteForeign a => WriteForeign (NullOrUndefined a) where
  writeImpl (NullOrUndefined a) = maybe undefined writeImpl a

instance writeForeignStrMap :: WriteForeign a => WriteForeign (StrMap.StrMap a) where
  writeImpl = toForeign <<< StrMap.mapWithKey (const writeImpl)

instance recordWriteForeign ::
  ( RowToList row rl
  , WriteForeignFields rl row row'
  ) => WriteForeign (Record row) where
  writeImpl rec = toForeign $ writeImplFields rlp rec
    where
      rlp = RLProxy :: RLProxy rl

class WriteForeignFields (rl :: RowList) row (row' :: # Type)
  | rl -> row row' where
  writeImplFields :: forall g. g rl -> Record row -> Record row'

instance consWriteForeignFields ::
  ( IsSymbol name
  , WriteForeign ty
  , WriteForeignFields tail row tailRow
  , RowCons name ty whatever row
  , RowLacks name tailRow
  , RowCons name Foreign tailRow row'
  ) => WriteForeignFields (Cons name ty tail) row row' where
  writeImplFields _ rec = result
    where
      namep = SProxy :: SProxy name
      value = writeImpl $ get namep rec
      tailp = RLProxy :: RLProxy tail
      rest = writeImplFields tailp rec
      result = insert namep value rest

instance nilWriteForeignFields ::
  ( TypeEquals {} (Record row')
  ) => WriteForeignFields Nil row row' where
  writeImplFields _ _ = to {}
