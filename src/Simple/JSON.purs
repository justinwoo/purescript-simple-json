module Simple.JSON (
  readJSON
, readJSON'
, writeJSON
, write
, read
, read'

, class ReadForeign
, readImpl
, class ReadForeignFields
, getFields
, class ReadForeignVariant
, readVariantImpl

, class WriteForeign
, writeImpl
, class WriteForeignFields
, writeImplFields
, class WriteForeignVariant
, writeVariantImpl

) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept, withExcept)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined(NullOrUndefined), readNullOrUndefined, unNullOrUndefined, undefined)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Record (get)
import Data.Record.Builder (Builder)
import Data.Record.Builder as Builder
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Variant (Variant, inj, on)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(RLProxy), kind RowList)

-- | Read a JSON string to a type `a` while returning a `MultipleErrors` if the
-- | parsing failed.
readJSON :: forall a
  .  ReadForeign a
  => String
  -> Either MultipleErrors a
readJSON = runExcept <<< (readImpl <=< parseJSON)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON' :: forall a
  .  ReadForeign a
  => String
  -> F a
readJSON' = readImpl <=< parseJSON

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
   . ReadForeign a
  => Foreign
  -> Either MultipleErrors a
read = runExcept <<< readImpl

read' :: forall a
  .  ReadForeign a
  => Foreign
  -> F a
read' = readImpl

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
  readImpl = traverse readImpl <=< readArray

instance readNullOrUndefined :: ReadForeign a => ReadForeign (NullOrUndefined a) where
  readImpl = readNullOrUndefined readImpl

instance readMaybe :: ReadForeign a => ReadForeign (Maybe a) where
  readImpl = map unNullOrUndefined <<< readImpl

instance readNullable :: ReadForeign a => ReadForeign (Nullable a) where
  readImpl o = withExcept (map reformat) $
    map toNullable <$> traverse readImpl =<< readNull o
    where
      reformat error = case error of
        TypeMismatch inner other -> TypeMismatch ("Nullable " <> inner) other
        _ -> error

instance readStrMap :: ReadForeign a => ReadForeign (StrMap.StrMap a) where
  readImpl = sequence <<< StrMap.mapWithKey (const readImpl) <=< readStrMap

instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList () fields
  ) => ReadForeign (Record fields) where
  readImpl o = do
    steps <- getFields fieldListP o
    pure $ Builder.build steps {}
    where
      fieldListP = RLProxy :: RLProxy fieldList

-- | A class for reading foreign values from properties
class ReadForeignFields (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  getFields :: RLProxy xs
    -> Foreign
    -> F (Builder (Record from) (Record to))

instance readFieldsCons ::
  ( IsSymbol name
  , ReadForeign ty
  , ReadForeignFields tail from from'
  , RowLacks name from'
  , RowCons name ty from' to
  ) => ReadForeignFields (Cons name ty tail) from to where
  getFields _ obj = do
    value :: ty <- withExcept' $ readImpl =<< readProp name obj
    rest <- getFields tailP obj
    let
      first :: Builder (Record from') (Record to)
      first = Builder.insert nameP value
    pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtProperty name

instance readFieldsNil ::
  ReadForeignFields Nil () () where
  getFields _ _ =
    pure id

instance readForeignVariant ::
  ( RowToList variants rl
  , ReadForeignVariant rl variants
  ) => ReadForeign (Variant variants) where
  readImpl o = readVariantImpl (RLProxy :: RLProxy rl) o

class ReadForeignVariant (xs :: RowList) (row :: # Type)
  | xs -> row where
  readVariantImpl :: RLProxy xs
    -> Foreign
    -> F (Variant row)

instance readVariantNil ::
  ReadForeignVariant Nil trash where
  readVariantImpl _ _ = fail $ ForeignError "Unable to match any variant member."

instance readVariantCons ::
  ( IsSymbol name
  , ReadForeign ty
  , RowCons name ty trash row
  , ReadForeignVariant tail row
  ) => ReadForeignVariant (Cons name ty tail) row where
  readVariantImpl _ o = do
    obj :: { type :: String, value :: Foreign } <- readImpl o
    if obj.type == name
      then do
        value :: ty <- readImpl obj.value
        pure $ inj namep value
      else
        (fail <<< ForeignError $ "Did not match variant tag " <> name)
    <|> readVariantImpl (RLProxy :: RLProxy tail) o
    where
      namep = SProxy :: SProxy name
      name = reflectSymbol namep

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

instance writeForeignNonEmptyList :: WriteForeign a => WriteForeign (NonEmptyList a) where
  writeImpl xs = toForeign $ fromFoldable $ writeImpl <$> xs

instance writeForeignForeignError :: WriteForeign ForeignError where
  writeImpl e = toForeign $ (show e)
  
instance writeForeignNullOrUndefined :: WriteForeign a => WriteForeign (NullOrUndefined a) where
  writeImpl (NullOrUndefined a) = maybe undefined writeImpl a

instance writeForeignMaybe :: WriteForeign a => WriteForeign (Maybe a) where
  writeImpl (Just a) = writeImpl a
  writeImpl Nothing = toForeign $ toNullable Nothing

instance writeForeignNullable :: WriteForeign a => WriteForeign (Nullable a) where
  writeImpl = writeImpl <<< toMaybe

instance writeForeignStrMap :: WriteForeign a => WriteForeign (StrMap.StrMap a) where
  writeImpl = toForeign <<< StrMap.mapWithKey (const writeImpl)

instance recordWriteForeign ::
  ( RowToList row rl
  , WriteForeignFields rl row () to
  ) => WriteForeign (Record row) where
  writeImpl rec = toForeign $ Builder.build steps {}
    where
      rlp = RLProxy :: RLProxy rl
      steps = writeImplFields rlp rec

class WriteForeignFields (rl :: RowList) row (from :: # Type) (to :: # Type)
  | rl -> row from to where
  writeImplFields :: forall g. g rl -> Record row -> Builder (Record from) (Record to)

instance consWriteForeignFields ::
  ( IsSymbol name
  , WriteForeign ty
  , WriteForeignFields tail row from from'
  , RowCons name ty whatever row
  , RowLacks name from'
  , RowCons name Foreign from' to
  ) => WriteForeignFields (Cons name ty tail) row from to where
  writeImplFields _ rec = result
    where
      namep = SProxy :: SProxy name
      value = writeImpl $ get namep rec
      tailp = RLProxy :: RLProxy tail
      rest = writeImplFields tailp rec
      result = Builder.insert namep value <<< rest
instance nilWriteForeignFields ::
  WriteForeignFields Nil row () () where
  writeImplFields _ _ = id

instance writeForeignVariant ::
  ( RowToList row rl
  , WriteForeignVariant rl row
  ) => WriteForeign (Variant row) where
  writeImpl variant = writeVariantImpl (RLProxy :: RLProxy rl) variant

class WriteForeignVariant (rl :: RowList) (row :: # Type)
  | rl -> row where
  writeVariantImpl :: forall g. g rl -> Variant row -> Foreign

instance nilWriteForeignVariant ::
  WriteForeignVariant Nil () where
  writeVariantImpl _ _ =
    -- a PureScript-defined variant cannot reach this path, but a JavaScript FFI one could.
    unsafeCrashWith "Variant was not able to be writen row WriteForeign."

instance consWriteForeignVariant ::
  ( IsSymbol name
  , WriteForeign ty
  , RowCons name ty subRow row
  , WriteForeignVariant tail subRow
  ) => WriteForeignVariant (Cons name ty tail) row where
  writeVariantImpl _ variant = do
    on
      namep
      writeVariant
      (writeVariantImpl (RLProxy :: RLProxy tail))
      variant
    where
    namep = SProxy :: SProxy name
    writeVariant value = toForeign
      { type: reflectSymbol namep
      , value: writeImpl value
      }
