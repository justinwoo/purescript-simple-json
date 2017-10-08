module Simple.JSON (
  readJSON
, readJSON'
, writeJSON
, write
, read

, class ReadForeign
, readImpl
, class ReadForeignFields
, getFields

, class WriteForeign
, writeImpl

, class WriteForeignFields
, writeImplFields

) where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Array (length)
import Data.Either (Either)
import Data.Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined(NullOrUndefined), readNullOrUndefined, unNullOrUndefined, undefined)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Record (get)
import Data.Record.Builder (Builder)
import Data.Record.Builder as Builder
import Data.StrMap as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
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
  .  ReadForeign a
  => Foreign
  -> F a
read = readImpl

-- | A class for reading foreign values to a type
class ReadForeign a where
  readImpl :: Foreign -> F a

instance readForeignForeign :: ReadForeign Foreign where
  readImpl = pure

instance readForeignUnit :: ReadForeign Unit where
  readImpl = const $ pure unit

instance readForeignChar :: ReadForeign Char where
  readImpl = readChar

instance readForeignNumber :: ReadForeign Number where
  readImpl = readNumber

instance readForeignInt :: ReadForeign Int where
  readImpl = readInt

instance readForeignString :: ReadForeign String where
  readImpl = readString

instance readForeignBoolean :: ReadForeign Boolean where
  readImpl = readBoolean

instance readForeignArray :: ReadForeign a => ReadForeign (Array a) where
  readImpl = readElements <=< readArray
    where
      readElements xs = sequence $ readImpl <$> xs

instance readForeignNullOrUndefined :: ReadForeign a => ReadForeign (NullOrUndefined a) where
  readImpl = readNullOrUndefined readImpl

instance readForeignMaybe :: ReadForeign a => ReadForeign (Maybe a) where
  readImpl = map unNullOrUndefined <<< readImpl

instance readForeignNullable :: ReadForeign a => ReadForeign (Nullable a) where
  readImpl o = withExcept (map reformat) $
    map toNullable <$> traverse readImpl =<< readNull o
    where
      reformat error = case error of
        TypeMismatch inner other -> TypeMismatch ("Nullable " <> inner) other
        _ -> error

instance readForeignStrMap :: ReadForeign a => ReadForeign (StrMap.StrMap a) where
  readImpl = sequence <<< StrMap.mapWithKey (const readImpl) <=< readStrMap

instance readForeignTuple :: (ReadForeign a, ReadForeign b) => ReadForeign (Tuple a b) where
  readImpl = asTuple <=< readArray
    where asTuple :: Array Foreign -> F (Tuple a b)
          asTuple = case _ of
            [a, b] -> do
              ra <- readImpl a
              rb <- readImpl b
              pure $ Tuple ra rb
            l -> fail $ TypeMismatch "2 values" (show (length l) <> " values")

instance readForeignRecord ::
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

instance readForeignFieldsCons ::
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

instance readForeignFieldsNil ::
  ReadForeignFields Nil () () where
  getFields _ _ =
    pure id

-- | A class for writing a value into JSON
-- | need to do this intelligently using Foreign probably, because of null and undefined whatever
class WriteForeign a where
  writeImpl :: a -> Foreign

instance writeForeignForeign :: WriteForeign Foreign where
  writeImpl = id

instance writeForeignUnit :: WriteForeign Unit where
  writeImpl = toForeign

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

instance writeForeignMaybe :: WriteForeign a => WriteForeign (Maybe a) where
  writeImpl (Just a) = writeImpl a
  writeImpl Nothing = toForeign $ toNullable Nothing

instance writeForeignNullable :: WriteForeign a => WriteForeign (Nullable a) where
  writeImpl = writeImpl <<< toMaybe

instance writeForeignStrMap :: WriteForeign a => WriteForeign (StrMap.StrMap a) where
  writeImpl = toForeign <<< StrMap.mapWithKey (const writeImpl)

instance writeForeignTuple :: (WriteForeign a, WriteForeign b) => WriteForeign (Tuple a b) where
  writeImpl (Tuple a b) = writeImpl [writeImpl a, writeImpl b]

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
