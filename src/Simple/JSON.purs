module Simple.JSON where

import Prelude

import Data.Foreign (F, Foreign, readArray, readBoolean, readChar, readInt, readNumber, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined, readNullOrUndefined)
import Data.StrMap (StrMap, empty, singleton, union)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Type.Proxy (Proxy(..))
import Type.Row (class ListToRow, class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON :: forall a
  .  ReadForeign a
  => String
  -> F a
readJSON = readImpl <=< parseJSON

-- | A class for reading foreign values to a type. Warning: This class should not be instantiated. 
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

instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList
  , ListToRow fieldList fields
  ) => ReadForeign (Record fields) where
  readImpl = unsafeCoerce $ getFields (Proxy :: Proxy (Record fields))

-- | A class for reading foreign values from properties. Warning: This class should not be instantiated.
class ReadForeignFields (xs :: RowList) where
  getFields :: forall fields
    .  RowToList fields xs
    => ListToRow xs fields
    => Proxy (Record fields)
    -> Foreign
    -> F (StrMap Foreign)

instance readFieldsCons ::
  ( IsSymbol name
  , ReadForeign ty
  , ListToRow tail tailRow
  , ReadForeignFields tail
  , RowToList tailRow tail
  ) => ReadForeignFields (Cons name ty tail) where
  getFields _ obj = do
    field <- readProp name obj
    first :: ty <- readImpl field
    rest <- getFields (Proxy :: Proxy (Record tailRow)) obj
    pure $ union (singleton name field) rest
    where
      name = reflectSymbol (SProxy :: SProxy name)

instance readFieldsNil :: ReadForeignFields Nil where
  getFields _ _ = pure empty
