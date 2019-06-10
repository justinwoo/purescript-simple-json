module Simple.JSON
( E
, readJSON
, readJSON'
, readJSON_
, writeJSON
, write
, read
, read'
, read_
, parseJSON
, undefined

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
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExcept)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Variant (Variant, inj, on)
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, isNull, isUndefined, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, tagOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object as Object
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (get)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (RLProxy(..))

-- | An alias for the Either result of decoding
type E a = Either MultipleErrors a

-- | Read a JSON string to a type `a` while returning a `MultipleErrors` if the
-- | parsing failed.
readJSON :: forall a
  .  ReadForeign a
  => String
  -> E a
readJSON = runExcept <<< (readImpl <=< parseJSON)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON' :: forall a
  .  ReadForeign a
  => String
  -> F a
readJSON' = readImpl <=< parseJSON

-- | Read a JSON string to a type `a` while returning `Nothing` if the parsing
-- | failed.
readJSON_ ::  forall a
   . ReadForeign a
  => String
  -> Maybe a
readJSON_ = hush <<< readJSON

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

-- | Read a Foreign value to a type
read :: forall a
   . ReadForeign a
  => Foreign
  -> E a
read = runExcept <<< readImpl

-- | Read a value of any type as Foreign to a type
readAsForeign :: forall a b
   . ReadForeign a
  => b
  -> E a
readAsForeign = read <<< unsafeToForeign

read' :: forall a
  .  ReadForeign a
  => Foreign
  -> F a
read' = readImpl

-- | Read a Foreign value to a type, as a Maybe of type
read_ :: forall a
   . ReadForeign a
  => Foreign
  -> Maybe a
read_ = hush <<< read

foreign import _parseJSON :: EU.EffectFn1 String Foreign

parseJSON :: String -> F Foreign
parseJSON
    = ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< runPure
  <<< try
  <<< EU.runEffectFn1 _parseJSON
  where
    -- Nate Faubion: "It uses unsafePerformEffect because that’s the only way to catch exceptions and still use the builtin json decoder"
    runPure = unsafePerformEffect

foreign import _undefined :: Foreign

undefined :: Foreign
undefined = _undefined

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
  readImpl = traverseWithIndex readAtIdx <=< readArray
    where
      readAtIdx i f = withExcept (map (ErrorAtIndex i)) (readImpl f)

instance readMaybe :: ReadForeign a => ReadForeign (Maybe a) where
  readImpl = readNullOrUndefined readImpl
    where
      readNullOrUndefined _ value | isNull value || isUndefined value = pure Nothing
      readNullOrUndefined f value = Just <$> f value

instance readNullable :: ReadForeign a => ReadForeign (Nullable a) where
  readImpl o = withExcept (map reformat) $
    map toNullable <$> traverse readImpl =<< readNull o
    where
      reformat error = case error of
        TypeMismatch inner other -> TypeMismatch ("Nullable " <> inner) other
        _ -> error

instance readObject :: ReadForeign a => ReadForeign (Object.Object a) where
  readImpl = sequence <<< Object.mapWithKey (const readImpl) <=< readObject'
    where
      readObject' :: Foreign -> F (Object Foreign)
      readObject' value
        | tagOf value == "Object" = pure $ unsafeFromForeign value
        | otherwise = fail $ TypeMismatch "Object" (tagOf value)


instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList () fields
  ) => ReadForeign (Record fields) where
  readImpl o = flip Builder.build {} <$> getFields fieldListP o
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
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => ReadForeignFields (Cons name ty tail) from to where
  getFields _ obj = (compose <$> first) `exceptTApply` rest
    where
      first = do
        value <- withExcept' (readImpl =<< readProp name obj)
        pure $ Builder.insert nameP value
      rest = getFields tailP obj
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtProperty name

exceptTApply :: forall a b e m. Semigroup e => Applicative m => ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
exceptTApply fun a = ExceptT $ applyEither
  <$> runExceptT fun
  <*> runExceptT a

applyEither :: forall e a b. Semigroup e => Either e (a -> b) -> Either e a -> Either e b
applyEither (Left e) (Right _) = Left e
applyEither (Left e1) (Left e2) = Left (e1 <> e2)
applyEither (Right _) (Left e) = Left e
applyEither (Right fun) (Right a) = Right (fun a)

instance readFieldsNil ::
  ReadForeignFields Nil () () where
  getFields _ _ =
    pure identity

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
  , Row.Cons name ty trash row
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

-- -- | A class for writing a value into JSON
-- -- | need to do this intelligently using Foreign probably, because of null and undefined whatever
class WriteForeign a where
  writeImpl :: a -> Foreign

instance writeForeignForeign :: WriteForeign Foreign where
  writeImpl = identity

instance writeForeignString :: WriteForeign String where
  writeImpl = unsafeToForeign

instance writeForeignInt :: WriteForeign Int where
  writeImpl = unsafeToForeign

instance writeForeignChar :: WriteForeign Char where
  writeImpl = unsafeToForeign

instance writeForeignNumber :: WriteForeign Number where
  writeImpl = unsafeToForeign

instance writeForeignBoolean :: WriteForeign Boolean where
  writeImpl = unsafeToForeign

instance writeForeignArray :: WriteForeign a => WriteForeign (Array a) where
  writeImpl xs = unsafeToForeign $ writeImpl <$> xs

instance writeForeignMaybe :: WriteForeign a => WriteForeign (Maybe a) where
  writeImpl = maybe undefined writeImpl

instance writeForeignNullable :: WriteForeign a => WriteForeign (Nullable a) where
  writeImpl = maybe (unsafeToForeign $ toNullable Nothing) writeImpl <<< toMaybe

instance writeForeignObject :: WriteForeign a => WriteForeign (Object.Object a) where
  writeImpl = unsafeToForeign <<< Object.mapWithKey (const writeImpl)

instance recordWriteForeign ::
  ( RowToList row rl
  , WriteForeignFields rl row () to
  ) => WriteForeign (Record row) where
  writeImpl rec = unsafeToForeign $ Builder.build steps {}
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
  , Row.Cons name ty whatever row
  , Row.Lacks name from'
  , Row.Cons name Foreign from' to
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
  writeImplFields _ _ = identity

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
  , Row.Cons name ty subRow row
  , WriteForeignVariant tail subRow
  ) => WriteForeignVariant (Cons name ty tail) row where
  writeVariantImpl _ variant =
    on
      namep
      writeVariant
      (writeVariantImpl (RLProxy :: RLProxy tail))
      variant
    where
    namep = SProxy :: SProxy name
    writeVariant value = unsafeToForeign
      { type: reflectSymbol namep
      , value: writeImpl value
      }

instance readForeignNEArray :: ReadForeign a => ReadForeign (NonEmptyArray a) where
  readImpl f = do
    raw :: Array a <- readImpl f
    except $ note (singleton $ ForeignError "Nonempty array expected, got empty array") $ fromArray raw

instance writeForeignNEArray :: WriteForeign a => WriteForeign (NonEmptyArray a) where
  writeImpl a = writeImpl <<< toArray $ a
