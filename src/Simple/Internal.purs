module Simple.Internal where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail, tagOf, unsafeFromForeign)
import Data.StrMap (StrMap)

-- | Test whether a foreign value is a dictionary
isStrMap :: Foreign -> Boolean
isStrMap v = tagOf v == "Object"

-- | Attempt to coerce a foreign value to a StrMap
readStrMap :: Foreign -> F (StrMap Foreign)
readStrMap value
  | isStrMap value = pure $ unsafeFromForeign value
  | otherwise = fail $ TypeMismatch "StrMap" (tagOf value)
