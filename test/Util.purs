module Test.Util where

import Prelude

import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record (get)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- | Check two records of the same type for equality.
equal
  :: forall r rs
   . RowToList r rs
  => EqualFields rs r
  => Record r
  -> Record r
  -> Boolean
equal a b = equalFields (Proxy :: Proxy rs) a b

class EqualFields (rs :: RowList Type) (row :: Row Type) | rs -> row where
  equalFields :: Proxy rs -> Record row -> Record row -> Boolean

instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , Row.Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && rest
    where
      get' = get (Proxy :: Proxy name)
      rest = equalFields (Proxy :: Proxy tail) a b

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true
