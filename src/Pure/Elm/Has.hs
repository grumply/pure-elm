{-# language RankNTypes, ConstraintKinds, FlexibleContexts, ScopedTypeVariables #-}
module Pure.Elm.Has (Has(..), using, rec, self) where

import Data.Function (fix)
import Data.Proxy
import Unsafe.Coerce

class Has a where
  it :: a

newtype Reading a b = Reading (Has a => b)

with_ :: forall a b. (Has a => b) -> a -> b
with_ b a = unsafeCoerce (Reading b :: Reading a b) a
{-# INLINE with_ #-}

using :: forall a r. a -> (Has a => r) -> r
using a k = with_ (\_ -> k) a Proxy
{-# INLINE using #-}

data Fixed a = Fixed a
type Rec a = Has (Fixed a)

self :: Rec a => a
self = let Fixed a = it in a

rec :: (Rec a => a) -> a
rec a = fix (\self -> using (Fixed self) a) 

