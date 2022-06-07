{-# language RankNTypes, ConstraintKinds, FlexibleContexts, ScopedTypeVariables #-}
module Pure.Elm.Has (Has(..), using, rec, self) where

import Data.Function (fix)
import Data.Proxy (Proxy(..))
import GHC.Exts (magicDict)
import Unsafe.Coerce (unsafeCoerce)

class Has a where
  it :: a

data Reading a b = Reading (Has a => Proxy a -> b)

with_ :: forall a b. (Has a => Proxy a -> b) -> a -> Proxy a -> b
with_ b a p = magicDict (Reading b) a p
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

