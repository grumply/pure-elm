{-# language RankNTypes #-}
module Pure.Elm.Has (Has(..), using) where

import Data.Proxy
import GHC.Exts

class Has a where
  it :: a

data Reading a b = Reading (Has a => Proxy a -> b)

with_ :: (Has a => Proxy a -> b) -> a -> Proxy a -> b
with_ f a p = magicDict (Reading f) a p
{-# INLINE with_ #-}

using :: forall a r. a -> (Has a => r) -> r
using a k = with_ (\_ -> k) a Proxy
{-# INLINE using #-}
