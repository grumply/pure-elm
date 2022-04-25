{-# language RankNTypes, ConstraintKinds, FlexibleContexts #-}
module Pure.Elm.Has (Has(..), using, loop, continue) where

import Data.Function (fix)
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

type Continue b c = Has (b -> c) 

continue :: Has (b -> c) => b -> c
continue = it

loop :: b -> (Has (b -> c) => b -> c) -> c
loop b f = flip fix b (\k b -> using k (f b))
