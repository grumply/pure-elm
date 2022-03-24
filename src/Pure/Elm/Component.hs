{-# language TypeApplications, KindSignatures, ScopedTypeVariables, TypeFamilies, RankNTypes, ConstrainedClassMethods, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Pure.Elm.Component 
  ( module Pure.Elm
  , Update
  , Render
  , Handler
  , Component(..)
  , HasCallStack
  , timed
  ) where

import qualified Pure (Pure(..))
import Pure.Elm hiding (run,receive,initialize)
import qualified Pure.Elm as Pure
import Pure.Data.Lifted (timed)

import Data.Typeable ( Typeable, typeOf )

import GHC.Stack (HasCallStack)

type Update  a = (HasCallStack, Component a, Elm (Msg a)) => a -> Model a -> IO (Model a)
type Render  a = (HasCallStack, Component a, Elm (Msg a)) => Model a -> View
type Handler a = (HasCallStack, Component a, Elm (Msg a)) => Evt -> IO ()

class Typeable (a :: *) => Component a where
  data Model a
  data Msg a
 
  startup :: [Msg a]
  startup = []

  receive :: [Msg a]
  receive = []

  shutdown :: [Msg a]
  shutdown = []

  model :: Model a
  model = error ("No default model defined for " ++ show (typeOf (undefined :: a)))

  -- GHC doesn't let me do this, but it would be nice if GHC would emit this warning for any overriding implementation.
  -- {-# WARNING initialize "Component.initialize is a render-blocking initialization method! Consider moving long-running and long-tail effects to a startup event." #-}
  initialize :: a -> IO (Model a)
  initialize _ = pure model

  upon :: Msg a -> Update a
  upon _ _ mdl = pure mdl

  view :: a -> Render a
  view _ _ = Null
 
  {-# INLINE app #-}
  app :: a -> App a (Model a) (Msg a)
  app a = Applet startup receive shutdown (initialize a) upon view 

  {-# INLINE run #-}
  run :: a -> View
  run a = Pure.run (app a) a

-- This is a troublesome instance, but I think it is necessary. There should 
-- not be two ways to render the same type. So, if you import 
-- Pure.Elm.Component, this instance comes along and makes some type inference
-- annoying and confusing; you'll end up seeing a missing Component instance
-- rather than a missing Pure instance.
instance {-# INCOHERENT #-} (Typeable a, Component a) => Pure.Pure a where
  view = run