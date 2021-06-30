{-# language TypeApplications, KindSignatures, ScopedTypeVariables, TypeFamilies, RankNTypes, ConstrainedClassMethods, FlexibleInstances #-}
module Pure.Elm.Component 
  ( module Pure.Elm
  , Update
  , Render
  , Handler
  , Component(..)
  ) where

import qualified Pure (Pure(..))
import Pure.Elm hiding (run,receive,initialize)
import qualified Pure.Elm as Pure

import Data.Typeable ( Typeable, typeOf )

type Update  a = (Component a, Elm (Msg a)) => a -> Model a -> IO (Model a)
type Render  a = (Component a, Elm (Msg a)) => Model a -> View
type Handler a = (Component a, Elm (Msg a)) => Evt -> IO ()

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

  initialize :: IO (Model a)
  initialize = pure model

  upon :: Msg a -> Update a
  upon _ _ mdl = pure mdl

  view :: a -> Render a
  view _ _ = Null
 
  {-# INLINE app #-}
  app :: App a (Model a) (Msg a)
  app = Applet startup receive shutdown initialize upon view 

  {-# INLINE run #-}
  run :: a -> View
  run = Pure.run app 

  {-# MINIMAL model | initialize #-}
