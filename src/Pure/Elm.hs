{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards #-}
module Pure.Elm (Elm,App(..),run,command,map,module Export) where

import Pure as Export hiding (Home,update,view)
import qualified Pure (view)

import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

import Prelude hiding (map)

-- | An implicit constraint to propagate an elm context.
type Elm msg = (?command :: msg -> IO ())

data App st msg = App 
  { _model  :: Elm msg => st
  , _update :: Elm msg => msg -> st -> IO st 
  , _view   :: Elm msg => st -> View
  }

-- | Turn an `App st msg` into a component with `msg` property.
run :: (Typeable st, Typeable msg) => App st msg -> (msg -> View)
run App {..} =
  Component $ \self ->
    let
      ?command = fix $ \f -> \msg -> modifyM_ self $ \_ mdl -> do
                     mdl' <- let ?command = f in _update msg mdl
                     pure (mdl',pure ())
    in      
      def { construct = return _model
          , receive   = _update
          , render    = \_ -> _view
          }

-- | Given a satisfied `Elm msg` constraint, send a command.
command :: Elm msg => msg -> IO ()
command msg = ?command msg

-- | Map over an `Elm` constraint.
map :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
map f a = 
  let g = ?command
   in let ?command = g . f
       in a


