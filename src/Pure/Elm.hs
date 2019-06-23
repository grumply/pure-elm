{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables #-}
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

data App env st msg = App 
  { _receive :: env -> msg
  , _model   :: Elm msg => st
  , _update  :: Elm msg => msg -> env -> st -> IO st 
  , _view    :: Elm msg => env -> st -> View
  }

-- | Turn an `App st msg` into a component with `msg` property.
run :: forall env st msg. (Typeable env, Typeable st, Typeable msg) => App env st msg -> env -> View
run App {..} env =
  -- the Proxy tags this Component with the App type to
  -- improve correctness of diffing.
  flip Component (Proxy :: Proxy (st,env,msg),env) $ \self ->
    let
      ?command = fix $ \f -> \msg -> modifyM_ self $ \(_,env) mdl -> do
                     mdl' <- let ?command = f in _update msg env mdl
                     pure (mdl',pure ())
    in      
      def { construct = pure _model 
          , receive   = \(_,env) -> _update (_receive env) env
          , render    = \(_,env) -> _view env
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


