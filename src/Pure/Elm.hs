{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables #-}
module Pure.Elm (App(..),run,command,map,module Export) where

import Pure as Export hiding (Home,update,view)
import qualified Pure (view)

import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

import Prelude hiding (map)

import Pure.Elm.Subscriptions as Export

data App env st msg = App 
  { _receive  :: Maybe (env -> msg)
  , _shutdown :: Maybe msg
  , _model    :: Elm msg => st
  , _update   :: Elm msg => msg -> env -> st -> IO st 
  , _view     :: Elm msg => env -> st -> View
  }

instance Default (App env st msg) where
  def = App Nothing Nothing (error "No default model supplied.") (\_ _ -> pure) (\_ _ -> Null)

-- | Turn an `App st msg` into a component with `msg` property.
run :: forall env st msg. (Typeable env, Typeable st, Typeable msg) => App env st msg -> env -> View
run App {..} env =
  -- the Proxy tags this Component with the App type to
  -- improve correctness of diffing.
  flip Component (Proxy :: Proxy (st,env,msg),env) $ \self ->
    let
      ?command = fix $ \f -> \msg -> modifyM self $ \(_,env) mdl -> do
                     mdl' <- let ?command = f in _update msg env mdl
                     pure (mdl',pure ())
    in      
      def { construct = do
            (_,env) <- ask self
            maybe (pure _model) (\rcv -> _update (rcv env) env _model) _receive
          , receive = \(_,env) -> maybe pure (\rcv -> _update (rcv env) env) _receive
          , unmounted = do
            (_,env) <- ask self
            mdl     <- get self
            maybe (pure ()) (\msg -> void $ _update msg env mdl) _shutdown
          , render    = \(_,env) -> _view env
          }

-- | Given a satisfied `Elm msg` constraint, send a command.
command :: Elm msg => msg -> IO ()
command msg = void $ ?command msg

-- | Map over an `Elm` constraint.
map :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
map f a = 
  let g = ?command
   in let ?command = g . f
       in a


