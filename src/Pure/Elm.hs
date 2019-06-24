{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables #-}
module Pure.Elm (Elm,App(..),run,command,map,module Export) where

import Pure as Export hiding (Home,update,view,receive)
import qualified Pure (view,receive)

import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

import Prelude hiding (map)

-- | An implicit constraint to propagate an elm context.
type Elm msg = (?command :: msg -> IO ())

data App env st msg = App 
  { receive  :: Maybe (env -> msg)
  , shutdown :: Maybe msg
  , model    :: Elm msg => st
  , update   :: Elm msg => msg -> env -> st -> IO st 
  , view     :: Elm msg => env -> st -> View
  }

instance Default (App env st msg) where
  def = App Nothing Nothing model update view
    where
      model = error "No default model."
      update _ _ st = pure st
      view _ _ = Null

-- | Turn an `App st msg` into a component with `msg` property.
run :: forall env st msg. (Typeable env, Typeable st, Typeable msg) => App env st msg -> env -> View
run App {..} env =
  -- the Proxy tags this Component with the App type to
  -- improve correctness of diffing.
  flip Component (Proxy :: Proxy (st,env,msg),env) $ \self ->
    let
      ?command = fix $ \f -> \msg -> modifyM_ self $ \(_,env) mdl -> do
                     mdl' <- let ?command = f in update msg env mdl
                     pure (mdl',pure ())
    in      
      def { construct = do
            (_,env) <- ask self
            maybe (pure model) (\rcv -> update (rcv env) env model) receive
          , Pure.receive = \(_,env) -> maybe pure (\rcv -> update (rcv env) env) receive
          , unmounted = do
            (_,env) <- ask self
            mdl     <- get self
            maybe (pure ()) (\msg -> void $ update msg env mdl) shutdown
          , render    = \(_,env) -> view env
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


