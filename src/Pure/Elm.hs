{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables #-}
module Pure.Elm (App(..),run,command,map,module Export,memo,memo',omem,omem') where

import Pure as Export hiding (Home,update,view)
import qualified Pure (view)

import Control.Concurrent (myThreadId,ThreadId)
import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

import Prelude hiding (map)

import Pure.Elm.Sub as Export
import qualified Pure.Elm.Memo as Memo

data App env st msg = App 
  { _startup  :: Maybe msg
  , _receive  :: Maybe (env -> msg)
  , _shutdown :: Maybe msg
  , _model    :: st
  , _update   :: Elm msg => msg -> env -> st -> IO st 
  , _view     :: Elm msg => env -> st -> View
  }

instance (Typeable env, Typeable st, Typeable msg) => Default (App env st msg) where
  def = 
    let 
      tr = typeOf (undefined :: App env st msg) 
      tm = typeOf (undefined :: st)
     in
      App Nothing Nothing Nothing 
        (error $ "Pure.Elm.def: No default model supplied to " ++ show tr ++ " of type " ++ show tm) 
        (\_ _ -> pure) 
        (\_ _ -> Null)

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
            mdl <- maybe (pure _model) (\str -> _update str env _model) _startup
            (_,env) <- ask self
            maybe (pure mdl) (\rcv -> _update (rcv env) env mdl) _receive
          , receive = \(_,env) -> maybe pure (\rcv -> _update (rcv env) env) _receive
          , unmounted = do
            (_,env) <- ask self
            mdl     <- get self
            maybe (pure ()) (\msg -> void $ _update msg env mdl) _shutdown
            myThreadId >>= Memo.cleanLedger
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

memo :: (Typeable a, Typeable b, Elm msg) => (b -> msg) -> (a -> IO b) -> a -> IO ()
memo msg f a = void (memo' msg f a)

memo' :: (Typeable a, Typeable b, Elm msg) => (b -> msg) -> (a -> IO b) -> a -> IO (Maybe ThreadId)
memo' msg f a = Memo.memo' f (command . msg) a

omem :: (Typeable a, Typeable b, Elm msg) => (b -> msg) -> a -> (a -> IO b) -> IO ()
omem msg a f = memo msg f a

omem' :: (Typeable a, Typeable b, Elm msg) => (b -> msg) -> a -> (a -> IO b) -> IO (Maybe ThreadId)
omem' msg a f = memo' msg f a

