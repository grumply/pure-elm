{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables, TypeApplications, BangPatterns, MagicHash, 
   AllowAmbiguousTypes, PatternSynonyms #-}
module Pure.Elm (App(..),pattern Applet,run,command,map,module Export,memo,omem) where

import Pure as Export hiding (Home,update,view)
import qualified Pure (view,update)

import Control.Concurrent (myThreadId,ThreadId)
import Control.Monad
import Data.Function
import Data.Typeable

import Prelude hiding (map)

import Pure.Elm.Sub as Export
import qualified Pure.Elm.Memo as Memo

import Data.Coerce

data App env st msg = App 
  { _startup  :: [msg]
  , _receive  :: [msg]
  , _shutdown :: [msg]
  , _model    :: st
  , _update   :: !(Elm msg => msg -> env -> st -> IO st)
  , _view     :: !(Elm msg => env -> st -> View)
  }

instance (Typeable env, Typeable st, Typeable msg) => Default (App env st msg) where
  def = 
    let 
      tr = typeOf (undefined :: App env st msg) 
      tm = typeOf (undefined :: st)
     in
      App [] [] []
        (error $ "Pure.Elm.def: No default model supplied to " ++ show tr ++ " of type " ++ show tm) 
        (\_ _ -> pure) 
        (\_ _ -> Null)

newtype ElmEnv msg env = Env env

pattern Applet s r d m u v = App s r d m u v

-- | Turn an `App st msg` into a component with `msg` property.
{-# INLINE run #-}
run :: forall env st msg. (Typeable env, Typeable st, Typeable msg) => App env st msg -> env -> View
run App {..} = Component app . (Env @msg)
  where
    app self =
      let
        {-# INLINE upd #-}
        upd msg = modifyM self $ \env mdl -> do
            mdl' <- let ?command = upd in _update msg (coerce env) mdl
            pure (mdl',pure ())

      in let 
        ?command = upd
      in let
        {-# INLINE update #-}
        update env = go
          where
            {-# NOINLINE go' #-}
            go' = go
            {-# INLINE go #-}
            go mdl [] = pure mdl
            go mdl (msg:msgs) = do
              mdl' <- _update msg env mdl
              go' mdl' msgs
      in 
        def 
          { construct = pure _model
          , executing = \mdl -> do
            env <- ask self
            update (coerce env) mdl _startup
          , receive = \env mdl ->
            update (coerce env) mdl _receive
          , unmounted = do
            env <- ask self
            mdl <- get self
            update (coerce env) mdl _shutdown
            myThreadId >>= Memo.cleanupThreadStore
          , render = _view . coerce
          }

-- | Given a satisfied `Elm msg` constraint, send a command.
{-# INLINE command #-}
command :: Elm msg => msg -> IO ()
command msg = void $ ?command msg

-- | Map over an `Elm` constraint.
{-# INLINE map #-}
map :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
map f a = 
  let g = ?command
   in let ?command = g . f
       in a

{-# INLINE memo #-}
memo :: forall tag a b msg. (Typeable tag,Typeable a, Typeable b, Elm msg) => (b -> msg) -> (a -> IO b) -> a -> IO ()
memo msg f a = do
  b <- Memo.memo @(tag,a) f a
  Memo.memo @(tag,b) (command . msg) b

{-# INLINE omem #-}
omem :: forall tag a b msg. (Typeable tag, Typeable a, Typeable b, Elm msg) => (b -> msg) -> a -> (a -> IO b) -> IO ()
omem msg a f = memo @tag msg f a