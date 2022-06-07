{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables, TypeApplications, BangPatterns, MagicHash, 
   AllowAmbiguousTypes, PatternSynonyms, ViewPatterns,
   FlexibleContexts #-}
module Pure.Elm (App(..),pattern Applet,run,command,commandWith,map,module Export,Pure.inline,memo,omem) where

import Pure as Export hiding (Home,update,view,inline)
import qualified Pure (view,update,inline)

import Control.Concurrent (myThreadId,ThreadId)
import Control.Monad
import Data.Function
import Data.Typeable

import Prelude hiding (map)

import Pure.Elm.Has as Has
import Pure.Elm.Sub as Export
import qualified Pure.Elm.Memo as Memo

import Data.Coerce
import GHC.Exts (inline)

import Data.IORef
import System.IO.Unsafe

data App env st msg = App 
  { _startup  :: Elm msg => [msg]
  , _receive  :: Elm msg => [msg]
  , _shutdown :: Elm msg => [msg]
  , _model    :: Elm msg => IO st
  , _update   :: Elm msg => msg -> env -> st -> IO st
  , _view     :: Elm msg => env -> st -> View
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
run (inline -> App {..}) = Component app . (Env @msg)
  where
    app self =
      let
        {-# INLINE upd #-}
        upd :: msg -> IO () -> IO Bool
        upd msg after = modifyM self $ \env mdl -> do
          mdl' <- Has.using upd (inline _update msg (coerce env) mdl)
          pure (mdl',after)
      in
        Has.using upd $
          let
            {-# INLINE update #-}
            update :: env -> st -> [msg] -> IO st
            update env mdl msgs = go mdl msgs
              where
                go :: Elm msg => st -> [msg] -> IO st
                go mdl [] = pure mdl
                go mdl (msg:msgs) = do
                  mdl' <- inline _update msg env mdl
                  go mdl' msgs
          in 
            def 
              { construct = inline _model
              , executing = \mdl -> do
                env <- ask self
                inline update (coerce env) mdl _startup
              , receive = \env mdl ->
                inline update (coerce env) mdl _receive
              , unmounted = do
                env <- ask self
                mdl <- get self
                inline update (coerce env) mdl _shutdown
                myThreadId >>= Memo.cleanupThreadStore
              , render = inline _view . coerce
              }

-- | Given a satisfied `Elm msg` constraint, send a command.
{-# INLINE command #-}
command :: Effect msg => msg -> IO ()
command msg = commandWith msg (pure ())

-- | Given a satisfied `Elm msg` constraint, send a command with an action
-- to perform after evaluate of the command.
{-# INLINE commandWith #-}
commandWith :: Effect msg => msg -> IO () -> IO ()
commandWith msg after = void (effect msg after)

{-# INLINE memo #-}
memo :: forall tag a b msg. (Typeable tag,Typeable a, Typeable b, Elm msg) => (b -> msg) -> (a -> IO b) -> a -> IO ()
memo msg f a = do
  b <- Memo.memo @(tag,a) f a
  Memo.memo @(tag,b) (command . msg) b

{-# INLINE omem #-}
omem :: forall tag a b msg. (Typeable tag, Typeable a, Typeable b, Elm msg) => (b -> msg) -> a -> (a -> IO b) -> IO ()
omem msg a f = memo @tag msg f a
