{-# LANGUAGE AllowAmbiguousTypes, RecursiveDo, ScopedTypeVariables,
   ImplicitParams, ConstraintKinds #-}
module Pure.Elm.Subscriptions where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Traversable
import Data.Typeable
import Data.Unique
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

import Data.Map as Map

type Elm msg = (?command :: msg -> IO Bool)

type Broker = IORef (Map TypeRep (MVar [(Unique,Any -> IO Bool)]))

{-# NOINLINE broker #-}
broker :: Broker
broker = unsafePerformIO (newIORef mempty)

subscribeWith :: forall msg. Typeable msg => (msg -> IO Bool) -> IO Unique
subscribeWith f = mdo
  let 
    g = f . unsafeCoerce
    tr = typeOf (undefined :: msg)
  u <- newUnique
  mv <- join $ atomicModifyIORef' broker $ \b -> 
    case Map.lookup tr b of
      Nothing -> (Map.insert tr mv b,newMVar [(u,g)])
      Just mv -> (b,modifyMVar_ mv (pure . (++ [(u,g)])) >> pure undefined)
  pure u

subscribe :: (Typeable msg, Elm msg) => IO ()
subscribe = void $ subscribeWith ?command

subscribe' :: (Typeable msg, Elm msg) => Proxy msg -> IO ()
subscribe' _ = void $ subscribeWith ?command

publish :: forall msg. Typeable msg => msg -> IO ()
publish msg = do
  let 
    any = unsafeCoerce msg
    tr = typeOf (undefined :: msg)
  b <- readIORef broker
  case Map.lookup tr b of
    Nothing -> pure ()
    Just mv -> do
      hs <- takeMVar mv
      hs' <- fmap catMaybes $ for hs $ \(u,h) -> do
        b <- h any
        pure $ if b then Just (u,h) else Nothing
      putMVar mv hs'

unsubscribe :: forall msg. (Typeable msg, Elm msg) => Unique -> IO ()
unsubscribe = unsubscribe' (Proxy :: Proxy msg)

unsubscribe' :: forall msg. Typeable msg => Proxy msg -> Unique -> IO ()
unsubscribe' _ u = do
  let tr = typeOf (undefined :: msg)
  b <- readIORef broker
  case Map.lookup tr b of
    Nothing -> pure ()
    Just mv -> modifyMVar_ mv (pure . Prelude.filter ((/= u) . fst))

