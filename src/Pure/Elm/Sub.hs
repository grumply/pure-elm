{-# LANGUAGE AllowAmbiguousTypes, RecursiveDo, ScopedTypeVariables,
   ImplicitParams, ConstraintKinds, TypeApplications, RankNTypes #-}
module Pure.Elm.Sub where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import Data.Foldable
import Data.List as List
import Data.Maybe
import Data.Traversable
import Data.Typeable
import Data.Unique
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

import Data.Map as Map hiding ((\\))

type Elm msg = (?command :: msg -> IO () -> IO Bool)

data Subscription msg = Subscription Unique

type Dispatcher = Either [Any] [(Unique,Any -> IO Bool)]
type Broker = TMVar (Map TypeRep Dispatcher)

{-# NOINLINE broker #-}
broker :: Broker
broker = unsafePerformIO (newTMVarIO mempty)

-- Be careful not to block in the callback.
unsafeSubscribeWith :: forall msg. Typeable msg => (msg -> IO Bool) -> IO (Subscription msg)
unsafeSubscribeWith f = mdo
  let 
    g = f . unsafeCoerce
    tr = typeOf (undefined :: msg)
  u <- newUnique
  join $ atomically $ do
    b <- takeTMVar broker
    case Map.lookup tr b of
      Nothing -> do
        putTMVar broker (Map.insert tr (Right [(u,g)]) b)
        pure (pure ())
      Just (Left as) -> do
        putTMVar broker (Map.insert tr (Right [(u,g)]) b)
        pure (for_ (Prelude.reverse as) g)
      Just (Right hs) -> do
        putTMVar broker (Map.insert tr (Right (hs ++ [(u,g)])) b)
        pure (pure ())
  pure (Subscription u)

subscribeWith :: (Typeable msg', Elm msg) => (msg' -> msg) -> IO (Subscription msg') 
subscribeWith f = unsafeSubscribeWith (\x -> ?command (f x) (pure ()))

subscribe :: (Typeable msg, Elm msg) => IO (Subscription msg)
subscribe = unsafeSubscribeWith (\x -> ?command x (pure ()))

publish :: Typeable msg => msg -> IO ()
publish = void . publish'

publish' :: forall msg. Typeable msg => msg -> IO Bool
publish' msg = do
  let 
    any = unsafeCoerce msg
    tr = typeOf (undefined :: msg)
  join $ atomically $ do
    b <- takeTMVar broker
    case Map.lookup tr b of
      Nothing -> do
        putTMVar broker (Map.insert tr (Left [any]) b)
        pure (pure False)
      Just (Left as) -> do
        putTMVar broker (Map.insert tr (Left (any:as)) b)
        pure (pure False)
      Just (Right hs) -> do
        putTMVar broker b
        pure $ do
          hs' <- fmap catMaybes $ for hs $ \(u,h) -> do
            b <- h any
            pure $ if b then Just (u,h) else Nothing
          let dead = fmap fst hs \\ fmap fst hs'
          case hs' of
            [] -> do
              cleanBroker @msg dead
              publish' msg
            _  -> 
              pure True

publishing :: (Typeable msg) => (Elm msg => a) -> a
publishing a = let ?command = \m _ -> publish' m in a

cleanBroker :: forall msg. Typeable msg => [Unique] -> IO ()
cleanBroker us = do
  let tr = typeOf (undefined :: msg)
  atomically $ do 
    b <- takeTMVar broker
    case Map.lookup tr b of
      Just (Right hs) -> do
        let hs' = List.filter ((not . (`elem` us)) . fst) hs
        putTMVar broker (Map.insert tr (Right hs') b)
      _ -> putTMVar broker b

unsubscribe :: forall msg. (Typeable msg, Elm msg) => Subscription msg -> IO ()
unsubscribe (Subscription u) = cleanBroker @msg [u] 
