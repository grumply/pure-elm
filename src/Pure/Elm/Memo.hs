{-# LANGUAGE ImplicitParams, ScopedTypeVariables, ExistentialQuantification #-}
module Pure.Elm.Memo where

import Control.Concurrent
import Control.Monad
import Data.Map as Map
import Data.Typeable
import System.Mem.StableName
import System.Mem.Weak
import System.IO.Unsafe

data Tracked = forall a b. (Typeable a, Typeable b) => Tracked (StableName a) (Weak b)

{-# NOINLINE ledger #-}
ledger :: MVar (Map ThreadId (MVar (Map TypeRep Tracked)))
ledger = unsafePerformIO (newMVar Map.empty)

memo' :: forall a b. (Typeable a,Typeable b) => (a -> IO b) -> (b -> IO ()) -> (a -> IO (Maybe ThreadId))
memo' f with a = do
  tid <- myThreadId
  sna <- makeStableName a
  let 
    tr = typeOf (undefined :: a,undefined :: b)

    clean = join $ modifyMVar ledger $ \ldgr -> do
      case Map.lookup tid ldgr of
        Nothing -> pure (ldgr,pure ())
        Just localStorage -> do
          let delete = modifyMVar_ localStorage (pure . Map.delete tr)
          pure (ldgr,delete)

    insert b = join $ modifyMVar ledger $ \ldgr ->
      case Map.lookup tid ldgr of
        Nothing -> do
          localStorage <- newMVar Map.empty
          pure 
            (Map.insert tid localStorage ldgr,do
              wk <- mkWeakPtr b (Just clean)
              let tracked = Tracked sna wk
              modifyMVar_ localStorage (pure . Map.insert tr tracked)
            )
        Just localStorage ->
          pure 
            (ldgr,do
              wk <- mkWeakPtr b (Just clean)
              let tracked = Tracked sna wk
              modifyMVar_ localStorage (pure . Map.insert tr tracked)
            )

  ldgr <- readMVar ledger
  mt <- maybe (pure Nothing) (fmap (Map.lookup tr) . readMVar) (Map.lookup tid ldgr)

  case mt of
    Just (Tracked old_sna wk) | cast old_sna == Just sna -> do
      mb <- deRefWeak wk
      case join (fmap cast mb) :: Maybe b of
        Just b  -> pure Nothing
        Nothing ->
          fmap Just $ forkIO $ do
            b <- f a
            insert b
            with b
    _ ->
      fmap Just $ forkIO $ do
        b <- f a
        insert b
        with b

cleanLedger :: ThreadId -> IO ()
cleanLedger tid = modifyMVar_ ledger (pure . Map.delete tid)
