{-# LANGUAGE ImplicitParams, ScopedTypeVariables, ExistentialQuantification, TupleSections, AllowAmbiguousTypes, TypeApplications, ViewPatterns, BangPatterns #-}
module Pure.Elm.Memo where

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Hashable
import Data.Map.Strict as Map
import Data.IntMap.Strict as IntMap
import Data.Typeable
import System.Mem.StableName
import System.IO.Unsafe

data Value = forall a b. (Typeable a, Typeable b) => Value {-# UNPACK #-}!(StableName a) !b

type Ledger = MVar (IntMap (MVar ThreadStore))

type ThreadStore = Map TypeRep Value

{-# NOINLINE ledger #-}
ledger :: Ledger 
ledger = unsafePerformIO (newMVar IntMap.empty)

modifyThreadStore :: (ThreadStore -> IO (ThreadStore,b)) -> IO b
modifyThreadStore f = do
  tid <- myThreadId
  ts  <- modifyMVar ledger (go tid)
  !b  <- modifyMVar ts (\ts -> f ts >>= \(ts',b) -> ts' `seq` pure (ts',b))
  pure b
  where
    go :: ThreadId -> IntMap (MVar ThreadStore) -> IO (IntMap (MVar ThreadStore),MVar ThreadStore)
    go tid@(hash -> htid) ldgr
      | Just ts <- IntMap.lookup htid ldgr = pure (ldgr,ts)
      | otherwise = do
        ts  <- newMVar Map.empty
        let ldgr' = IntMap.insert htid ts ldgr
        ldgr' `seq` pure (ldgr',ts)

withThreadStore :: (ThreadStore -> IO a) -> IO a
withThreadStore f = modifyThreadStore $ \ts -> do
  !a <- f ts
  pure (ts,a)

removeThreadStore :: Int -> IO ()
removeThreadStore htid = modifyMVar_ ledger (\l -> let l' = IntMap.delete htid l in l' `seq` pure l')

lookupValue :: forall tag a b. (Typeable tag, Typeable a, Typeable b) => StableName a -> IO (Maybe b)
lookupValue sna = let tag = typeOf (undefined :: tag) in
  withThreadStore $ \ts ->
    case Map.lookup tag ts of
      Just (Value sna' b) | eqStableName sna sna' -> pure (cast b)
      _ -> pure Nothing

insertValue :: forall tag a b. (Typeable tag, Typeable a, Typeable b) => StableName a -> b -> IO ()
insertValue sna b = modifyThreadStore insert
  where
    tag = typeOf (undefined :: tag)
    insert ts = 
      let ts' = Map.insert tag (Value sna b) ts
      in ts' `seq` pure (ts',())

memo :: forall tag a b. (Typeable tag, Typeable a,Typeable b) => (a -> IO b) -> a -> IO b
memo f a = do
  sna <- makeStableName a
  mv  <- lookupValue @tag sna 
  case mv of
    Nothing -> do
      !b <- f a
      insertValue @tag sna b
      pure b
    Just b -> 
      pure b

cleanupThreadStore :: ThreadId -> IO ()
cleanupThreadStore tid = modifyMVar_ ledger (\l -> let l' = IntMap.delete (hash tid) l in l' `seq` pure l')