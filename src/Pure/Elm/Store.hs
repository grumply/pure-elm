{-# language FlexibleContexts, ScopedTypeVariables, Rank2Types, ConstraintKinds, BlockArguments, TupleSections, BangPatterns #-}
module Pure.Elm.Store (Store,State,store,storeIO,substore,substoreIO,get,put,updateWith_,updateWith,updateWithM_,updateWithM,update_,update,updateM_,updateM,use,react,Has,using,it) where

import Pure.Elm.Fold hiding (get,put,use,using,State)
import Pure.Elm.Has 

import Control.Concurrent
import Control.Monad (join,unless,(>=>))
import Data.IORef
import Data.Typeable
import GHC.Magic
import GHC.Exts
import System.IO.Unsafe
import System.Mem.Weak

import Prelude hiding (read)

type Store a = MVar (a,[IORef (a -> IO ())])

store :: IO a -> Store a
store ioa = noinline $ unsafePerformIO $ storeIO ioa

storeIO :: IO a -> IO (Store a)
storeIO ioa = ioa >>= \a -> newMVar (a,[])

substore :: Store a -> (a -> IO b) -> Store b
substore s f = noinline $ unsafePerformIO $ substoreIO s f

substoreIO :: Store a -> (a -> IO b) -> IO (Store b)
substoreIO s f = do
  b  <- using s (get >>= f)
  mv <- newMVar (b,[])
  f_ <- newIORef undefined
  let cleanup = modifyMVar_ s $ \(x,fs) -> pure (x,filter (/= f_) fs)
  w <- mkWeakMVar mv cleanup
  let
    f' a = do
      ms <- deRefWeak w
      case ms of
        Nothing -> cleanup
        Just mv -> do
          b <- f a
          updateWithM_ mv (pure . const b)
  writeIORef f_ f'
  modifyMVar_ s $ \(x,fs) -> pure (x,fs ++ [f_])
  pure mv

updateWith_ :: Store a -> (a -> a) -> IO ()
updateWith_ s f = updateWithM_ s (pure . f)
{-# INLINE updateWith_ #-}

updateWith :: Store a -> (a -> (a,b)) -> IO b
updateWith s f = updateWithM s (pure . f)
{-# INLINE updateWith #-}

updateWithM_ :: Store a -> (a -> IO a) -> IO ()
updateWithM_ s f = updateWithM s (f >=> pure . (,()))
{-# INLINE updateWithM_ #-}

updateWithM :: Store a -> (a -> IO (a,b)) -> IO b
updateWithM s f = join $ modifyMVar s $ \(x,fs) -> do
  (x',b) <- f x
  let run = sequence_ (fmap (readIORef >=> ($ x')) fs)
  pure ((x',fs), run >> pure b)
{-# INLINE updateWithM #-}

update_ :: Has (Store a) => (a -> a) -> IO ()
update_ f = updateM_ (pure . f)
{-# INLINE update_ #-}

update :: Has (Store a) => (a -> (a,b)) -> IO b
update f = updateM (pure . f) 
{-# INLINE update #-}

updateM_ :: Has (Store a) => (a -> IO a) -> IO ()
updateM_ f = updateM (f >=> pure . (,()))
{-# INLINE updateM_ #-}

updateM :: Has (Store a) => (a -> IO (a,b)) -> IO b
updateM = updateWithM it 
{-# INLINE updateM #-}

put :: Has (Store a) => a -> IO ()
put x = update_ (const x)
{-# INLINE put #-}

get :: forall a. Has (Store a) => IO a
get = fst <$> readMVar (it :: Store a)
{-# INLINE get #-}

-- This constraint is introduced with `reacting` or its derivatives 
-- `react` or `reactive` and is witnessed with `with`. 
type State a = (Has (Store a), Has a)

-- This is only valid inside of a `react` where the `State a` constraint
-- can be properly satisfied. Do not try to satisfy the constraint with 
-- `using` as it will not be reactive to changes and could cause a
-- memory leak!
use :: (a -> b) -> (State a => b)
use = ($ it)
{-# INLINE use #-}

-- Construct a reactive context from a store. Changes to the store will be
-- propagated to users of the `State a` constraint (specifically, `with`
-- in the body of the third argument of `react`).
react :: forall a. Typeable a => Store a -> (a -> IO ()) -> (State a => View) -> View
react w f g = foldM (\a _ -> f a >> pure a) initialize (using w g)
  where
    initialize :: Elm a => IO (a,a -> IO())
    initialize = do
      f_ <- newIORef undefined
      let remove = modifyMVar_ w (\(a,fs) -> pure (a,filter (/= f_) fs))
      writeIORef f_ $ \x ->do
        alive <- effect x (pure ())
        unless alive remove
      a <- modifyMVar w (\(x,fs) -> pure ((x,fs ++ [f_]),x))
      f a
      pure (a,const remove)
{-# INLINE react #-}

