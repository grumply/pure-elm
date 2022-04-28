{-# language TypeFamilies, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, RankNTypes, PartialTypeSignatures, InstanceSigs, AllowAmbiguousTypes, ConstraintKinds, DataKinds #-}
module Pure.Elm.Fold (fold,foldM,module Pure.Elm.Component,module Pure.Elm.Has,Cont,call,shift,reset,Reader,ask,reader,Writer,tell,listen,writer,State,modify,put,get,state,Error,throw,catch,Producing,yield,Consuming,await,Pool,pool,fork) where

import Pure (Pure(..))
import Pure.Elm.Has
import qualified Pure.Elm
import Pure.Elm.Component hiding (using,shift,state,Left,Right,Shift,modify,ask,get,put)

import Control.Concurrent (MVar,newEmptyMVar,putMVar,takeMVar)
import Data.Kind
import Data.Typeable
import Data.Unique (Unique,newUnique,hashUnique)

import System.IO.Unsafe

import Prelude hiding (Read)

{-# INLINE fold #-}
fold :: (Typeable msg, Typeable a) => (Elm msg => msg -> a -> a) -> (Elm msg => a) -> ((Has a, Elm msg) => View) -> View
fold step initial v = foldM (\msg a -> pure (step msg a)) (pure (initial,pure ())) v

{-# INLINE [1] foldM #-}
foldM :: (Typeable msg, Typeable a) => (Elm msg => msg -> a -> IO a) -> (Elm msg => IO (a,IO ())) -> ((Has a, Elm msg) => View) -> View
foldM step initial v = run (Fold step initial v)

data Fold msg a = Fold (Elm msg => msg -> a -> IO a) (Elm msg => IO (a,IO ())) ((Has a, Elm msg) => View)

instance (Typeable msg, Typeable a) => Component (Fold msg a) where
  data Model (Fold msg a) = Model (a,IO())

  initialize :: Elm (Msg (Fold msg a)) => Fold msg a -> IO (Model (Fold msg a))
  initialize (Fold _ initial _) = Model <$> Pure.Elm.map (Message :: msg -> Msg (Fold msg a)) initial

  data Msg (Fold msg a) = Message msg | Shutdown

  shutdown = [Shutdown]

  upon :: Msg (Fold msg a) -> Update (Fold msg a)
  upon (Message msg) (Fold step _ _) (Model (a,s)) = do
    a' <- Pure.Elm.map (Message :: msg -> Msg (Fold msg a)) step msg a
    pure (Model (a',s))
  upon Shutdown _ mdl@(Model (_,shutdown)) = do
    shutdown 
    pure mdl

  view :: Fold msg a -> Render (Fold msg a)
  view (Fold _ _ v) (Model (a,_)) = using a (Pure.Elm.map (Message :: msg -> Msg (Fold msg a)) v)

data Void

data Fork = Fork Unique View | Return Unique

type Pool = Elm Fork

join :: Pool => Unique -> IO ()
join = command . Return

pool :: ([(Int,View)] -> View) -> (Pool => View) -> View
pool f v = fold update [((-1),v)] (f it)
  where
    update (Fork u v) ts = ((hashUnique u,v):ts)
    update (Return u) ((t,_):ts)
      | hashUnique u == t = ts
      | otherwise = update (Return u) ts 
    update _ _ = 
      error "Invariant broken: thread not found"

data Yield a = Yield a
type Producing a = Elm (Yield a)
yield :: Producing a => a -> IO ()
yield = command . Yield

fork :: forall a. (Typeable a, Pool) => (Producing a => View) -> IO a
fork producer = do
  mv <- newEmptyMVar
  u <- newUnique
  command (Fork u (call @a producer (consumer mv u)))
  takeMVar mv
  where
    consumer :: Consuming a => MVar a -> Unique -> View
    consumer mv u = 
      let 
        send = unsafePerformIO $ do
          putMVar mv await 
          command (Return u)
      in
        send `seq` Null

data Await a = Await a
type Consuming a = Has (Await a)
await :: Consuming a => a
await = let Await a = it in a

call :: forall a. Typeable a => (Producing a => View) -> (Consuming a => View) -> View
call v f = fold (\(Yield (a :: a)) _ -> using (Await a) f) v it

data Shift = Shift View

type Cont = Elm Shift

shift :: Cont => View -> IO () 
shift v = command (Shift v)

reset :: (Cont => View) -> View
reset v = fold (\(Shift v) (b,_) -> (Prelude.not b,v)) (False,v) $ do
  let (b,u) = it
  if b 
    then Tagged @True u
    else Tagged @False u

data Read a = Read a

type Reader a = Has (Read a)

ask :: Reader a => a
ask = let Read a = it in a

reader :: Typeable a => a -> (Reader a => View) -> View
reader a v = fold (\(_ :: Void) x -> x) (Read a) v

data Entry a = Entry a

data Log a = Log a

type Writer a = (Elm (Entry a),Has (Log a))

tell :: Writer a => a -> IO ()
tell = command . Entry

listen :: Writer a => a
listen = let Log a = it in a

writer :: forall a. Typeable a => Monoid a => (Writer a => View) -> View
writer = fold (\(Entry a) (Log l) -> Log (a <> l)) (Log @a mempty)

data Modify a = Modify (a -> a)

data Get a = Get a

type State a = (Elm (Modify a),Has (Get a))

modify :: State a => (a -> a) -> IO ()
modify = command . Modify

put :: State a => a -> IO ()
put = modify . const

get :: State a => a
get = let Get a = it in a

state :: forall a. Typeable a => a -> (State a => View) -> View
state initial v = fold (\(Modify f) (Get a) -> Get (f a)) (Get initial) v

data Throw a = Throw a

type Error a = Elm (Throw a)

throw :: Error a => a -> IO ()
throw = command . Throw

catch :: forall a. Typeable a => (Error a => View) -> (a -> View) -> View
catch action recovery = fold (\(Throw a) _ -> recovery a) action it

