{-# language TypeFamilies, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, RankNTypes, PartialTypeSignatures, InstanceSigs, AllowAmbiguousTypes, ConstraintKinds, DataKinds, GADTs, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms #-}
module Pure.Elm.Fold (fold,foldM,module Pure.Elm.Component,module Pure.Elm.Has,Cont,shift,reset,Reader,ask,reader,env,Writer,tell,listen,writer,translate,State,modify,put,get,state,zoom,Error,throw,catch,pass,Producer,yield,(#),Pool,Thread,pool,fork,call,exec,stack,eval) where

import Pure (Pure(..))
import Pure.Elm.Has
import qualified Pure.Elm
import Pure.Elm.Component hiding (using,shift,state,Left,Right,Shift,modify,ask,get,put,self,zoom,translate,(#))

import Control.Concurrent (MVar,newEmptyMVar,putMVar,takeMVar)
import Data.Kind
import Data.Typeable
import Data.Unique (Unique,newUnique,hashUnique)

import System.IO.Unsafe

import Control.Category
import Prelude hiding (Read,(.),id)

import Unsafe.Coerce
import System.IO.Unsafe

import Debug.Trace

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

{- I wanted this to work, but I couldn't get constraint satisfaction to happen
    during/before `do` desugaring.  That is:

    > pure () >>= \() -> "Done"

    works. But this:

    > do { () <- pure (); "Done" } 

    does not work. It complains (at runtime!) about an IsString instance, meaning
    the rhs of the bind has had its constraints ignored?

fmap :: (a -> b) -> (Producer a => View) -> (Producer b => View)
fmap = (#)

join :: (Producer View => View) -> View
join v = call v id

(>>=) :: forall a. Typeable a => (Producer a => View) -> (a -> View) -> View
(>>=) v f = join (fmap f v)

(>>) :: forall a. Typeable a => (Producer a => View) -> View -> View
(>>) v1 v2 = (>>=) v1 (\(_ :: a) -> v2)

fail :: String -> View
fail = txt

pure :: a -> (Producer a => View)
pure a = exec (pure a)

return = pure

liftA2 f va vb = call va (\a -> call vb (\b -> pure (f a b)))
-}

data Void

data Fork = Fork Unique View | Return Unique

type Pool = Elm Fork

join :: Pool => Unique -> IO ()
join = command . Return

pool :: ([(Int,View)] -> View) -> (Pool => View) -> View
pool f v = fold update [((-1),v)] (f it)
  where
    update (Fork u v) ts = trace "Fork" ((hashUnique u,v):ts)
    update (Return u) ((t,_):ts)
      | hashUnique u == t = trace "Return" ts
      | otherwise = trace "killed a thread" $ update (Return u) ts 
    update _ _ = 
      error "Invariant broken: thread not found"

data Stack
instance Theme Stack where
  theme c =
    is c $ do
      child "*:not(:first-child)" $ do
        display =: none

stack :: (Pool => View) -> View
stack = pool (\xs -> Keyed Div <| Themed @Stack |#> xs)

type Thread a = Producer a => View

trivial :: (Elm () => View) -> View
trivial = fold (\() () -> ()) ()

fork :: forall a. Pool => Thread a -> IO a
fork producer = do
  mv <- newEmptyMVar
  u <- newUnique
  command (Fork u (trivial $ Pure.Elm.map @(Produce a) @() (\(Produce a) -> let !_ = unsafePerformIO (command (Return u) >> putMVar mv a) in ()) producer))
  takeMVar mv

eval :: Pool => (Producer a => View) -> a
eval = unsafePerformIO . fork

data Produce a = Produce a
type Producer a = Elm (Produce a)
yield :: Producer a => a -> IO ()
yield = command . Produce

infixr 0 #
(#) :: (a -> b) -> (Producer a => x) -> (Producer b => x)
(#) f v = Pure.Elm.map (\(Produce b) -> Produce (f b)) v

call :: forall a. Typeable a => (Producer a => View) -> (a -> View) -> View
call v f = fold (\(Produce a) _ -> (f a,True)) (v,False) 
  -- prevent sharing by forcing a full build with Tagged
  (let (v,b) = it in if b then Tagged @True v else Tagged @False v)

exec :: IO a -> (Producer a => View)
exec f = foldM (\() _ -> f >>= yield >> pure ()) (command () >> pure ((),pure ())) Null

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

env :: Typeable a => (a -> b) -> (Reader b => x) -> (Reader a => x)
env f v = let Read a = it in using (Read (f a)) v

data Entry a = Entry a

data Log a = Log a

type Writer a = (Elm (Entry a),Has (Log a))

tell :: Writer a => a -> IO ()
tell = command . Entry

listen :: Writer a => a
listen = let Log a = it in a

writer :: forall a. Typeable a => Monoid a => (Writer a => View) -> View
writer = fold (\(Entry a) (Log l) -> Log (a <> l)) (Log @a mempty)

translate :: Monoid a => (a -> b) -> (a -> b -> a) -> (Writer b => x) -> (Writer a => x)
translate getter setter v = Pure.Elm.map (\(Entry b) -> Entry (setter mempty b)) (let Log a = it in using (Log (getter a)) v)

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

zoom :: Typeable a => (a -> b) -> (a -> b -> a) -> (State b => x) -> (State a => x)
zoom getter setter v = Pure.Elm.map (\(Modify f) -> Modify (\a -> setter a (f (getter a)))) (let Get a = it in using (Get (getter a)) v)

data Throw a = Throw a

type Error a = Elm (Throw a)

throw :: Error a => a -> IO ()
throw = command . Throw

catch :: forall a. Typeable a => (Error a => View) -> (a -> View) -> View
catch action recovery = fold (\(Throw a) _ -> recovery a) action it

pass :: (a -> b) -> (Error a => x) -> (Error b => x)
pass transform v = Pure.Elm.map (\(Throw a) -> Throw (transform a)) v

