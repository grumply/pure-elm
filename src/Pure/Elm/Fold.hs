{-# language TypeFamilies, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, RankNTypes, PartialTypeSignatures, InstanceSigs, AllowAmbiguousTypes, ConstraintKinds, DataKinds #-}
module Pure.Elm.Fold (fold,foldM,Producer,yield,Consumer,await,call,module Pure.Elm.Component,module Pure.Elm.Has,call,Cont,shift,reset) where

import Pure (Pure(..))
import Pure.Elm.Has
import qualified Pure.Elm
import Pure.Elm.Component hiding (using,shift,state,Left,Right,Shift)
import Data.Kind
import Data.Typeable

import System.IO.Unsafe

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


newtype Yield a = Yield a
yield :: Elm (Yield a) => a -> IO ()
yield = command . Yield

newtype Await a = Await a
await :: Has (Await a) => a
await = let Await a = it in a

type Producer a = Elm (Yield a) => View
type Consumer a = Has (Await a) => View

call :: forall a. Typeable a => Producer a -> Consumer a -> View
call v f = fold (\(Yield (a :: a)) _ -> using (Await a) f) v it

newtype Shift = Shift View

type Cont = Elm Shift

shift :: Cont => View -> IO () 
shift v = command (Shift v)

reset :: (Cont => View) -> View
reset v = fold (\(Shift v) (b,_) -> (Prelude.not b,v)) (False,v) $ do
  let (b,u) = it
  if b 
    then Tagged @True u
    else Tagged @False u
    
