{-# language TypeFamilies, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, RankNTypes, PartialTypeSignatures, InstanceSigs, AllowAmbiguousTypes, ConstraintKinds, DataKinds, GADTs, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, PatternSynonyms, LambdaCase #-}
module Pure.Elm.Fold (fold,foldM,module Pure.Elm.Component,module Pure.Elm.Has,Reader,ask,reader,local,Writer,Listener,tell,listen,writer,translate,State,Modify,modify,put,get,state,state',stateIO,stateIO',stateIOWith,stateIOWith',zoom,async,Error,throw,catch,pass,Producer,yield,(#),stream,call,(<<-),(->>),exec,constant,worker,every,every',supply) where

import Pure (Pure(..))
import Pure.Elm.Has
import qualified Pure.Elm
import Pure.Elm.Component hiding (using,shift,state,Left,Right,Shift,modify,ask,get,put,self,zoom,translate,(#),static,Static,Start)

import Control.Concurrent (MVar,newEmptyMVar,putMVar,takeMVar,forkIO,killThread,ThreadId)
import qualified Control.Exception as E (catch,mask,AsyncException(ThreadKilled))
import Control.Monad (forever)
import Data.IORef
import Data.Kind
import Data.Typeable
import Data.Unique (Unique,newUnique,hashUnique)
import Prelude hiding (Read,(.),id)

import System.IO.Unsafe
import Unsafe.Coerce

import Debug.Trace

{-# INLINE fold #-}
fold :: (Typeable eff, Typeable a) => (Effect eff => eff -> a -> a) -> (Effect eff => a) -> ((Reader a, Effect eff) => View) -> View
fold step initial v = foldM (\eff a -> pure (step eff a)) (pure (initial,\_ -> pure ())) v

{-# INLINE foldM #-}
foldM :: forall eff a. (Typeable eff, Typeable a) => (Effect eff => eff -> a -> IO a) -> (Effect eff => IO (a,a -> IO ())) -> ((Reader a, Effect eff) => View) -> View
foldM step initial v = run (Fold (Pure.Elm.map (Eff :: eff -> Msg (Fold eff a)) step) (Pure.Elm.map (Eff :: eff -> Msg (Fold eff a)) initial) (\a -> using a (Pure.Elm.map (Eff :: eff -> Msg (Fold eff a)) v)))

data Fold eff a = Fold (Effect (Msg (Fold eff a)) => eff -> a -> IO a) (Effect (Msg (Fold eff a)) => IO (a,a -> IO ())) (Effect (Msg (Fold eff a)) => a -> View)

instance (Typeable eff, Typeable a) => Component (Fold eff a) where
  data Model (Fold eff a) = Model a (a -> IO())

  {-# INLINE initialize #-}
  initialize (Fold _ initial _) = do
    (a,stop) <- initial
    pure (Model a stop)

  data Msg (Fold eff a) = Eff eff | Shutdown

  shutdown = [Shutdown]

  {-# INLINE upon #-}
  upon (Eff eff) (Fold step _ _) (Model a s) = do
    a' <- step eff a
    pure (Model a' s)
  upon Shutdown _ mdl@(Model a shutdown) = do
    shutdown a
    pure mdl

  {-# INLINE view #-}
  view (Fold _ _ v) (Model a _) = v a

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

{-# INLINE constant #-}
constant :: View -> View
constant v = fold (\() v -> v) v it

type Producer a = Effect a

{-# INLINE yield #-}
yield :: Producer a => a -> IO ()
yield = effect'

{-# INLINE (#) #-}
infixr 0 #
(#) :: (a -> b) -> (Producer a => x) -> (Producer b => x)
(#) = Pure.Elm.map

{-# INLINE stream #-}
stream :: Typeable a => (Producer a => View) -> (a -> IO ()) -> View
stream v f = foldM (\a _ -> f a) def v

{-# INLINE call #-}
call :: forall a. Typeable a => (Producer a => View) -> (a -> View) -> View
call v f = eager (\(WithDict v) -> fold step initial (maybe v f it)) (WithDict @(Producer a) v) 
  where
    initial :: Maybe a
    initial = Nothing

    step :: a -> Maybe a -> Maybe a
    step a _ = Just a

infixr 0 <<-
{-# INLINE (<<-) #-}
(<<-) :: Typeable a => (a -> View) -> (Producer a => View) -> View
(<<-) f v = call v f

{-# INLINE (->>) #-}
infixl 1 ->>
(->>) :: Typeable a => (Producer a => View) -> (a -> View) -> View
(->>) v f = call v f

exec :: IO a -> (Producer a => View)
exec f = let x = unsafePerformIO (f >>= yield) in x `seq` Null

type Reader a = Has a

{-# INLINE ask #-}
ask :: Reader a => a
ask = it

{-# INLINE reader #-}
reader :: a -> (Reader a => x) -> x
reader = using

{-# INLINE local #-}
local :: (a -> b) -> (Reader b => x) -> (Reader a => x)
local f x = reader (f ask) x

type Writer a = (Monoid a, Effect a, Reader a)

{-# INLINE tell #-}
tell :: Writer a => a -> IO ()
tell = effect'

listen :: Writer a => a
listen = it

{-# INLINE writer #-}
writer :: forall a. Typeable a => Monoid a => (Writer a => View) -> View
writer = fold (<>) (mempty @a)

{-# INLINE translate #-}
translate :: (Monoid a, Monoid b) => (a -> b) -> (b -> a) -> (Writer a => x) -> (Writer b => x)
translate f g x = using (g listen) (Pure.Elm.map f x)

type Modify a = Effect (a -> a)
type State a = (Modify a, Reader a)

{-# INLINE state #-}
state :: Typeable a => (Modify a => a) -> (State a => View) -> View
state = fold ($)

{-# INLINE state' #-}
state' :: forall a. Typeable a => (Modify a => a) -> (State a => View) -> View
state' a v = eager (\(WithDict a) -> state a v) (WithDict @(Modify a) a)

{-# INLINE stateIO #-}
stateIO :: Typeable a => (Modify a => IO a) -> (State a => View) -> View
stateIO io = stateIOWith (io >>= \a -> pure (a,\_ -> pure ()))

{-# INLINE stateIO' #-}
stateIO' :: forall a. Typeable a => (Modify a => IO a) -> (State a => View) -> View
stateIO' io v = eager (\(WithDict io) -> stateIO io v) (WithDict @(Modify a) io)

{-# INLINE stateIOWith #-}
stateIOWith :: Typeable a => (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateIOWith = foldM (\f a -> pure (f a)) 

{-# INLINE stateIOWith' #-}
stateIOWith' :: forall a. Typeable a => (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateIOWith' io v = eager (\(WithDict io) -> stateIOWith io v) (WithDict @(Modify a) io)

{-# INLINE async #-}
async :: View -> View
async v = stateIO' (modify v >> pure Null) it

data WithDict c a = WithDict (c => a)

{-# INLINE modify #-}
modify :: forall a. Modify a => (Reader a => a) -> IO ()
modify f = effect' (\a -> using (a :: a) f)

{-# INLINE put #-}
put :: Modify a => a -> IO ()
put = modify

{-# INLINE get #-}
get :: Reader a => a
get = it

{-# INLINE zoom #-}
zoom :: forall a b x. Typeable a => (a -> b) -> (b -> a -> a) -> (State b => x) -> (State a => x)
zoom f g v = using (f get) (Pure.Elm.map (\h a -> g (h (f a)) a) v)

type Error e = Effect e

{-# INLINE throw #-}
throw :: Error e => e -> IO ()
throw = effect'

{-# INLINE catch #-}
catch :: forall e. Typeable e => (Error e => View) -> (e -> IO ()) -> View
catch = stream 

{-# INLINE pass #-}
pass :: (a -> b) -> (Error a => x) -> (Error b => x)
pass = Pure.Elm.map

{-# INLINE worker #-}
worker :: IO () -> View -> View
worker action = stateIO (forkIO action >>= \tid -> pure (tid,killThread))

{-# INLINE every #-}
every :: Time -> IO () -> IO ()
every t io = forever (io >> delay t)

{-# INLINE every' #-}
every' :: Time -> IO () -> IO ()
every' t io = forever (delay t >> io)

data Supply a = Supply Time (IO a) (Reader a => View)
instance Typeable a => Component (Supply a) where
  data Model (Supply a) = Unstarted | Started ThreadId | Supplied ThreadId a

  model = Unstarted

  data Msg (Supply a) = Start | Stop | Receive | Update a

  startup = [Start]
  shutdown = [Stop]
  receive = [Receive]

  upon Start (Supply t io _) = \_ -> do
    tid <- forkIO (forever (io >>= \a -> command (Update a) >> delay t))
    pure (Started tid)

  upon Stop _ = \case
    Supplied tid _ -> killThread tid >> pure Unstarted
    Started  tid   -> killThread tid >> pure Unstarted
    x              -> pure x       

  upon (Update a) _ = \case
    Started  tid   -> pure (Supplied tid a)
    Supplied tid _ -> pure (Supplied tid a)
    x              -> pure x       

  upon Receive (Supply t io _) = \case
    Started tid -> do
      killThread tid
      tid <- forkIO (forever (io >>= \a -> command (Update a) >> delay t))
      pure (Started tid)
    Supplied tid a -> do
      killThread tid
      tid <- forkIO (forever (io >>= \a -> command (Update a) >> delay t))
      pure (Supplied tid a)
    _ -> do
      tid <- forkIO (forever (io >>= \a -> command (Update a) >> delay t))
      pure (Started tid)

  upon _ _ = pure 

  view (Supply _ _ v) (Supplied _ a) = using a v
  view _ _ = Null

{-# INLINE supply #-}
supply :: Typeable a => Time -> IO a -> (Reader a => View) -> View
supply t io v = run (Supply t io v)
