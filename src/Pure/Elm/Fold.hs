{-# language TypeFamilies, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, RankNTypes, PartialTypeSignatures, InstanceSigs, AllowAmbiguousTypes #-}
module Pure.Elm.Fold (fold,foldM,module Pure.Elm.Component,module Pure.Elm.Has) where

import Pure.Elm.Has
import qualified Pure.Elm
import Pure.Elm.Component hiding (using,Left,Right)
import Data.Typeable

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

{-# INLINE CONLIKE combine #-}
combine :: forall msg_a msg_b a b. Fold msg_a a -> ((Has a, Elm msg_a) => Fold msg_b b) -> Fold (Either msg_a msg_b) (a,b)
combine (Fold on_msg_a initialize_a _) f = Fold on_msg_a_b initialize_a_b view_a_b
    where
      on_msg_a_b :: Elm (Either msg_a msg_b) => Either msg_a msg_b -> (a,b) -> IO (a,b)
      on_msg_a_b (Left msg_a) (a,b) = do
        a' <- Pure.Elm.map (Left :: msg_a -> Either msg_a msg_b) on_msg_a msg_a a
        pure (a',b)
      on_msg_a_b (Right msg_b) (a,b) = do
        let Fold on_msg_b _ _ = using a (Pure.Elm.map (Left :: msg_a -> Either msg_a msg_b) f)
        b' <- Pure.Elm.map (Right :: msg_b -> Either msg_a msg_b) on_msg_b msg_b b
        pure (a,b')

      initialize_a_b :: Elm (Either msg_a msg_b) => IO ((a,b),IO ())
      initialize_a_b = do
        (a,after_a) <- Pure.Elm.map (Left :: msg_a -> Either msg_a msg_b) initialize_a
        let Fold _ initialize_b _ = using a (Pure.Elm.map (Left :: msg_a -> Either msg_a msg_b) f)
        (b,after_b) <- Pure.Elm.map (Right :: msg_b -> Either msg_a msg_b) initialize_b
        pure ((a,b),after_a >> after_b)
      
      view_a_b :: (Has (a,b), Elm (Either msg_a msg_b)) => View 
      view_a_b =
        let (a,b) = it @(a,b)
            Fold _ _ view_b = using a (Pure.Elm.map (Left :: msg_a -> Either msg_a msg_b) f)
         in using b (Pure.Elm.map (Right :: msg_b -> Either msg_a msg_b) view_b)

-- This isn't generally possible since we can't pattern match on lambdas, but
-- since we can abstract over arguments with `Has`, we can hide the lambda in
-- the type system! Neat, if it works!
{-# RULES
   "foldM/foldM" forall step_a initialize_a step_b initialize_b view_b.
     foldM step_a initialize_a (foldM step_b initialize_b view_b) =
       let Fold step_a_b initialize_a_b view_a_b = combine (Fold step_a initialize_a Null) (Fold step_b initialize_b view_b)
        in foldM step_a_b initialize_a_b view_a_b
   #-}
