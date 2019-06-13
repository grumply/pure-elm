{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards #-}
module Pure.Elm (Elm,command,run,module Export) where

import Pure as Export hiding (initial,update,view)
import qualified Pure (view)
import Pure.Router as Export hiding (router)

import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

type Elm msg = (?command :: msg -> IO ())

command :: Elm msg => msg -> IO ()
command msg = ?command msg

data App rt st msg = App
  { home    :: rt
  , initial :: Elm msg => st
  , router  :: Elm msg => Routing rt rt
  , update  :: Elm msg => rt -> st -> msg -> IO st
  , view    :: Elm msg => rt -> st -> View
  }

instance (Typeable rt, Typeable st, Typeable msg) => Pure (App rt st msg) where
  view App {..} =
    flip Component () $ \self ->
      let 
        go r = modify_ self $ \_ (_,st) -> (r,st)
      in let
        ?command = fix $ \f -> \msg -> modifyM_ self $ \_ (rt,st) -> do
                       st' <- let ?command = f in update rt st msg
                       pure ((rt,st'),pure ())
      in
        def { construct = return (home,initial)
            , mounted   = void $ onRoute' go
            , render    = \_ (rt,st) ->
              Div <||>
                [ view rt st
                , View (Router home (route router))
                ]
            }

run :: (Typeable rt, Typeable st, Typeable msg) 
    => rt
    -> (Elm msg => st)
    -> (Elm msg => Routing rt rt)
    -> (Elm msg => rt -> st -> msg -> IO st)
    -> (Elm msg => rt -> st -> View)
    -> IO ()
run home initial router update view = inject body (View App {..})
