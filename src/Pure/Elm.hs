{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards #-}
module Pure.Elm (App(..),Elm,command,module Export,goto,lref) where

import Pure as Export hiding (initial,update,view)
import qualified Pure (view)
import Pure.Router hiding (router)

import Control.Monad
import Data.Function
import Data.Typeable

type Elm msg = (?command :: msg -> IO ())

command :: Elm msg => msg -> IO ()
command msg = ?command msg

data App rt st msg = App
  { initial :: Elm msg => st
  , router  :: Elm msg => Router rt
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
        def { construct = return (initialRoute router,initial)
            , mounted   = void $ onRoute' go
            , render    = \_ (rt,st) ->
              Div <||>
                [ view rt st
                , View router
                ]
            }
