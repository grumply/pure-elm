{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards #-}
module Pure.Elm (Elm,command,App(..),run,Routed(..),routed,module Export) where

import Pure as Export hiding (Home,update,view)
import qualified Pure (view)
import Pure.Router as Export hiding (router)

import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

type Elm msg = (?command :: msg -> IO ())

command :: Elm msg => msg -> IO ()
command msg = ?command msg

data App st msg = App st (st -> msg -> IO st) (Elm msg => st -> View)

run :: (Typeable st, Typeable msg) => App st msg -> View
run (App model update view) = 
  flip Component () $ \self ->
    let
      ?command = fix $ \f -> \msg -> modifyM_ self $ \_ st -> do
                     st' <- let ?command = f in update st msg
                     pure (st',pure ())  
    in      
      def { construct = return model
          , render    = \_ -> view
          }

instance (Typeable st, Typeable msg) => Pure (App st msg) where
  view = run

data Routed st msg = Routed (Routing msg ()) (App st msg)

routed :: (Typeable st, Typeable msg) => Routed st msg -> View
routed (Routed rtr (App model update view)) = flip Component () $ \self ->
  let 
    router = fmap Just . route rtr
    go mm = modify_ self $ \_ _ -> mm
  in
    def { construct = return Nothing 
        , mounted   = void $ onRoute' go
        , render    = \_ mmsg -> 
          Div <||>
            [ Component (\self ->
              let
                ?command = fix $ \f -> \msg -> modifyM_ self $ \_ mdl -> do
                               mdl' <- let ?command = f in update mdl msg
                               pure (mdl',pure ())  
              in      
                def { construct = return model
                    , receive   = \mmsg mdl -> 
                      case mmsg of
                        Nothing  -> pure mdl
                        Just msg -> ?command msg >> pure mdl
                    , render    = \_ -> view
                    }) mmsg
            , View (Router Nothing router)
            ]
        }

instance (Typeable st, Typeable msg) => Pure (Routed st msg) where
  view = routed
