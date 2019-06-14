{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards #-}
module Pure.Elm (Run(..),Elm,command,embed,App(..),Routed(..),module Export) where

import Pure as Export hiding (Home,update,view)
import qualified Pure (view)
import Pure.Router as Export hiding (router,route)
import qualified Pure.Router as Router

import Control.Monad
import Control.Monad.IO.Class as Export
import Data.Function
import Data.Typeable

class Run a where
  run :: a -> View

type Elm msg = (?command :: msg -> IO ())

command :: Elm msg => msg -> IO ()
command msg = ?command msg

embed :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
embed f a = 
  let g = ?command
   in let ?command = g . f
       in a

data App st msg = App (Elm msg => IO ()) (Elm msg => st) (Elm msg => msg -> st -> IO st) (Elm msg => st -> View)

runApp :: (Typeable st, Typeable msg) => App st msg -> View
runApp (App startup model update view) = 
  flip Component () $ \self ->
    let
      ?command = fix $ \f -> \msg -> modifyM_ self $ \_ mdl -> do
                     mdl' <- let ?command = f in update msg mdl
                     pure (mdl',pure ())
    in      
      def { construct = return model
          , executing = startup
          , render    = \_ -> view
          }

instance (Typeable st, Typeable msg) => Pure (App st msg) where
  view = runApp

instance (Typeable st, Typeable msg) => Run (App st msg) where
  run = runApp

data Routed st msg = Routed (Routing msg ()) (App st msg)

runRouted :: (Typeable st, Typeable msg) => Routed st msg -> View
runRouted (Routed rtr (App startup model update view)) = flip Component () $ \self ->
  let 
    router = fmap Just . Router.route rtr
    go mm = modify_ self $ \_ _ -> mm
  in
    def { construct = return Nothing 
        , mounted   = void $ onRoute' go
        , render    = \_ mmsg -> 
          Div <||>
            [ Component (\self ->
              let
                ?command = fix $ \f -> \msg -> modifyM_ self $ \_ mdl -> do
                               mdl' <- let ?command = f in update msg mdl
                               pure (mdl',pure ())  
              in      
                def { construct = return model
                    , executing = startup
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
  view = runRouted

instance (Typeable st, Typeable msg) => Run (Routed st msg) where
  run = runRouted
