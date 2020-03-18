{-# LANGUAGE ImplicitParams, RankNTypes, BangPatterns, RecordWildCards, ScopedTypeVariables, CPP #-}
module Pure.Elm.Application where

import Pure.Elm hiding (run)
import qualified Pure.Elm

import Pure.Router

import Data.Typeable

data Application settings session route = Application
  { config   :: settings
  , initial  :: session
  , pages    :: Elm (Command settings session route) => route -> settings -> session -> View
  , startup  :: route -> settings -> session -> IO ()
  , shutdown :: route -> settings -> session -> IO ()
  , routing  :: route -> route -> settings -> session -> IO session
  }

class Routes rt where
  home   :: rt
  title  :: rt -> Txt
  routes :: forall x. Routing rt x

data Command settings session route 
  = Startup
  | Route route
  | Settings 
  | Update (session -> session)
  | Shutdown

#ifdef __GHCJS__
foreign import javascript unsafe
  "if (document.title != $1) { document.title = $1; }" set_title_js :: Txt -> IO ()

foreign import javascript unsafe
  "$('meta[name=\"description\"]').attr('content', $1)" set_description_js :: Txt -> IO ()

foreign import javascript unsafe
  "window.scrollTo({ top: 0 })" scroll_top_js :: IO ()

#endif

setTitle :: Txt -> IO ()
setTitle t =
#ifdef __GHCJS__
  set_title_js t
#else
  pure ()
#endif

setDescription :: Txt -> IO ()
setDescription d =
#ifdef __GHCJS__
  set_description_js d
#else
  pure ()
#endif

scrollTop :: IO ()
scrollTop =
#ifdef __GHCJS__
  scroll_top_js
#else
  pure ()
#endif

run :: forall settings session route. 
       (Typeable settings, Typeable session, Typeable route, Routes route) 
    => Application settings session route -> IO ()
run Application {..} = inject body (Div <||> [ router, Pure.Elm.run app config ])
  where
    router = View (Router (home :: route) (route routes))
    app :: App settings (route,settings,session) (Command settings session route)
    app = App [Startup] [Settings] [Shutdown] (home :: route,config,initial) update view
      where
        update Startup      _ st@(route,settings,session) = do
          onRoute' (command . Route)
          startup route settings session 
          pure st
        update Shutdown     _ st@(route,settings,session) = shutdown route settings session >> pure st
        update Settings settings (route,_,session)        = pure (route,settings,session)
        update (Update f)   _ (route,settings,session)    = pure (route,settings,f session)
        update (Route route) _ (old,settings,session)       = do
          scrollTop
          setTitle (title route)
          session' <- routing old route settings session
          pure (route,settings,session')

        view :: Elm (Command settings session route) => settings -> (route,settings,session) -> View
        view _ (route,settings,session) = pages route settings session 

update :: Elm (Command settings session route) => (session -> session) -> IO ()
update = command . Update

settings :: (?settings :: settings) => settings
settings = ?settings

session :: (?session :: session) => session
session = ?session

type Page settings session route = Elm (Command settings session route) => settings -> session -> View

page :: (Elm (Command settings session route) => ((?session :: session, ?settings :: settings) => View)) -> Page settings session route
page f = \set ses -> 
  let 
    ?session  = ses
    ?settings = set
  in
    f

