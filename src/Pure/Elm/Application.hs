{-# LANGUAGE ImplicitParams, RankNTypes, BangPatterns, RecordWildCards, ScopedTypeVariables, CPP #-}
module Pure.Elm.Application where

import Pure.Elm hiding (run)
import qualified Pure.Elm

import Pure.Router

import Data.Typeable

data Application settings session route = Application
  { config   :: settings
  , initial  :: session
  , pages    :: Elm (Command session settings route) => route -> settings -> session -> View
  , startup  :: settings -> session -> route -> IO ()
  , shutdown :: settings -> session -> route -> IO ()
  }

class Routes rt where
  home   :: rt
  title  :: rt -> Txt
  routes :: forall x. Routing rt x

data Command session settings route 
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

run :: forall settings session route. 
       (Typeable settings, Typeable session, Typeable route, Routes route) 
    => Application settings session route -> IO ()
run Application {..} = inject body (Div <||> [ router, Pure.Elm.run app config ])
  where
    router = View (Router (home :: route) (route routes))
    app :: App settings (route,settings,session) (Command session settings route)
    app = App [Startup] [Settings] [Shutdown] (home :: route,config,initial) update view
      where
        update Startup      _ st@(route,settings,session) = do
          onRoute' (command . Route)
          startup settings session route
          pure st
        update Shutdown     _ st@(route,settings,session) = shutdown settings session route >> pure st
        update Settings settings (route,_,session)        = pure (route,settings,session)
        update (Update f)   _ (route,settings,session)    = pure (route,settings,f session)
        update (Route route) _ (_,settings,session)       = do
          setTitle (title route)
          pure (route,settings,session)

        view :: Elm (Command session settings route) => settings -> (route,settings,session) -> View
        view _ (route,settings,session) = pages route settings session 

update :: Elm (Command session settings route) => (session -> session) -> IO ()
update = command . Update

settings :: (?settings :: settings) => settings
settings = ?settings

session :: (?session :: session) => session
session = ?session

type Page session settings route = Elm (Command session settings route) => session -> settings -> View

page :: (Elm (Command session settings route) => ((?session :: session, ?settings :: settings) => View)) -> Page session settings route
page f = \ses set -> 
  let 
    ?session  = ses
    ?settings = set
  in
    f

