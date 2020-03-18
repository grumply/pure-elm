{-# LANGUAGE ImplicitParams, RankNTypes, BangPatterns, RecordWildCards, ScopedTypeVariables, CPP, ConstraintKinds, OverloadedStrings, AllowAmbiguousTypes, TypeApplications #-}
module Pure.Elm.Application 
  ( App(..)
  , Command(Message)
  , Elm
  , run
  , retitle
  , command
  , reroute
  , link
  , processLinks
  , ProcessLinksSettings(..)
  , processLinksWith
  , Page
  , page
  , Settings
  , settings
  , Session
  , session
  , Routes(..)
  , subscribeWith
  , subscribe
  , subscribe'
  , publish
  , publish'
  , publishing
  , module Export
  , Pure.Elm.Sub.unsubscribe
  , Pure.Elm.Sub.unsubscribeWith
  , Pure.Elm.Sub.unsafeSubscribeWith
  ) where

import Pure as Export hiding (Home,update,view,url)
import Pure.Data.Txt as Txt (uncons)
import Pure.Elm hiding (App,Elm,run,command,url,unsafeSubscribeWith,subscribeWith,subscribe,subscribe',publish,publish',publishing)
import qualified Pure.Elm
import qualified Pure.Elm.Sub
import Pure.Router as Export
import qualified Pure.Router

import qualified Data.Map as Map

import Control.Applicative
import Control.Monad (foldM,void)
import Data.Typeable
import Data.Unique

data App env st msg rt = App
  { _startup  :: [msg]
  , _receive  :: [msg]
  , _routing  :: [rt -> msg]
  , _shutdown :: [msg]
  , _model    :: st
  , _update   :: !(Elm msg rt => rt -> msg -> env -> st -> IO st)
  , _view     :: !(Elm msg rt => rt -> env -> st -> View)
  }

class Routes rt where
  home   :: rt
  title  :: rt -> Txt
  title _ = ""
  url :: rt -> Txt
  url _ = ""
  routes :: forall x. Routing rt x
  routes = dispatch home

data Command msg rt
  = Startup
  | Route rt
  | Retitle Txt
  | Message msg

type Elm msg rt = Pure.Elm.Elm (Command msg rt)

{-# INLINE retitle #-}
retitle :: Elm msg rt => Txt -> IO ()
retitle = Pure.Elm.command . Retitle

{-# INLINE command #-}
command :: Elm msg rt => msg -> IO ()
command = Pure.Elm.command . Message

{-# INLINE reroute #-}
reroute :: Elm msg rt => rt -> IO ()
reroute = Pure.Elm.command . Route

{-# INLINE link #-}
link :: (HasFeatures a, Elm msg rt, Routes rt) => rt -> a -> a
link rt a = OnClickWith intercept (const (reroute rt)) (Href (url rt) a)

data ProcessLinksSettings = ProcessLinksSettings
  { blank :: Bool
  , noopener :: Bool
  }

{-# INLINE processLinksWith #-}
processLinksWith :: ProcessLinksSettings -> View -> View
processLinksWith pls v =
  let as = attributes (getFeatures v)
      ps = properties (getFeatures v) 
      v' = setChildren (fmap (processLinksWith pls) (getChildren v)) v
  in case Map.lookup "href" ps <|> Map.lookup "href" as of
       Just ref ->
         case Txt.uncons ref of
           Just ('/',_) -> lref ref v'
           _            -> v' <| blank_ . noopener_
       Nothing          -> v'
  where
    blank_
      | blank pls = Attribute "target" "_blank"
      | otherwise = id
    noopener_
      | noopener pls = Rel "noopener"
      | otherwise    = id

{-# INLINE processLinks #-}
processLinks :: (Elm msg rt, Routes rt) => View -> View
processLinks = processLinksWith (ProcessLinksSettings True True)

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

{-# INLINE run #-}
run :: forall env st msg rt. (Typeable env, Typeable st, Typeable msg, Typeable rt, Routes rt) 
    => App env st msg rt -> env -> IO ()
run App {..} = \config -> inject body (Div <||> [ router, Pure.Elm.run app config ])
  where
    router = View (Router (home :: rt) (Pure.Router.route routes))

    app :: Pure.Elm.App env (rt,st) (Command msg rt)
    app = Pure.Elm.App (Startup:fmap Message _startup) (fmap Message _receive) (fmap Message _shutdown) (home :: rt,_model) update view
      where
        lift = Message

        update :: Elm msg rt => Command msg rt -> env -> (rt,st) -> IO (rt,st)
        update Startup      _ st = do
          onRoute' (Pure.Elm.command . Route)
          pure st

        update (Route newRoute) env (oldRoute,st) = do
          -- Give the `routing` messages access to the scroll position, etc...
          st' <- foldM (\st m -> _update oldRoute (m newRoute) env st) st _routing
          scrollTop
          setTitle (title newRoute)
          pure (newRoute,st')

        update (Retitle t) env x = do
          setTitle t
          pure x

        update (Message msg) env (rt,st) = do
          st' <- _update rt msg env st
          pure (rt,st')

        view :: Elm msg rt => env -> (rt,st) -> View
        view env (rt,st) = _view rt env st 

type Settings settings = ?elm_application_settings :: settings

{-# INLINE settings #-}
settings :: Settings settings => settings
settings = ?elm_application_settings

type Session session = ?elm_application_session :: session

{-# INLINE session #-}
session :: Session session => session
session = ?elm_application_session

type Page env st msg rt = Elm msg rt => env -> st -> View

{-# INLINE page #-}
page :: (Elm msg rt => ((Session st,Settings env) => View)) -> Page env st msg rt
page f = \env st -> 
  let 
    ?elm_application_settings = env
    ?elm_application_session  = st
  in
    f

{-# INLINE subscribeWith #-}
subscribeWith :: forall rt msg msg'. (Typeable msg', Typeable rt, Elm msg rt) => (msg' -> msg) -> IO ()
subscribeWith f = void $ Pure.Elm.Sub.unsafeSubscribeWith (?command . Message @msg @rt . f)

{-# INLINE subscribe #-}
subscribe :: forall rt msg. (Typeable msg, Typeable rt, Elm msg rt) => IO ()
subscribe = void $ Pure.Elm.Sub.unsafeSubscribeWith (?command . Message @msg @rt)

{-# INLINE subscribe' #-}
subscribe' :: forall rt msg. (Typeable msg, Typeable rt, Elm msg rt) => IO Unique
subscribe' = Pure.Elm.Sub.unsafeSubscribeWith (?command . Message @msg @rt)

-- | `publish` requires visible type application to dispatch .
{-# INLINE publish #-}
publish :: forall rt msg. (Typeable rt, Typeable msg) => msg -> IO ()
publish = Pure.Elm.Sub.publish . Message @msg @rt

-- | `publish'` requires visible type application.
{-# INLINE publish' #-}
publish' :: forall rt msg. (Typeable rt, Typeable msg) => msg -> IO Bool
publish' = Pure.Elm.Sub.publish' . Message @msg @rt

{-# INLINE publishing #-}
publishing :: forall rt msg a. (Typeable msg, Typeable rt) => (Elm msg rt => a) -> a
publishing a = let ?command = Pure.Elm.Sub.publish' in a
