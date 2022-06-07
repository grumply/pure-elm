{-# LANGUAGE 
     ImplicitParams, RankNTypes, BangPatterns, RecordWildCards, 
     ScopedTypeVariables, CPP, ConstraintKinds, OverloadedStrings, 
     AllowAmbiguousTypes, TypeApplications, LambdaCase, PatternSynonyms,
     FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
     LambdaCase, TypeFamilies, UndecidableInstances, FlexibleContexts
   #-}
module Pure.Elm.Application 
  ( Application(..)
  , Update
  , Render
  , Handler
  , command
  , retitle
  , reroute
  , message
  , link
  , withScrollPositionFromHistory
  , restoreScrollPosition
  , restoreScrollPositionSmooth
  , storeScrollPosition
  , processLinks
  , LinkSettings(..)
  , processLinksWith
  , setScrollRestoration
  , ScrollRestoration(..)
  , setManualScrollRestoration
  , URL(..)
  , url
  , internal
  , external
  , module Export
  , module Pure.Elm.Sub
  , pattern Applet
  ) where

import qualified Pure (Pure(..))
import Pure as Export hiding (Home,update,view,url,link,initialize,receive)
import Pure.Data.Txt as Txt (uncons,null,isPrefixOf)
import Pure.Data.URI (encodeURI,decodeURI)
import Pure.Elm hiding (App,url,link,initialize,receive)
import qualified Pure.Elm
import qualified Pure.Elm.Sub hiding (map)
import Pure.Router as Export hiding (route)
import qualified Pure.Router

import qualified Data.Map as Map

import Control.Applicative
import Control.Monad (foldM,void,when)
import Data.Foldable (for_)
import Data.Typeable
import Data.Unique

import Data.Typeable (Typeable, typeOf)

import GHC.Stack (HasCallStack)

import Pure.Elm.Application.Route
import Pure.Elm.Application.Scroll

type Update  a = (HasCallStack, Application a, Elm (Msg a)) => Route a -> a -> Model a -> IO (Model a)
type Render  a = (HasCallStack, Application a, Elm (Msg a)) => Model a -> View
type Handler a = (HasCallStack, Application a, Elm (Msg a)) => Evt -> IO ()

data AppMsg app
  = Startup
  | Message (Msg app)
  | Route (Route app)
  | Routed (Route app)
  | Retitle Txt

data AppModel app = AppModel
  { appModel :: Model app
  , appRoute :: Route app
  }

class Typeable app => Application app where
  data Model app
  data Route app
  data Msg app

  startup :: [Msg app]
  startup = []

  receive :: [Msg app]
  receive = []

  shutdown :: [Msg app]
  shutdown = []

  -- | A required fallback, or home, route.
  home :: Route app

  -- | An optional title, by route. If `Nothing` is returned, the routing 
  -- machinery will not touch the title when the route changes!  This means 
  -- any existing title will remain and must be overridden with `retitle`. 
  --
  -- The default implementation is the obvious:
  --
  -- > const Nothing
  --
  title  :: Route app -> Maybe Txt
  title = const Nothing

  -- | A conversion method from the route type to an internal or external URL.
  --
  -- The default implementation is the obvious:
  --
  -- > const ""
  --
  location :: Route app -> Txt
  location = const ""

  -- | A route parser using the `Routing` monad provided by `pure-router`. The
  -- default implementation is:
  --
  -- > dispatch home
  --
  routes :: forall x. Pure.Router.Routing (Route app) x
  routes = Pure.Router.dispatch home

  model :: Model app
  model = error ("No default model defined for " ++ show (typeOf (undefined :: app)))

  -- GHC doesn't let me do this, but it would be nice if GHC would emit this warning for any overriding implementation.
  -- {-# WARNING initialize "Application.initialize is a render-blocking initialization method! Consider moving long-running and long-tail effects to a startup event." #-}
  initialize :: app -> IO (Model app)
  initialize _ = pure model

  upon :: Msg app -> Update app
  upon _msg _route _app model = pure model

  route :: Route app -> Update app
  route _new _old _app model = pure model

  view :: Route app -> app -> Render app
  view _ _ _ = Null

  {-# INLINE app #-}
  app :: app -> Pure.Elm.App app (AppModel app) (AppMsg app)
  app a = Applet (Startup : fmap Message startup) (fmap Message receive) (fmap Message shutdown) model' upon' view'
    where
      model' = AppModel <$> initialize a <*> pure home

      upon' :: Elm (AppMsg app) => AppMsg app -> app -> AppModel app -> IO (AppModel app)
      upon' = \case
        Startup -> \app mdl -> do
          subscribe @(AppMsg app)
          setManualScrollRestoration
          Pure.Router.onRoute' (command . Routed @app)
          pure mdl

        Message m -> \app mdl -> do
          mdl' <- Pure.Elm.map @(Msg app) @(AppMsg app) Message (upon m (appRoute mdl) app (appModel mdl))
          pure mdl
            { appModel = mdl' }

        Route r -> \app mdl -> do
          storeScrollPosition
          Pure.Router.goto (encodeURI (location r))
          pure mdl

        Routed r -> \app mdl -> do
          mdl' <- Pure.Elm.map @(Msg app) @(AppMsg app) Message (route r (appRoute mdl) app (appModel mdl))
          for_ (title r) setTitle
          pure mdl
            { appRoute = r
            , appModel = mdl'
            }

        Retitle t -> \app mdl -> do
          setTitle t
          pure mdl  
        
      view' :: Elm (AppMsg app) => app -> AppModel app -> View
      view' app mdl =
        Div <||>
          [ View (Pure.Router.Router (home @app) (Pure.Router.route routes))
          , Pure.Elm.map @(Msg app) @(AppMsg app) Message (view (appRoute mdl) app (appModel mdl))
          ]

  {-# INLINE execute #-}
  execute :: app -> View
  execute a = run (app a) a

  {-# MINIMAL home #-}

-- | Create an internal link to an application route.
--
-- NOTE: Be sure that: 
--
-- > forall rt. route routes (location rt) == Just rt
--
link :: (Typeable app, Application app, HasFeatures a) => Route app -> a -> a
link rt a = OnClickWith intercept (\_ -> Pure.Elm.Sub.publish (Route rt)) (Href (encodeURI (location rt)) a)

-- | Command the application to retitle the page, from anywhere, without 
-- requiring an `Elm (Msg app)` constraint.
retitle :: forall app. Typeable app => Txt -> IO ()
retitle = Pure.Elm.Sub.publish . Retitle @app

-- | Command the application, from anywhere, without requiring an 
-- `Elm (Msg app)` constraint.
message :: Typeable app => Msg app -> IO ()
message = Pure.Elm.Sub.publish . Message

-- | Command the application to manually route. This should be equivalent to
-- clicking on a link constructed with the `link` decorator, and will do the
-- necessary HTML5 History modifications. Thus, it should be equivalent to:
--
-- > url goto setLocation (location rt)
--
-- where `setLocation` is equivalent to the javascript:
-- 
-- > window.location.href = _
--
{-# INLINE reroute #-}
reroute :: Typeable app => Route app -> IO ()
reroute = Pure.Elm.Sub.publish . Route

-- | Calling `processLinks` will turn any `href` values in the `View` into click
-- event interceptions that inject the route into the browser's history. 
-- External -- links are not injected in this fashion. FOr saafety, this 
-- implementation -- defaults to tagging external links with `rel="noopener"` 
-- and `target="blank"`.
--
-- Note: Since external links are not intercepted, local, fully-scoped URLs 
-- are, also, not injected. This can be an important point when interpreting 
-- views that are dynamically generated at runtime, like those produced from 
-- markdown. If the markdown contains local, internal links that have a non-
-- relative path within the application, they will cause the application to 
-- fully reload when clicked!
{-# INLINE processLinks #-}
processLinks :: View -> View
processLinks = processLinksWith (LinkSettings True True Nothing [])

