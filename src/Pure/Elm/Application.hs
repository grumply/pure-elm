{-# LANGUAGE 
     ImplicitParams, RankNTypes, BangPatterns, RecordWildCards, 
     ScopedTypeVariables, CPP, ConstraintKinds, OverloadedStrings, 
     AllowAmbiguousTypes, TypeApplications, LambdaCase 
   #-}
module Pure.Elm.Application 
  ( App(..)
  , Command(Message)
  , Elm
  , Application
  , run
  , retitle
  , describe
  , command
  , reroute
  , link
  , withScrollPositionFromHistory
  , restoreScrollPosition
  , restoreScrollPositionSmooth
  , storeScrollPosition
  , processLinks
  , LinkSettings(..)
  , processLinksWith
  , Page
  , page
  , Settings
  , settings
  , Session
  , session
  , URL(..)
  , url
  , internal
  , external
  , Routes(..)
  , subscribeWith
  , subscribe
  , publishing
  , module Export
  , Pure.Elm.Sub.publish
  , Pure.Elm.Sub.publish'
  , Pure.Elm.Sub.unsubscribe
  ) where

import Pure as Export hiding (Home,update,view,url)
import Pure.Data.Txt as Txt (uncons,null)
import Pure.Elm hiding (App,Elm,run,command,url,unsafeSubscribeWith,subscribeWith,subscribe,subscribe',publish,publish',publishing)
import qualified Pure.Elm
import qualified Pure.Elm.Sub
import Pure.Router as Export
import qualified Pure.Router

import qualified Data.Map as Map

import Control.Applicative
import Control.Monad (foldM,void,when)
import Data.Foldable (for_)
import Data.Typeable
import Data.Unique

-- There was a decision in this module to resolve which type parameterizes the 
-- route type: the message type, or the context constraint. It was chosen that
-- the context would constrain the route and message types separately, since it
-- simplifies the implementation of the message type and allows for fully
-- decomposed implementation of the two types.  There should be no superior 
-- choice in terms of constraint deduction. This leads to `Pure.Elm.Application`
-- being slightly less constraint agnostic than `Pure.Elm` since many commands 
-- must declare the calling context for both types when called rather than 
-- inferring the context from the call. This is disappointing, but necessary to
-- unify the two into a routed application. 
--
-- The other option was to make the `rt -> msg` injector, as seen in `App`'s 
-- `_routing` field, existential and `Typeable`. But then the onus is on the
-- user to have a valid routing constructor in their message type and would
-- not permit easy dispatching on the route in both `_update` and `_view` unless
-- the user chose to store the route during the `_update`. This would also lead
-- to overlappable multi-routes with the possibility of externally- inaccessible 
-- routing states. I would prefer such multi-routes to be impelemented via nested 
-- `App`s, instead.
--
-- With these choices made, it was found that a simple solution to the constraint 
-- problem is a concrete constraint type.
--
-- > type MyApp = Elm MyMessageType MyRouteType
--
-- Or
--
-- > type MyApp a = Elm MyMessageType MyRouteType => a
--
-- This allows for simple, but required, typing of application-aware views:
--
-- > someView :: MyApp => View
--
-- or 
--
-- > someView :: MyApp View
--
-- Note that, unlike `Pure.Elm`, any constrained view infects its caller with
-- the constraint. While this was true in `Pure.Elm`, GHC was able to deduce 
-- the constraint type and apply it automatically to any un-typed methods. 
--
-- That is, the following would be valid in `Pure.Elm`:
--
-- > myWrapper = Div <||> [ someWrappedView ]
-- >
-- > someWrappedView = Button <| OnClick (const (command SomeMsg)) |> [ "My Button" ]
--
-- But in `Pure.Elm.Application`, the same code requires manual typing:
--
-- > myWrapper :: MyApp => View
-- > myWrapper = Div <||> [ someWrappedView ]
-- >
-- > someWrappedView :: MyApp => View
-- > someWrappedView = Button <| OnClick (const (command SomeMsg)) |> [ "My Button" ]

data App env st msg rt = App
  { _startup  :: [msg]
  , _receive  :: [msg]
  , _route    :: [rt -> msg]
  , _shutdown :: [msg]
  , _model    :: st
  , _update   :: !(Elm msg rt => rt -> msg -> env -> st -> IO st)
  , _view     :: !(Elm msg rt => rt -> env -> st -> View)
  }

-- | The URL type represents internal and external URLs: a value of URL is either
-- `Internal url` or `External url`. See `url` for case analysis and `internal` 
-- and `external` for URL construction.
data URL = Internal Txt | External Txt

-- | Case analysis for the URL type. If the value is `Internal a`, apply the 
-- first function to a; if the value is `External b`, apply the second function 
-- to b.
url :: (Txt -> a) -> (Txt -> a) -> URL -> a
url int ext = \case
  Internal i -> int i
  External e -> ext e

-- | Construct an internal URL.
internal :: Txt -> URL
internal = Internal

-- | Construct an external URL.
external :: Txt -> URL
external = External

-- | `Routes` class defines a valid routing type with associated conversions 
-- and parsing. 
class Routes rt where
  -- | A required fallback, or home, route.
  home :: rt

  -- | An optional title, by route. If `Nothing` is returned, the routing 
  -- machinery in this module will not touch the title when the route changes!
  -- This means any existing title will remain and must be overridden with 
  -- `retitle`. 
  --
  -- The default implementation is:
  --
  -- > const Nothing
  --
  title  :: rt -> Maybe Txt
  title = const Nothing

  -- | A conversion method from the route type to an internal or external URL.
  -- The default implementation is:
  --
  -- > const (internal "")
  --
  -- Note that `internal ""` is a no-op URL.
  --
  location :: rt -> URL
  location = const (internal "") -- no-op

  -- | A route parser using the `Routing` monad provided by `pure-router`. The
  -- default implementation is:
  --
  -- > dispatch home
  --
  routes :: forall x. Routing rt x
  routes = dispatch home

#ifdef __GHCJS__
foreign import javascript unsafe
  "var st = history.state || {}; st.elmScrollY = window.pageYOffset; st.elmScrollX = window.pageXOffset; history.replaceState(st,null,null);"
    store_scroll_position_js :: IO ()

foreign import javascript unsafe
  "$r = (history.state && history.state.elmScrollY) || 0" recall_page_y_offset_js :: IO Int

foreign import javascript unsafe
  "$r = (history.state && history.state.elmScrollX) || 0" recall_page_x_offset_js :: IO Int

foreign import javascript unsafe
  "window.scrollTo($1,$2)" scroll_to_js :: Int -> Int -> IO ()

foreign import javascript unsafe
  "window.scrollTo({ top: $2, left: $1, behavior: 'smooth' })" scroll_to_smooth_js :: Int -> Int -> IO ()

foreign import javascript unsafe
  "var a = document.querySelector('meta[name=\"description\"]'); if (a) { a.setAttribute('content', $1);} else { var meta = document.createElement('meta'); meta.setAttribute('name','description'); meta.setAttribute('content', $1); document.getElementsByTagName('head')[0].appendChild(meta)};"
    set_description_js :: Txt -> IO ()
#endif

describe :: Txt -> IO ()
describe d =
#ifdef __GHCJS__
  set_description_js d
#else
  pure ()
#endif

-- | Store current scroll position in the current `window.history.state`.
storeScrollPosition :: IO ()
storeScrollPosition = 
#ifdef __GHCJS__
  store_scroll_position_js
#else
  pure ()
#endif

-- | Handle a stored scroll position with a callback `f :: X-Offset -> Y-Offset -> IO ()`
-- If no non-zero scroll position is available, the callback is not called. 
withScrollPositionFromHistory :: (Int -> Int -> IO ()) -> IO ()
withScrollPositionFromHistory f = do
#ifdef __GHCJS__
  x <- recall_page_x_offset_js
  y <- recall_page_y_offset_js
  when (y /= 0 || x /= 0) (f x y)
#else
  pure ()
#endif

-- | Restore a scroll position immediately. 
--
-- Note: This method must be called after content is restored. In some cases
-- of dynamic content rendering, orchestration will be required to restore 
-- scroll position successfully.
--
-- Note: The approach taken in this module will not allow for restoring scroll 
-- positon when the browser's forward button is clicked.
restoreScrollPosition :: IO ()
restoreScrollPosition = withScrollPositionFromHistory setScrollPosition
  where
    setScrollPosition x y = do
#ifdef __GHCJS__
      scroll_to_js x y
#else
      pure ()
#endif

-- | Restore a scroll position smoothly. 
--
-- Note: This method must be called after content is restored. In some cases
-- of dynamic content rendering, orchestration will be required to restore 
-- scroll position successfully.
--
-- Note: The approach taken in this module will not allow for restoring scroll 
-- positon when the browser's forward button is clicked.
restoreScrollPositionSmooth :: IO ()
restoreScrollPositionSmooth = withScrollPositionFromHistory setScrollPositionSmooth
  where
    setScrollPositionSmooth x y = do
#ifdef __GHCJS__
      scroll_to_smooth_js x y
#else
      pure ()
#endif

-- | Link to a valid route via the `Routes` type class.
--
-- Be sure that: 
--
-- > forall rt. route routes (location rt) == Just rt
--
-- Note: This method stores a scroll position in the current history element 
-- before routing to a new path. See `restoreScrollPosition` for scroll 
-- restoration.
link :: (HasFeatures a, Routes rt) => rt -> a -> a
link = url lref' Href . location
  where
    lref' ref a = OnClickWith intercept (\_ -> handle) (Href ref a)
      where
        handle = do
          storeScrollPosition
          goto ref

data Command msg rt
  = Startup
  | Route rt
  | Routed rt
  | Retitle Txt
  | Message msg

type Elm msg rt = Pure.Elm.Elm (Command msg rt)

type Application env st msg rt = (Elm msg rt,Session st,Settings env)

-- | Command the application to retitle the page.
{-# INLINE retitle #-}
retitle :: Elm msg rt => Txt -> IO ()
retitle = Pure.Elm.command . Retitle

-- | Command the application to process a message. This can be called from any
-- constrained `Elm msg rt` context.
{-# INLINE command #-}
command :: Elm msg rt => msg -> IO ()
command = Pure.Elm.command . Message

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
reroute :: Elm msg rt => rt -> IO ()
reroute = Pure.Elm.command . Route

data LinkSettings = LinkSettings
  { blank :: Bool
  , noopener :: Bool
  , scopes :: [Txt]
  }

-- | Calling `processLinks` will turn any `href` values into click event 
-- interceptions that inject the route into the constraint-supplied `Elm` 
-- context. If supplied, via the `ProcessLinksSettings`, a set of `scopes` will 
-- be compared against the hostname of the `href` to detect internal links. 
-- Optionally, external links can be tagged with `rel="noopener"` and 
-- `target="_blank"`.
{-# INLINE processLinksWith #-}
processLinksWith :: LinkSettings -> View -> View
processLinksWith ls = go
  where
    blank_
      | blank ls  = Attribute "target" "_blank"
      | otherwise = id

    noopener_
      | noopener ls = Rel "noopener"
      | otherwise   = id

    lref' ref a = OnClickWith intercept (\_ -> handle) (Href ref a)
      where
        handle = do
          storeScrollPosition
          goto ref

    go v =
      let as = attributes (getFeatures v)
          ps = properties (getFeatures v) 
          v' = setChildren (fmap go (getChildren v)) v
      in case Map.lookup "href" ps <|> Map.lookup "href" as of
           Just ref 
             | isRelative ref 
             || hostname ref `elem` scopes ls -> lref' ref v'
             | otherwise                      -> v' <| blank_ . noopener_
           Nothing                            -> v'

-- | Calling `processLinks` will turn any `href` values in the `View` into click
-- event interceptions that inject the route into the constraint-supplied `Elm` 
-- context. External links are not injected. This implementation defaults to 
-- tagging external links with `rel="noopener"` and `target="blank"`, for 
-- safety.
--
-- Note:
--
-- Since external links are not intercepted, local, fully-scoped URLs are, also,
-- not injected. This can be an important point when interpreting views that are 
-- dynamically generated at runtime, like those produced from markdown. If the 
-- markdown contains local, internal, links that have a full to the application, they 
-- will cause the application to fully reload, when clicked. 
{-# INLINE processLinks #-}
processLinks :: (Elm msg rt, Routes rt) => View -> View
processLinks = processLinksWith (LinkSettings True True [])

#ifdef __GHCJS__
foreign import javascript unsafe
  "if (document.title != $1) { document.title = $1; }" set_title_js :: Txt -> IO ()

foreign import javascript unsafe
  "window.scrollTo({ top: 0 })" scroll_top_js :: IO ()

foreign import javascript unsafe
  "window.location.href = $1" set_location_js :: Txt -> IO ()

foreign import javascript unsafe
  "window.getHostname($1)" hostname_js :: Txt -> Txt

foreign import javascript unsafe
  "window.isRelative($1)" is_relative_js :: Txt -> Bool
#endif

setTitle :: Txt -> IO ()
setTitle t =
#ifdef __GHCJS__
  set_title_js t
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

setLocation :: Txt -> IO ()
setLocation l =
#ifdef __GHCJS__
  set_location_js l
#else
  pure ()
#endif

hostname :: Txt -> Txt
hostname t =
#ifdef __GHCJS__
  hostname_js t
#else
  ""
#endif

isRelative :: Txt -> Bool
isRelative t =
#ifdef __GHCJS__
  is_relative_js t
#else
  False
#endif

-- Turn an `App` with a supplied environment into a `View`.
{-# INLINE run #-}
run :: forall env st msg rt. (Typeable env, Typeable st, Typeable msg, Typeable rt, Routes rt) 
    => App env st msg rt -> env -> View
run App {..} = \config -> Div <||> [ router, Pure.Elm.run app config ]
  where
    router = View (Router (home :: rt) (Pure.Router.route routes))

    app :: Pure.Elm.App env (rt,st) (Command msg rt)
    app = Pure.Elm.App (Startup:fmap Message _startup) (fmap Message _receive) (fmap Message _shutdown) (home :: rt,_model) update view
      where
        lift = Message

        update :: Elm msg rt => Command msg rt -> env -> (rt,st) -> IO (rt,st)
        update Startup      _ st = do
          onRoute' (Pure.Elm.command . Routed)
          pure st

        update (Route newRoute) env st = do
          url goto setLocation (location newRoute)
          pure st

        update (Routed newRoute) env (oldRoute,st) = do
          st' <- foldM (\st m -> _update oldRoute (m newRoute) env st) st _route
          scrollTop
          for_ (title newRoute) setTitle 
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

-- | Access an `Application`s `Settings`, the current environment or configuration.
{-# INLINE settings #-}
settings :: Settings settings => settings
settings = ?elm_application_settings

type Session session = ?elm_application_session :: session

-- | Access an `Application`s `Session`, the current state.
{-# INLINE session #-}
session :: Session session => session
session = ?elm_application_session

type Page env st msg rt = Elm msg rt => env -> st -> View

-- | Supply an environment and state to a context with access to an implicit
-- environment and current state.
{-# INLINE page #-}
page :: (Elm msg rt => ((Session st,Settings env) => View)) -> Page env st msg rt
page f = \env st -> 
  let 
    ?elm_application_settings = env
    ?elm_application_session  = st
  in
    f

-- | Subscribe to any globally broadcast messages on the `msg'` channel by way of
-- re-wrapping for the correct `Elm msg rt` context.
{-# INLINE subscribeWith #-}
subscribeWith :: forall rt msg msg'. (Typeable msg', Typeable rt, Elm msg rt) => (msg' -> msg) -> IO (Subscription msg')
subscribeWith f = Pure.Elm.Sub.unsafeSubscribeWith (?command . Message @msg @rt . f)

-- | Subscribe to any globally broadcast message on the `msg` channel by way of
-- injecting into a `Command msg rt`.
{-# INLINE subscribe #-}
subscribe :: forall rt msg. (Typeable msg, Typeable rt, Elm msg rt) => IO (Subscription msg)
subscribe = Pure.Elm.Sub.unsafeSubscribeWith (?command . Message @msg @rt)

{-# INLINE publishing #-}
publishing :: forall rt msg a. (Typeable msg, Typeable rt) => (Elm msg rt => a) -> a
publishing a = let ?command = Pure.Elm.Sub.publish' in a
