{-# language LambdaCase, CPP, OverloadedStrings #-}
module Pure.Elm.Application.Route where

import Pure.Elm.Application.Scroll

import Pure 
import Pure.Data.Txt as Txt
import Pure.Router

import qualified Data.Map as Map

import Control.Applicative

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

data LinkSettings = LinkSettings
  { blank :: Bool
  , noopener :: Bool
  , preloader :: Maybe (Txt -> View -> View)
  , scopes :: [Txt]
  }

instance Default LinkSettings where
  def = LinkSettings True True Nothing []

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

    preload ref =
      case preloader ls of
        Just f -> f ref
        _      -> id

    lref' ref a = OnClickWith intercept (\_ -> goto ref) (Href ref a)

    go v =
      let as = attributes (getFeatures v)
          ps = properties (getFeatures v) 
          v' = setChildren (fmap go (getChildren v)) v
          ipo = Txt.isPrefixOf
      in case Map.lookup "href" ps <|> Map.lookup "href" as of
           Just ref
             |  "#"       `ipo` ref
             || "mailto:" `ipo` ref
             || "webcal:" `ipo` ref           -> v'

             | isRelative ref 
             || hostname ref `elem` scopes ls -> preload ref (lref' ref v')

             | otherwise                      -> v' <| blank_ . noopener_

           Nothing                            -> v'

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

setTitle :: Txt -> IO ()
setTitle t =
#ifdef __GHCJS__
  set_title_js t
#else
  pure ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "window.location.href = $1" set_location_js :: Txt -> IO ()

foreign import javascript unsafe
  "window.getHostname($1)" hostname_js :: Txt -> Txt

foreign import javascript unsafe
  "window.isRelative($1)" is_relative_js :: Txt -> Bool

foreign import javascript unsafe
  "if (document.title != $1) { document.title = $1; }" set_title_js :: Txt -> IO ()
#endif

