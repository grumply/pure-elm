# pure-elm

An implementation of the [Elm Architecture](https://guide.elm-lang.org/architecture/) in Pure with extension for easy nesting.

## Demo

An implementation of the button example from [elm-architecture-tutorial](https://github.com/evancz/elm-architecture-tutorial/blob/master/examples/01-button.elm).

```haskell
module Main where

import Pure.Elm

main = inject body (run app env)
  where
    app = App [] [] [] mdl update view
    mdl = 0
    env = ()

-- Types

type Env = ()

type Model = Int

-- Update

data Msg = Increment | Decrement

update :: Elm Msg => Msg -> Env -> Model -> IO Int
update Increment _ = pure . succ
update Decrement _ = pure . pred

-- View

view :: Elm Msg => Env -> Model -> View
view _ model = 
  Div <||>
    [ Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , Br
    , text model
    , Br
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```

## Core App/API

For reference, the core `App` type is:

```haskell
data App env st msg = App 
  { _startup  :: [msg]
  , _receive  :: [env -> msg]
  , _shutdown :: [msg]
  , _model    :: st
  , _update   :: Elm msg => msg -> env -> st -> IO st 
  , _view     :: Elm msg => env -> st -> View
  }
```

The core API:

```haskell
Pure.Elm.run :: (Typeable env, Typeable st, Typeable msg) => App env st msg -> env -> View
Pure.Elm.command :: Elm msg => msg -> IO ()
Pure.Elm.map :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
```

## Publish/Subscribe

The subscription API:

```haskell
Pure.Elm.subscribe :: (Typeable msg, Elm msg) => IO ()
Pure.Elm.subscribeWith :: (Typeable msg', Elm msg) => (msg' -> msg) -> IO ()
Pure.Elm.publish :: Typeable msg => msg -> IO ()
Pure.Elm.unsubscribe :: forall msg. (Typeable msg, Elm msg) => Unique -> IO ()
Pure.Elm.unsubscribeWith :: Typeable msg => Proxy msg -> Unique -> IO ()
```

## Memoization

The memoization API:

```haskell
Pure.Elm.memo :: (Typeable a, Typeable b, Elm msg) => (b -> msg) -> (a -> IO b) -> a -> IO ()
```