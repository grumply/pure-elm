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
Pure.Elm.run :: App env st msg -> env -> View
Pure.Elm.command :: Elm msg => msg -> IO ()
Pure.Elm.map :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
```

## Publish/Subscribe

The subscription API:

```haskell
Pure.Elm.subscribe :: Elm msg => IO ()
Pure.Elm.subscribeWith :: Elm msg => (msg' -> msg) -> IO ()
Pure.Elm.publish :: msg -> IO ()
Pure.Elm.publishing :: (Elm msg => a) -> a
```

Note that `unsubscribe` is unnecessary, in general, but exists if required and must be paired with `subscribe'` rather than `subscribe`.

## Memoization

The memoization API:

```haskell
Pure.Elm.memo :: Elm msg => (b -> msg) -> (a -> IO b) -> a -> IO ()
```

As an example of `memo` in action:

```haskell
main = inject body (run test ())
  where 
    test = App [] [] [] Model update view

data Model = Model
data Msg = Test Int | Print Txt
data Tag

update (Print m) _ mdl = logJSON m >> pure mdl
update t _ mdl = do
  flip (memo @Tag Print) t $ \_ -> do
    logJSON ("Getting time" :: Txt)
    toPrettyTime <$> time 
  pure mdl   

view _ _ = Div <||>
  [ Button <| OnClick (\_ -> command (Test 1)) |> [ "Test 1" ]
  , Button <| OnClick (\_ -> command (Test 2)) |> [ "Test 2" ]
  ]
```

When compiled with optimization, multiple clicks of `Test 1` or `Test 2` results in a single call to `act`. Alternating calls to `Test 1` and `Test 2` results in multiple calls to `act`.

When compiled without optimization, `act` is called with every click of the buttons.

Note: `memo` does not work in event handlers, like `OnClick`, etc....