# pure-elm

An implementation of the [Elm Architecture](https://guide.elm-lang.org/architecture/) in Pure.

## Demo

An implementation of the button example from [elm-architecture-tutorial](https://github.com/evancz/elm-architecture-tutorial/blob/master/examples/01-button.elm).

```haskell
module Main where

import Pure.Elm

main = inject body (run app)
  where
    app = App model update view

-- Model

type Model = Int

model :: Model
model = 0

-- Update

data Msg = Increment | Decrement

update :: Msg -> Model -> IO Int
update Increment = pure . succ
update Decrement = pure . pred

-- View

view :: Elm Msg => Model -> View
view model = 
  Div <||>
    [ Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , Br
    , text model
    , Br
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```
