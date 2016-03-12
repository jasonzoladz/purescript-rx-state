# purescript-rx-state
NOT READY YET.  COME BACK SOON.


A tiny library for unidirectional data flow in PureScript applications using RxJS.  (tldr: [Erik Meijer](https://en.wikipedia.org/wiki/Erik_Meijer_(computer_scientist)) has already solved your state management problems.)

As this library relies on [RxJS](https://github.com/Reactive-Extensions/RxJS), you'll need to `npm install rx`.

(Note:  I've deliberately not taken a dependency on [`purescript-rx`](https://github.com/anttih/purescript-rx).  It's a very nice wrapper (and you should use it), but I've wrapped a couple RxJS functions that don't line-up with the types defined in `purescript-rx`.  There are no conflicts, however.  You can use it alongside this.)

####Usage

It's dead simple.  The API is very similar to `startApp` in Elm.

First, define your `State`.  It must be some record type, like:

```purescript
type State = { num :: Int }
```
Then define some actions and effects:

```purescript
data Action
  = Increment
  | Decrement
  | NoOp

data Effect
  = AjaxLaunchMissles
  | NoFx
```

Next, define a `Channel`s for your `Action`s and `Effect`s.  
**Note: your `Channel` must carry a `Foldable`.**  Most likely, you'll use an `Array`, like so:

```purescript
actionChannel :: Channel (Array Action)
actionChannel = newChannel []

effectChannel :: Channel (Array Effect)
effectChannel = newChannel []
```

Now, define your update function.  Your update function takes a `State`, and an `Action`, and returns a new `State`.

```purescript
update :: State -> Action -> State
update state action = do
  case action of
    Increment -> state { num = state.num + 1 }
    Decrement -> state { num = state.num - 1 }
    NoOp      -> state
```

And define your function that performs (possibly asynchronous) effects:

```purescript
performEffect :: forall e. Effect -> Eff ( console :: CONSOLE, ajax :: AJAX | e) Unit
performEffect fx =
  case fx of
    AjaxLaunchMissles -> runAff
                            (\_ -> send [ Increment ] actionsChannel)
                            (\_ -> send [ Increment ] actionsChannel)
                            (affjax $ defaultRequest { url = "/api", method = GET })

    NoFx              -> return unit
```

Finally, wire it up in `main` using `startApp`...  Here, I'm using `purescript-react` for rendering.  A minimal, but complete, example is [here](https://github.com/jasonzoladz/purescript-rx-state-react-example).

```purescript
main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff ) Unit
main = startApp update performEffect myRender actionsChannel effectsChannel initState

  where
    view :: AppState -> ReactElement
    view appState = D.div' [ createFactory hello appState ]

    myRender state = do
      container <- elm'
      RD.render (view state) container

    elm' :: forall eff. Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      return $ fromJust (toMaybe elm)
```
