# purescript-rx-state
NOT READY YET.  COME BACK SOON.


A tiny library for unidirectional data flow in PureScript applications using RxJS.  (tl;dr: [Erik Meijer](https://en.wikipedia.org/wiki/Erik_Meijer_(computer_scientist)) has already solved your state management problems.)

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
  = AjaxIncrement
  | NoFx
```

Next, define `Channel`s for your `Action`s and `Effect`s.  
**Note: your `Channel` must carry a `Foldable`.**  Most likely, you'll use an `Array`, like so:

```purescript
actionsChannel :: Channel (Array Action)
actionsChannel = newChannel []

effectsChannel :: Channel (Array Effect)
effectsChannel = newChannel []
```

Now, define your update function.  Your update function takes a `State`, and an `Action`, and returns a new `State`.

```purescript
update :: State -> Action -> State
update state action =
  case action of
    Increment -> state { num = state.num + 1 }
    Decrement -> state { num = state.num - 1 }
    NoOp      -> state
```

And define your function that performs (possibly asynchronous) effects.  If an asynchronous `Effect` returns a payload, you simply dispatch the payload as a part of another `Action`.

```purescript
performEffect :: forall e. Effect -> Eff ( console :: CONSOLE, ajax :: AJAX | e) Unit
performEffect fx =
  case fx of
    AjaxIncrement -> runAff
                            (\_ -> send [ Increment ] actionsChannel)
                            (\_ -> send [ Increment ] actionsChannel)
                            ((affjax $ defaultRequest { url = "http://jsonplaceholder.typicode.com/posts/1", method = GET })

    NoFx              -> return unit
```


In your `views`, you can dispatch an `Action` or `Effect` by using the `send` function to put an `Action` or `Effect` on the corresponding `Channel`.  Again, note that you are `send`ing a `Foldable Action`/`Foldable Effect`, since you may want to dispatch multiple actions or effects at the same time.

If using `purescript-react`, you might do this:

```purescript
hello :: ReactClass State
hello = createClass $ spec unit $ \ctx -> do
  state <- getProps ctx
  return $
    D.div [] [ D.h1 []
                  [ D.text "Hello, the state is: "
                  , D.text (show state.num)
                  ],
               D.div []
                  [ D.button [ P.onClick (\_ -> send [Increment] actionsChannel) ]
                             [ D.text "Increment" ]
                  , D.button [ P.onClick \_ -> send [Decrement] actionsChannel ]
                             [ D.text "Decrement" ]
                  , D.button [ P.onClick \_ -> send [AjaxIncrement] effectsChannel ]
                             [ D.text "Ajax Increment" ]
                  ]
```

Finally, wire it up in `main` using `startApp`.  Here, I'm using `purescript-react` for rendering but you could substitute any function with the same type as:

```purescript
fooRender :: forall view eff.  State -> Eff eff view
```

```purescript
main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff ) Unit
main = startApp update performEffect myRender actionsChannel effectsChannel initState

  where
    view :: State -> ReactElement
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

A minimal, but complete, example is in the  [example repo](https://github.com/jasonzoladz/purescript-rx-state-react-example).
