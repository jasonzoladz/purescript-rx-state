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

Now, define your update function and effect function.  Your update function takes a `State`, and an `Action`, and returns a new `State`.

..............
