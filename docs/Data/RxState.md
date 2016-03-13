## Module Data.RxState

#### `Channel`

``` purescript
data Channel :: * -> *
```

Represents a stream of values.  Values are broadcast to all
subscribers. (In terms of RxJS, it's a `BehaviorSubject`.)

##### Instances
``` purescript
Functor Channel
Apply Channel
Applicative Channel
Bind Channel
Monad Channel
```

#### `newChannel`

``` purescript
newChannel :: forall a. a -> Channel a
```

Create a new channel with an initial value.

#### `send`

``` purescript
send :: forall a eff. a -> Channel a -> Eff eff Unit
```

Send a value to a channel.

#### `foldp`

``` purescript
foldp :: forall a b. (a -> b -> b) -> b -> Channel a -> Channel b
```

Fold from the past, returning intermediate results. (In terms of RxJS, it's just the `scan` function.)

#### `subscribe`

``` purescript
subscribe :: forall eff a. Channel a -> (a -> Eff eff Unit) -> Eff eff Unit
```

Takes values off the channel and passes them to an effectful function.

#### `merge`

``` purescript
merge :: forall a. Channel a -> Channel a -> Channel a
```

Merge two channels into a single channel.

#### `_map`

``` purescript
_map :: forall a b. (a -> b) -> Channel a -> Channel b
```

Create a new channel by mapping a function over an existing channel.

#### `(<~)`

``` purescript
infixl 9 _map as <~
```

_left-associative / precedence 9_

Fancy map.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> Channel a -> Channel a
```

Create a new channel by selecting values from an existing channel.

#### `just`

``` purescript
just :: forall a. a -> Channel a
```

Create a channel with a single value.

#### `flatMap`

``` purescript
flatMap :: forall a b. Channel a -> (a -> Channel b) -> Channel b
```

Monadic bind.

#### `combineLatest`

``` purescript
combineLatest :: forall a b c. (a -> b -> c) -> Channel a -> Channel b -> Channel c
```

Takes a combining function and two input channels, and produces a new channel.  The new
channel yields a result whenever a value fires on either of the input channels.  This
result derives from combining the newly fired value and the most recent (older) value
from the other input channel.

#### `take`

``` purescript
take :: forall a. Int -> Channel a -> Channel a
```

Creates a new channel comprising at most `n` values from the input channel.

#### `startApp`

``` purescript
startApp :: forall eff state action effect view f. (Foldable f) => (state -> action -> state) -> (effect -> Eff eff Unit) -> (state -> Eff eff view) -> Channel (f action) -> Channel (f effect) -> state -> Eff eff Unit
```

Kicks off your app.  Takes an `update` function, an `effect` function,
a `render` function, an `actions` channel, an `effects` channel, and an initial state.


