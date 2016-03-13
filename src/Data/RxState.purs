module Data.RxState
  ( Channel (..)
  , newChannel
  , send
  , foldp
  , subscribe
  , merge
  , _map
  , filter
  , just
  , flatMap
  , combineLatest
  , take
  , startApp
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (class Foldable, traverse_, foldl)

-- | Represents a stream of values.  Values are broadcast to all
-- | subscribers. (In terms of RxJS, it's a `BehaviorSubject`.)
foreign import data Channel :: * -> *

-- | Create a new channel with an initial value.
foreign import newChannel :: forall a. a -> Channel a

-- | Send a value to a channel.
foreign import send :: forall a eff. a -> Channel a -> Eff eff Unit

-- | Fold from the past, returning intermediate results. (In terms of RxJS, it's just the `scan` function.)
foreign import foldp :: forall a b. (a -> b -> b) -> b -> Channel a -> Channel b

-- | Takes values off the channel and passes them to an effectful function.
foreign import subscribe :: forall eff a. Channel a -> (a -> Eff eff Unit) -> Eff eff Unit

-- | Merge two channels into a single channel.
foreign import merge :: forall a. Channel a -> Channel a -> Channel a

-- | Create a new channel by mapping a function over an existing channel.
foreign import _map :: forall a b. (a -> b) -> Channel a -> Channel b


-- | Create a new channel by selecting values from an existing channel.
foreign import filter :: forall a. (a -> Boolean) -> Channel a -> Channel a

-- | Create a channel with a single value.
foreign import just :: forall a. a -> Channel a

-- | Monadic bind.
foreign import flatMap :: forall a b. Channel a -> (a -> Channel b) -> Channel b

-- | Takes a combining function and two input channels, and produces a new channel.  The new
-- | channel yields a result whenever a value fires on either of the input channels.  This
-- | result derives from combining the newly fired value and the most recent (older) value
-- | from the other input channel.
foreign import combineLatest :: forall a b c. (a -> b -> c) -> Channel a -> Channel b -> Channel c

-- | Creates a new channel comprising at most `n` values from the input channel.
foreign import take :: forall a. Int -> Channel a -> Channel a

instance functorChannel :: Functor Channel where
  map = _map

instance applyChannel :: Apply Channel where
  apply = combineLatest id

instance applicativeChannel :: Applicative Channel where
  pure = just

instance observableChannel :: Bind Channel where
  bind = flatMap

instance monadChannel :: Monad Channel

-- | Kicks off your app.  Takes an `update` function, an `effect` function,
-- | a `render` function, an `actions` channel, an `effects` channel, and an initial state.
startApp :: forall eff state action effect view f. (Foldable f)
                                                => (state -> action -> state)
                                                -> (effect -> Eff eff Unit)
                                                -> (state -> Eff eff view)
                                                -> Channel (f action)
                                                -> Channel (f effect)
                                                -> state
                                                -> Eff eff Unit
startApp updateFunction effectFunction renderFunction actionChan effectChan initState = do

  let stateChannel = foldp (updateMany updateFunction) initState actionChan

  subscribe stateChannel (\state -> (renderFunction state) >>= \_ -> return unit)

  subscribe effectChan (effectsMany effectFunction)

  where
  updateMany :: forall state action eff f. (Foldable f)
                                      => (state -> action -> state)
                                      -> f action
                                      -> state
                                      -> state
  updateMany f xs s = foldl f s xs

  effectsMany f = traverse_ f
