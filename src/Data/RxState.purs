module Data.RxState
  ( Channel (..)
  , newChannel
  , send
  , foldp
  , subscribe
  , merge
  , (<~)
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

foreign import data Channel :: * -> *

foreign import newChannel :: forall a. a -> Channel a

foreign import send :: forall a eff. a -> Channel a -> Eff eff Unit

foreign import foldp :: forall a b. (a -> b -> b) -> b -> Channel a -> Channel b

foreign import subscribe :: forall eff a. Channel a -> (a -> Eff eff Unit) -> Eff eff Unit

foreign import merge :: forall a. Channel a -> Channel a -> Channel a

foreign import _map :: forall a b. (a -> b) -> Channel a -> Channel b

infixl 9 _map as <~

foreign import filter :: forall a. (a -> Boolean) -> Channel a -> Channel a

foreign import just :: forall a. a -> Channel a

foreign import flatMap :: forall a b. Channel a -> (a -> Channel b) -> Channel b

foreign import combineLatest :: forall a b c. (a -> b -> c) -> Channel a -> Channel b -> Channel c

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
