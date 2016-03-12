module RxState where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable

foreign import data Channel :: * -> *

foreign import newChannel :: forall a. a -> Channel a

foreign import send :: forall a eff. a -> Channel a -> Eff eff Unit

foreign import foldp :: forall a b. (a -> b -> b) -> b -> Channel a -> Channel b

foreign import subscribe :: forall eff a. Channel a -> (a -> Eff eff Unit) -> Eff eff Unit

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
