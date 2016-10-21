module Control.Observable.Time
  ( delay
  , interval
  , timeout
  , debounce
  , bufferEvery
  ) where

import Prelude
import Control.Monad.Eff.Timer (clearInterval, setInterval, setTimeout, TIMER)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.ST (readSTRef, writeSTRef, newSTRef, modifySTRef)
import Control.Observable (bufferOn, sampleOn, EffO, free, observe, observable, Observable)
import Data.CatList (CatList)
import Data.Maybe (maybe, Maybe(Just, Nothing))

-- | Take an `Observable` and delay it by the given number of
-- | milliseconds.
delay :: forall a e. Int -> Observable a -> EffO (timer :: TIMER | e) (Observable a)
delay t o = observable \sink -> do
  let next v = void $ setTimeout t $ sink.next v
      complete = void $ setTimeout t sink.complete
  sub <- observe next sink.error complete o
  free [sub]

-- | Starting at 0, yield an incrementing integer value every `t`
-- | milliseconds.
-- |
-- | Note that this `Observable` never completes. It will keep counting
-- | until you unsubscribe from it.
interval :: forall e. Int -> EffO (timer :: TIMER | e) (Observable Int)
interval t = unsafeCoerceEff $ observable \sink -> do
  counter <- newSTRef (-1)
  tick <- setInterval t $ modifySTRef counter (_ + 1) >>= sink.next
  pure {unsubscribe: clearInterval tick}

-- | Yield the values from the first `Observable` until the timeout, given
-- | in milliseconds, has elapsed. Then, start yielding values from the
-- | second `Observable`.
timeout :: forall a e. Int -> Observable a -> Observable a -> EffO (timer :: TIMER | e) (Observable a)
timeout t o1 o2 = unsafeCoerceEff $ observable \sink -> do
  current <- newSTRef Nothing
  let unsubscribe = readSTRef current >>= maybe (pure unit) _.unsubscribe
  timer <- setTimeout t do
    unsubscribe
    void $ observe sink.next sink.error sink.complete o2 >>= Just >>> writeSTRef current
  observe sink.next sink.error sink.complete o1 >>= Just >>> writeSTRef current
  pure {unsubscribe}

-- | Every `t` milliseconds, yield the most recent value that was yielded by
-- | the input `Observable`.
debounce :: forall a e. Int -> Observable a -> EffO (timer :: TIMER | e) (Observable a)
debounce t o = interval t >>= flip sampleOn o >>> pure

-- | Buffer the values yielded by the input `Observable` each interval,
-- | and yield them as a `CatList` at the end of it. The interval is
-- | specified in milliseconds.
bufferEvery :: forall a e. Int -> Observable a -> EffO (timer :: TIMER | e) (Observable (CatList a))
bufferEvery t o = interval t >>= flip bufferOn o >>> pure
