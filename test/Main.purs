module Test.Main where

import Prelude
import Data.Array as Array
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (writeRef, REF, newRef, readRef, modifyRef)
import Control.Monad.Eff.Timer (setTimeout, TIMER)
import Control.Observable (fromFoldable, take, observe, Observable, OBSERVABLE)
import Control.Observable.Time (bufferEvery, debounce, timeout, delay, interval)
import Data.Array ((..))
import Data.Monoid (mempty, class Monoid)
import Test.Unit (suite, test, Test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

collectVals :: forall a e m. (Monoid m) => (a -> m) -> Observable a -> Aff (ref :: REF, observable :: OBSERVABLE | e) m
collectVals wrap o = makeAff \reject resolve -> do
  coll <- newRef mempty
  let collectOne a = modifyRef coll (flip append (wrap a))
      allDone = readRef coll >>= resolve
  observe collectOne reject allDone o
  pure unit

expect :: forall a e. (Eq a, Show a) => Array a -> Observable a -> Test (observable :: OBSERVABLE, ref :: REF | e)
expect m o = do
  r <- collectVals Array.singleton o
  equal m r

expectEqual :: forall a e. (Eq a, Show a) => Observable a -> Observable a -> Test (observable :: OBSERVABLE, ref :: REF | e)
expectEqual o1 o2 = do
  r1 <- collectVals Array.singleton o1
  r2 <- collectVals Array.singleton o2
  equal r1 r2

expectAfter :: forall a e. (Eq a, Show a) => Int -> Array a -> Observable a -> Test (observable :: OBSERVABLE, timer :: TIMER, ref :: REF | e)
expectAfter t m o = do
  ok <- liftEff (newRef false)
  liftEff (setTimeout t $ writeRef ok true)
  result <- collectVals Array.singleton o
  ready <- liftEff (readRef ok)
  assert ("test should have taken more than " <> show t <> "ms to complete but didn't") ready
  equal m result

main :: forall e. Eff (avar :: AVAR, ref :: REF, console :: CONSOLE, testOutput :: TESTOUTPUT, observable :: OBSERVABLE, timer :: TIMER | e) Unit
main = runTest do

  suite "time operations" do
    test "delay" do
      (liftEff $ delay 10 (fromFoldable (1..9))) >>= expectAfter 10 (1..9)
    test "interval" do
      tick <- liftEff $ interval 10
      expectAfter 30 (0..2) (take 3 tick)
    test "timeout" do
      tick <- liftEff $ interval 10
      t <- liftEff $ timeout 35 tick (fromFoldable (1..3))
      expectAfter 35 [0,1,2,1,2,3] t
    test "debounce" do
      tick <- liftEff $ interval 10 >>= delay 5
      deb <- liftEff $ debounce 20 tick
      expectAfter 60 [0,2,4] $ take 3 deb
    test "bufferEvery" do
      tick <- liftEff $ interval 10 >>= delay 5
      buf <- liftEff $ bufferEvery 20 tick
      expectAfter 60 [[0],[1,2],[3,4],[5,6]] $ take 4 buf <#> Array.fromFoldable
