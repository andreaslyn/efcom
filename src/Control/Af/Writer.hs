module Control.Af.Writer
  ( Writer
  , runWriter
  , execWriter
  , tell
  , lazyTell
  , listen
  , lazyListen
  , pass
  , lazyPass
  , listens
  , lazyListens
  , censor
  , lazyCensor
  ) where

import Control.Af
import Control.Af.Cell


data Writer w

type instance Effect (Writer w) = '[Cell w]


{-# INLINE runWriter #-}
runWriter ::
  forall w efs a. Monoid w =>
  Af (Writer w : efs) a -> Af efs (a, w)
runWriter af =
  runCell @(Writer w) (runAfHead af) mempty (\ a w -> return (a, w))


{-# INLINE execWriter #-}
execWriter ::
  forall w efs a. Monoid w =>
  Af (Writer w : efs) a -> Af efs w
execWriter af =
  runCell @(Writer w) (runAfHead af) mempty (\ _ w -> return w)


{-# INLINE tell #-}
tell :: forall w efs. (Monoid w, In (Writer w) efs) => w -> Af efs ()
tell w = do
  w0 <- readCell @(Writer w)
  writeCell @(Writer w) (mappend w0 w)


{-# INLINE lazyTell #-}
lazyTell :: forall w efs. (Monoid w, In (Writer w) efs) => w -> Af efs ()
lazyTell w = do
  w0 <- readCell @(Writer w)
  lazyWriteCell @(Writer w) (mappend w0 w)


{-# INLINE lazyListen #-}
lazyListen ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Af efs a -> Af efs (a, w)
lazyListen af = do
  delimitCell @(Writer w) af mempty
    (\ a w -> lazyTell w >> return (a, w)) mappend


{-# INLINE listen #-}
listen ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Af efs a -> Af efs (a, w)
listen af = do
  delimitCell @(Writer w) af mempty
    (\ a w -> tell w >> return (a, w)) mappend


{-# INLINE lazyPass #-}
lazyPass ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Af efs (a, w -> w) -> Af efs a
lazyPass af = do
  delimitCell @(Writer w) af mempty
    (\ (a, f) w -> lazyTell (f w) >> return a) mappend


{-# INLINE pass #-}
pass ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Af efs (a, w -> w) -> Af efs a
pass af = do
  delimitCell @(Writer w) af mempty
    (\ (a, f) w -> tell (f w) >> return a) mappend


{-# INLINE lazyListens #-}
lazyListens ::
  forall w efs a b. (Monoid w, In (Writer w) efs) =>
  (w -> b) -> Af efs a -> Af efs (a, b)
lazyListens f af = do
  delimitCell @(Writer w) af mempty
    (\ a w -> lazyTell w >> return (a, f w)) mappend


{-# INLINE listens #-}
listens ::
  forall w efs a b. (Monoid w, In (Writer w) efs) =>
  (w -> b) -> Af efs a -> Af efs (a, b)
listens f af = do
  delimitCell @(Writer w) af mempty
    (\ a w -> tell w >> return (a, f w)) mappend


{-# INLINE lazyCensor #-}
lazyCensor ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  (w -> w) -> Af efs a -> Af efs a
lazyCensor f af =
  delimitCell @(Writer w) af mempty
    (\ a w -> lazyTell (f w) >> return a) mappend


{-# INLINE censor #-}
censor ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  (w -> w) -> Af efs a -> Af efs a
censor f af = 
  delimitCell @(Writer w) af mempty
    (\ a w -> tell (f w) >> return a) mappend
