module Control.Efcom.Writer
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

import Control.Efcom
import Control.Efcom.Cell


data Writer w

type instance Effect (Writer w) = '[Cell w]


{-# INLINE runWriter #-}
runWriter ::
  forall w efs a. Monoid w =>
  Com (Writer w : efs) a -> Com efs (a, w)
runWriter ef =
  runCell @(Writer w) (runComHead ef) mempty (\ a w -> return (a, w))


{-# INLINE execWriter #-}
execWriter ::
  forall w efs a. Monoid w =>
  Com (Writer w : efs) a -> Com efs w
execWriter ef =
  runCell @(Writer w) (runComHead ef) mempty (\ _ w -> return w)


{-# INLINE tell #-}
tell :: forall w efs. (Monoid w, In (Writer w) efs) => w -> Com efs ()
tell w = do
  w0 <- readCell @(Writer w)
  writeCell @(Writer w) (mappend w0 w)


{-# INLINE lazyTell #-}
lazyTell :: forall w efs. (Monoid w, In (Writer w) efs) => w -> Com efs ()
lazyTell w = do
  w0 <- readCell @(Writer w)
  lazyWriteCell @(Writer w) (mappend w0 w)


{-# INLINE lazyListen #-}
lazyListen ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Com efs a -> Com efs (a, w)
lazyListen ef = do
  delimitCell @(Writer w) ef mempty
    (\ a w -> lazyTell w >> return (a, w)) mappend


{-# INLINE listen #-}
listen ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Com efs a -> Com efs (a, w)
listen ef = do
  delimitCell @(Writer w) ef mempty
    (\ a w -> tell w >> return (a, w)) mappend


{-# INLINE lazyPass #-}
lazyPass ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Com efs (a, w -> w) -> Com efs a
lazyPass ef = do
  delimitCell @(Writer w) ef mempty
    (\ (a, f) w -> lazyTell (f w) >> return a) mappend


{-# INLINE pass #-}
pass ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Com efs (a, w -> w) -> Com efs a
pass ef = do
  delimitCell @(Writer w) ef mempty
    (\ (a, f) w -> tell (f w) >> return a) mappend


{-# INLINE lazyListens #-}
lazyListens ::
  forall w efs a b. (Monoid w, In (Writer w) efs) =>
  (w -> b) -> Com efs a -> Com efs (a, b)
lazyListens f ef = do
  delimitCell @(Writer w) ef mempty
    (\ a w -> lazyTell w >> return (a, f w)) mappend


{-# INLINE listens #-}
listens ::
  forall w efs a b. (Monoid w, In (Writer w) efs) =>
  (w -> b) -> Com efs a -> Com efs (a, b)
listens f ef = do
  delimitCell @(Writer w) ef mempty
    (\ a w -> tell w >> return (a, f w)) mappend


{-# INLINE lazyCensor #-}
lazyCensor ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  (w -> w) -> Com efs a -> Com efs a
lazyCensor f ef =
  delimitCell @(Writer w) ef mempty
    (\ a w -> lazyTell (f w) >> return a) mappend


{-# INLINE censor #-}
censor ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  (w -> w) -> Com efs a -> Com efs a
censor f ef = 
  delimitCell @(Writer w) ef mempty
    (\ a w -> tell (f w) >> return a) mappend
