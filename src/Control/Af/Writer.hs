module Control.Af.Writer
  ( Writer
  , runWriter
  , execWriter
  , tell
  , writer
  , listen
  , pass
  , listens
  , censor
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
  runCell @(Writer w) (meetEffect af) mempty (\a w -> return (a, w))


{-# INLINE execWriter #-}
execWriter ::
  forall w efs a. Monoid w =>
  Af (Writer w : efs) a -> Af efs w
execWriter af =
  runCell @(Writer w) (meetEffect af) mempty (\_ w -> return w)


{-# INLINE tell #-}
tell :: forall w efs. (Monoid w, In (Writer w) efs) => w -> Af efs ()
tell w = do
  w0 <- readCell @(Writer w)
  writeCell @(Writer w) (mappend w0 w)


{-# INLINE writer #-}
writer ::
  forall w efs a. (Monoid w, In (Writer w) efs) => (a, w) -> Af efs a 
writer (a, w) = tell w >> return a


{-# INLINE listen #-}
listen ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Af efs a -> Af efs (a, w)
listen af = do
  scopeCell @(Writer w) af mempty
    (\a w -> tell w >> return (a, w)) tell


{-# INLINE pass #-}
pass ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  Af efs (a, w -> w) -> Af efs a
pass af = do
  scopeCell @(Writer w) af mempty
    (\(a, f) w -> tell (f w) >> return a) tell


{-# INLINE listens #-}
listens ::
  forall w efs a b. (Monoid w, In (Writer w) efs) =>
  (w -> b) -> Af efs a -> Af efs (a, b)
listens f m = do
  (a, w) <- listen m
  return (a, f w)


{-# INLINE censor #-}
censor ::
  forall w efs a. (Monoid w, In (Writer w) efs) =>
  (w -> w) -> Af efs a -> Af efs a
censor f m = pass $ do
  a <- m
  return (a, f)
