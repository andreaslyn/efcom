{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Af.Shortcut
  ( Shortcut
  , runShortcut
  , takeShortcut
  , scopeShortcut
  ) where

import Control.Af.Internal.Effect
import Control.Af.Internal.AfArray
import Control.Af.Internal.Af
import Control.Af.Internal.In

import Unsafe.Coerce (unsafeCoerce)


{-# INLINE runShortcut #-}
runShortcut ::
  forall ref sh efs a b.
  Af (Shortcut sh ref : efs) a -> -- Effectful computation to execute.
  (a -> Af efs b) ->        -- Action used if computation returns
                            -- without taking the shortcut.
  (sh -> Af efs b) ->       -- Action used if shortcut is taken.
  Af efs b
runShortcut af f g = Af $ \ sz ar0 s0 ->
  case unAf af sz ar0 s0 of
    (# ar1, s1, (# e | #) #) ->
      case readAfArray @Int ar1 0# s1 of
        (# s2, i #) ->
          if i == 0
          then
            unAf (g (unsafeCoerce e)) sz ar1 s2
          else
            let s3 = writeStrictAfArray ar1 0# (i - 1) s2
            in (# ar1, s3, (# e | #) #)
    (# ar1, s1, (# | a #) #) ->
      unAf (f a) sz ar1 s1


{-# INLINE takeShortcut #-}
takeShortcut :: forall ref sh efs a. In (Shortcut sh ref) efs => sh -> Af efs a
takeShortcut sh = Af $ \ _ ar s ->
  let d = shortcutDepth @(Shortcut sh ref) @efs
      s' = writeStrictAfArray @Int ar 0# d s
  in (# ar, s', (# unsafeCoerce sh | #) #)



{-# INLINE scopeShortcut #-}
scopeShortcut ::
  forall ref sh efs a b. In (Shortcut sh ref) efs =>
  Af efs a ->           -- Effect computation to execute in scope.
  (a -> Af efs b) ->    -- Action when the shortcut is not taken.
  (sh -> Af efs b) ->   -- Action when the shortcut is taken.
  Af efs () ->          -- Action used when a shortcut of another
                        -- shortcut effect is taken.
  Af efs b
scopeShortcut af f g r = Af $ \ sz ar0 s0 ->
  let ix = cellIndex @(Shortcut sh ref) @efs sz in
  case copyFromAfArray ar0 ix sz s0 of
    (# s1, backup #) ->
      case unAf af sz ar0 s1 of
        (# ar1, s2, (# e | #) #) ->
          case readAfArray @Int ar1 0# s2 of
            (# s3, i #) ->
              if i == shortcutDepth @(Shortcut sh ref) @efs
              then
                let s4 = copyToAfArray backup ar1 ix s3
                in unAf (g (unsafeCoerce e)) sz ar1 s4
              else
                let !(# ar2, s4, _ #) = unAf r sz ar1 s3
                in (# ar2, s4, (# e | #) #)
        (# ar1, s2, (# | a #) #) ->
          unAf (f a) sz ar1 s2
