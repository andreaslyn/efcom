module Pure.Countdown
  ( runCountdownPut
  , runCountdownExc
  ) where


{-# INLINABLE countdownPut #-}
countdownPut :: Int -> (Int, Int)
countdownPut n = do
  if n < 0
  then (n, n)
  else countdownPut (n - 1)


{-# INLINE runCountdownPut #-}
runCountdownPut :: Int -> (Int, Int)
runCountdownPut = countdownPut


{-# INLINABLE countdownExc #-}
countdownExc :: Int -> Either String (Int, Int)
countdownExc n = do
  if n <= 0
  then Left "what"
  else countdownExc (n - 1)


{-# INLINE runCountdownExc #-}
runCountdownExc :: Int -> Either String (Int, Int)
runCountdownExc = countdownExc
