module Pure.Countdown
  ( runCountdownPut
  , runCountdownExc
  ) where


{-# NOINLINE countdownPut #-}
countdownPut :: Integer -> (Integer, Integer)
countdownPut n = do
  if n < 0
  then (n, n)
  else countdownPut (n - 1)


{-# INLINE runCountdownPut #-}
runCountdownPut :: Integer -> (Integer, Integer)
runCountdownPut = countdownPut


{-# NOINLINE countdownExc #-}
countdownExc :: Integer -> Either String (Integer, Integer)
countdownExc n = do
  if n <= 0
  then Left "what"
  else countdownExc (n - 1)


{-# INLINE runCountdownExc #-}
runCountdownExc :: Integer -> Either String (Integer, Integer)
runCountdownExc = countdownExc


{-# NOINLINE countdownCountupExc #-}
countdownCountupExc :: Integer -> Either String (Integer, Integer)
countdownCountupExc n =
  if n <= 0
  then Left "what"
  else fmap (\ (x, n') -> (x + 1, n')) (countdownCountupExc (n - 1))


{-# INLINE runCountdownCountupExc #-}
runCountdownCountupExc :: Integer -> Either String (Integer, Integer)
runCountdownCountupExc = countdownCountupExc
