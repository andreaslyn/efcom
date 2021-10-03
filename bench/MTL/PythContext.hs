module MTL.PythContext
  ( runPythTriplesContext
  ) where

import Control.Monad

import GHC.Exts (Int#, (==#), (+#), runRW#, State#)


newtype Prompt = Prompt Int#


newtype Array = Array Int


eqPrompt :: Prompt -> Prompt -> Bool
eqPrompt (Prompt p1) (Prompt p2) =
  case p1 ==# p2 of
    0# -> False
    _ -> True


data ContMap :: (* -> *) -> * -> * -> * where
  BindCont ::
    forall ha a b c.
    (b -> Cont ha c) -> ContMap ha a b -> ContMap ha a c
  RunCont ::
    forall ha a b c.
    (Cont ha b -> c) -> ContMap ha a b -> ContMap ha a c
  ResetCont ::
    forall ha a b c.
    (Cont ha b -> Cont ha c) -> ContMap ha a b -> ContMap ha a c
  IdCont :: forall ha a. ContMap ha a a


contMap :: ContMap ha a b -> a -> Cont ha b
contMap IdCont a = return a
contMap (BindCont f IdCont) a = f a
contMap (BindCont f k) a = contMap k a >>= f
contMap (RunCont f k) a = return $ f (contMap k a)
contMap (ResetCont f k) a = f (contMap k a)


_contMap1 :: ContMap ha a b -> Cont ha a -> Cont ha b
_contMap1 IdCont m = m
_contMap1 (BindCont f k) m = _contMap1 k m >>= f
_contMap1 (RunCont f k) m = return $ f (_contMap1 k m)
_contMap1 (ResetCont f k) m = f (_contMap1 k m)


data Yield (ha :: * -> *) (a :: *) =
  Pure a | forall ans. Yield Prompt (ha ans) (ContMap ha ans a)


data Context = Context Prompt Array


newtype Cont (ha :: * -> *) (a :: *) = Cont
  { unCont :: forall s. State# s -> Context -> (# State# s, Yield ha a #) }


instance Monad (Cont ha) where
  return a = Cont $ \ s _ -> (# s, Pure a #)
  ct >>= f = Cont $ \ s0 ctx -> case unCont ct s0 ctx of
    (# s1, Pure a #) -> unCont (f a) s1 ctx
    (# s1, Yield pt ha k #) -> (# s1, Yield pt ha (BindCont f k) #)


instance Functor (Cont ha) where
  fmap = liftM


instance Applicative (Cont ha) where
  pure = return
  (<*>) = ap



type Handler (ha :: * -> *) (ans :: *) =
  forall x. ha x -> ContMap ha x ans -> Cont ha ans


runCont :: Handler ha ans -> (a -> ans) -> Cont ha a -> ans
runCont h fin = \ ct -> case runRW# (unCont ct) (Context (Prompt 0#) (Array 0)) of
  (# _, Pure a #) -> fin a
  (# _, Yield _ ha k #) ->
    runCont h id (h ha (RunCont (runCont h fin) k))


setCont :: Handler ha ans -> (a -> ans) -> (Prompt -> Cont ha a) -> Cont ha ans
setCont h fin = \ ct -> Cont $ \ s (Context (Prompt pt) ar) ->
  unCont (resetCont h fin (Prompt pt) (ct (Prompt pt))) s (Context (Prompt (pt +# 1#)) ar)


resetCont :: Handler ha ans -> (a -> ans) -> Prompt -> Cont ha a -> Cont ha ans
resetCont h fin pt = \ ct -> Cont $ \ s0 ctx ->
  case unCont ct s0 ctx of
    (# s1, Pure a #) -> (# s1, Pure (fin a) #)
    (# s1, Yield qt ha k #) ->
      if eqPrompt pt qt
      then unCont (h ha (ResetCont (resetCont h fin pt) k)) s1 ctx
      else (# s1, Yield qt ha (ResetCont (resetCont h fin pt) k) #)


data ListOps a = Empty | Choose ![a]


{-# INLINE empty #-}
empty :: Prompt -> Cont ListOps a
empty pt = Cont $ \ s _ -> (# s, Yield pt Empty IdCont #)


{-# INLINE choose #-}
choose :: Prompt -> [Integer] -> Cont ListOps Integer
choose pt is = Cont $ \ s _ -> (# s, Yield pt (Choose is) IdCont #)


{-# NOINLINE pythTriples #-}
pythTriples :: Prompt -> [Integer] -> Cont ListOps (Integer, Integer, Integer)
pythTriples pt ns = do
  x <- choose pt ns
  y <- choose pt ns
  z <- choose pt ns
  if x*x + y*y == z*z
  then return (x, y, z)
  else empty pt


handler :: Handler ListOps [r]
handler Empty _ = return []
handler (Choose as) k = chooseAll as []
  where
    chooseAll [] acc = return $! foldr (flip (++)) [] acc
    chooseAll (a : as') acc = do
      !a' <- contMap k a
      chooseAll as' $! a' : acc


{-# NOINLINE runPythTriplesContext #-}
runPythTriplesContext :: Integer -> [(Integer, Integer, Integer)]
runPythTriplesContext n =
  runCont handler id (setCont handler return (\ pt -> pythTriples pt [1..n]))
