module MTL.PythNoPrompt
  ( runPythTriplesNoPrompt
  ) where

import Control.Monad


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
  Pure a | forall ans. Yield (ha ans) (ContMap ha ans a)


newtype Cont (ha :: * -> *) (a :: *) = Cont
  { unCont :: Yield ha a }


instance Monad (Cont ha) where
  return a = Cont (Pure a)
  ct >>= f = Cont $ case unCont ct of
    Pure a -> unCont (f a)
    Yield ha k -> Yield ha (BindCont f k)


instance Functor (Cont ha) where
  fmap = liftM


instance Applicative (Cont ha) where
  pure = return
  (<*>) = ap



type Handler (ha :: * -> *) (ans :: *) =
  forall x. ha x -> ContMap ha x ans -> Cont ha ans


runCont :: Handler ha ans -> (a -> ans) -> Cont ha a -> ans
runCont h fin = \ ct -> case unCont ct of
  Pure a -> fin a
  Yield ha k ->
    runCont h id (h ha (RunCont (runCont h fin) k))


data ListOps a = Empty | Choose ![a]


{-# INLINE empty #-}
empty :: Cont ListOps a
empty = Cont (Yield Empty IdCont)


{-# INLINE choose #-}
choose :: [Integer] -> Cont ListOps Integer
choose is = Cont (Yield (Choose is) IdCont)


{-# NOINLINE pythTriples #-}
pythTriples :: [Integer] -> Cont ListOps (Integer, Integer, Integer)
pythTriples ns = do
  x <- choose ns
  y <- choose ns
  z <- choose ns
  if x*x + y*y == z*z
  then return (x, y, z)
  else empty


concatAcc :: forall a. [a] -> [[a]] -> [a]
concatAcc acc [] = acc
concatAcc acc (as : ass) = concatAcc (as ++ acc) ass


{-# INLINE concatR #-}
concatR :: forall a. [[a]] -> [a]
concatR = concatAcc []


handler :: Handler ListOps [r]
handler Empty _ = return []
handler (Choose as) k = chooseAll as []
  where
    chooseAll [] acc = return $! concatR acc
    chooseAll (a : as') acc = do
      !a' <- contMap k a
      chooseAll as' $! a' : acc


{-# NOINLINE runPythTriplesNoPrompt #-}
runPythTriplesNoPrompt :: Integer -> [(Integer, Integer, Integer)]
runPythTriplesNoPrompt n =
  runCont handler return (pythTriples [1..n])
