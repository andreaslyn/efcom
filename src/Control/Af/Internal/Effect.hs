module Control.Af.Internal.Effect
  ( Effect
  , Effects
  , type (++)
  , IOE
  , STE
  , Cell
  , Escape
  ) where


type family Effect (e :: *) :: [* -> *]


type Effects (e :: *) = ApplyAll (Effect e) e


type family ApplyAll (fs :: [* -> *]) (x :: *) :: [*] where
  ApplyAll '[] x = '[]
  ApplyAll (f : fs) x = f x : ApplyAll fs x


type family (ds :: [*]) ++ (es :: [*]) :: [*] where
  '[] ++ es = es
  (d : ds) ++ es = d : ds ++ es


data IOE :: *

data STE (st :: *) :: *

data Cell (cell :: *) (ref :: *) :: *

data Escape (escape :: *) (ref :: *) :: *
