module Control.Af.Effect
  ( Effect
  , ApplyEffect
  , MeetEffect
  , IOE
  , STE
  , StateE
  , ExceptE
  ) where


type family Effect (e :: * -> *) :: [* -> *]


type ApplyEffect (e :: * -> *) (i :: *) = ApplyAll (Effect e) (e i)


type MeetEffect (e :: * -> *) (i :: *) (es :: [*])
  = Append (ApplyEffect e i) es


type family ApplyAll (fs :: [* -> *]) (x :: *) :: [*] where
  ApplyAll '[] x = '[]
  ApplyAll (f : fs) x = f x : ApplyAll fs x


type family Append (ds :: [*]) (es :: [*]) :: [*] where
  Append '[] es = es
  Append (d : ds) es = d : Append ds es


data IOE :: *

data STE (s :: *) :: *

data StateE (state :: *) (label :: *) :: *

data ExceptE (except :: *) (label :: *) :: *
