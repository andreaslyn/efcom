module Control.Af.Internal.Effect
  ( Effect
  , ApplyEffect
  , MeetEffect
  , IOE
  , STE
  , StateE
  , ExceptE
  ) where


type family Effect (e :: *) :: [* -> *]


type ApplyEffect (e :: *) = ApplyAll (Effect e) e


type MeetEffect (e :: *) (es :: [*])
  = Append (ApplyEffect e) es


type family ApplyAll (fs :: [* -> *]) (x :: *) :: [*] where
  ApplyAll '[] x = '[]
  ApplyAll (f : fs) x = f x : ApplyAll fs x


type family Append (ds :: [*]) (es :: [*]) :: [*] where
  Append '[] es = es
  Append (d : ds) es = d : Append ds es


data IOE :: *

data STE (st :: *) :: *

data StateE (state :: *) (tag :: *) :: *

data ExceptE (except :: *) (tag :: *) :: *
