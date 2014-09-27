{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SubGroup where

import RootSystem
import Weyl


class (WeylGroup w e r rt) => SubWeylGroup sw w e r rt | sw -> w, sw -> e, sw -> r, sw -> rt where
    ambientGroup :: sw -> w
    subGenerators :: sw -> [e]

data Subgroup a = Sub a deriving(Eq,Ord)

instance (SubWeylGroup sw w e r rt) => WeylGroup (Subgroup sw) e r rt where
	one (Sub sw) = one $ ambientGroup $ sw
	generators (Sub sw) = subGenerators sw
	weylGroup = undefined

