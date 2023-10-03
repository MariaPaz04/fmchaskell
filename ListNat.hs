module ListNat where

import Prelude hiding(Nil, length, sum, product)
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )
