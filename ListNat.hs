module ListNat where
import Nat
import Bool

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )

