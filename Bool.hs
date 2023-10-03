module Bool where
import Nat
import ListNat

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )