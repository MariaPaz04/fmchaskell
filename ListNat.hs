module ListNat where

import Prelude hiding (ev, (<=), (>), last, init, max, maximum, minimum, min, drop, take, enumFromTo, (^), reverse, (++), (*), product, (+), sum, length, elem)
import Nat

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (n : ns) = S (length ns) 

elem :: Nat -> ListNat -> Bool
elem _ [] = False
elem m (n : ns) = if n == m then True else (elem m ns)

sum :: ListNat -> Nat
sum [] = O
sum (n : ns) = n + sum ns

product :: ListNat -> Nat
product [] = O
product (n : ns) = n * product ns

(++) :: ListNat -> ListNat -> ListNat
(++) [] ms = ms
(++) (n : ns) ms = n : (ns ++ ms)

reverse :: ListNat -> ListNat
reverse [] = []
reverse (n : ns) = (reverse ns) ++ [n]

allEven :: ListNat -> Bool
allEven [] = True
allEven (n : ns) = ev n && allEven ns
 where ev :: Nat -> Bool
       ev O = True
       ev (S O) = False
       ev (S (S n)) = ev n

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (n : ns) = ev n || allEven ns
 where ev :: Nat -> Bool
       ev O = True
       ev (S O) = False
       ev (S (S n)) = ev n
