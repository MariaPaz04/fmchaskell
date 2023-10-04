module ListNat where

import Prelude hiding ((>),take, drop, enumFromTo, length, sum, product, elem, (++),rem, reverse,(+), (*), (^), Bool, True, False)
import Nat
import Bool
import Ordering

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
product [] = (S O)
product (n : ns) = n * product ns

(++) :: ListNat -> ListNat -> ListNat
(++) [] ms = ms
(++) (n : ns) ms = n : (ns ++ ms)

reverse :: ListNat -> ListNat
reverse [] = []
reverse (n : ns) = (reverse ns) ++ [n]

allEven :: ListNat -> Bool
allEven [] = True
allEven (n : ns) = if_then_else_2 (ev n) (allEven ns) False

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (n : ns) = if_then_else_2 (ev n) True (anyEven ns)

allOdd :: ListNat -> Bool
allOdd [] = True
allOdd (n : ns) = if_then_else_2 (od n) (allOdd ns) False

anyOdd :: ListNat -> Bool
anyOdd [] = False
anyOdd (n : ns) = if_then_else_2 (od n) True (anyOdd ns)

allZero :: ListNat -> Bool
allZero [] = True
allZero (n : ns) = if_then_else_2 (isZero n) (allZero ns) False

anyZero :: ListNat -> Bool
anyZero [] = False
anyZero (n : ns) = if_then_else_2 (isZero n) True (anyZero ns)

addNat :: Nat -> ListNat -> ListNat
addNat m [] = []
addNat m (n : ns) = (m + n):(addNat m ns)

multNat :: Nat -> ListNat -> ListNat
multNat m [] = []
multNat m (n : ns) = (m * n):(multNat m ns)

expNat :: Nat -> ListNat -> ListNat
expNat m [] = []
expNat m (n : ns) = (n ^ m):(expNat m ns)

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo m n
  | m > n = []
  | otherwise = m : enumFromTo (S m) n

enumTo :: Nat -> ListNat
enumTo = enumFromTo O
 
take :: Nat -> ListNat -> ListNat
take (S m) (n : ns) = n : take m ns
take _ _ = []

drop :: Nat -> ListNat -> ListNat
drop _ [] = []
drop O ns = ns
drop (S m) (n : ns) = drop m ns

elemIndices :: Nat -> ListNat -> ListNat
elemIndices _ [] = []
elemIndices m (n : ns)
  |n == m = O : ns'
  |otherwise = ns'
    where
        ns' = addNat (S O) (elemIndices n ns)

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd (n : ns) (m : ms) = (n + m): pwAdd ns ms
pwAdd _ _ = []

pwMult :: ListNat -> ListNat -> ListNat
pwMult (n : ns) (m : ms) = (n * m): pwMult ns ms
pwMult _ _ = []