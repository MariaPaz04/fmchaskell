module ListNat where

import Prelude hiding (last,init,tail,min, max,maximum, minimum,(<=),(>),take, drop, enumFromTo, length, sum, product, elem, (++),rem, reverse,(+), (*), (^))
import Nat
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
allEven (n : ns) = ev n && allEven ns

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (n : ns) = ev n || anyEven ns

allOdd :: ListNat -> Bool
allOdd [] = True
allOdd (n : ns) = od n && allOdd ns

anyOdd :: ListNat -> Bool
anyOdd [] = False
anyOdd (n : ns) = od n || anyOdd ns

allZero :: ListNat -> Bool
allZero [] = True
allZero (n : ns) = isZero n && allZero ns

anyZero :: ListNat -> Bool
anyZero [] = False
anyZero (n : ns) = isZero n || anyZero ns

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

isSorted :: ListNat -> Bool
isSorted (n : (m : ms)) = if (n <= m) then (isSorted (m : ms)) else False
isSorted _ = True

filterOdd :: ListNat -> ListNat
filterOdd [] = []
filterOdd (n : ns) = if od n then n : filterOdd ns else filterOdd ns

filterEven :: ListNat -> ListNat
filterEven [] = []
filterEven (n : ns) = if ev n then n : filterEven ns else filterEven ns

minimum :: ListNat -> Nat
minimum [] = error "there is no minimum"
minimum [n] = n
minimum (n : ns) = min n (minimum ns)

maximum :: ListNat -> Nat
maximum [] = error "There is no maximum"
maximum [n] = n
maximum (n : ns) = max n (maximum ns)

isPrefixOf :: ListNat -> ListNat -> Bool
isPrefixOf [] _ = True
isPrefixOf (n : ns) (m : ms) = n == m && isPrefixOf ns ms
isPrefixOf _ _ = False

mix :: ListNat -> ListNat -> ListNat
mix [] ms = ms
mix ns [] = ns
mix (n : ns) (m : ms) = n : (m : mix ns ms)

intersperse :: Nat -> ListNat -> ListNat
intersperse n [] = []
intersperse n [m] =  [m]
intersperse n (m : ms) = m : (n : intersperse n ms)

head :: ListNat -> Nat
head [] = error "Empty list has no head."
head (n : _) = n

tail :: ListNat -> ListNat
tail [] =  error "Empty list has no tail."
tail (_ : ns) = ns

init :: ListNat -> ListNat
init [] = error "Empty list has no init."
init [_] = []
init (n : ns) = n : init ns 

last :: ListNat -> Nat
last [] = error "Empty list has no init."
last [n] = n
last (_ : ns) = last ns