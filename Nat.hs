module Nat where

import Prelude hiding ((<=), (<), (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem, (-), if_then_else, leq, eq, ev, od, isMul3, divides)
import Ordering

(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S(n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = (n * m) + n

(^) :: Nat -> Nat -> Nat
n ^ O = (S O)
n ^ (S m) = n * (n ^ m)

quot :: Nat -> Nat -> Nat
quot _ O = error "Zero cannot divide anything"
quot O _ = O
quot n m --Resolvido no Zulip
  | m < n      = S $ quot (n - m) m 
  | otherwise  =     quot (n - m) m

min :: Nat -> Nat -> Nat
min _ _ = O 
min (S m) (S n) = S (min m n)

gcd :: Nat -> Nat -> Nat
gcd n O = n 
gcd n m = gcd m (rem n m)

lcm :: Nat -> Nat -> (Nat, Nat)
lcm n m = div (n * m) (gcd n m)

--Feita em sala
div :: Nat -> Nat -> (Nat, Nat)
div _ O = error "Division by zero"
div n m 
  | n < m     = (O, n)
  | otherwise = (q'+ (S O), r')
      where 
        (q', r') = div (n - m) m

max :: Nat -> Nat -> Nat
max _ _ = O 
max (S n) (S m) = S (max n m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n 

rem :: Nat -> Nat -> Nat
rem O n = O
rem (S m) O = error "Nothing remainds from Zero"
rem n m
   | m < n     = S $ rem (n - m) m
   | otherwise =     rem (n - m) m

(-) :: Nat -> Nat -> Nat
(-) n O = n 
(-) n (S m) =  n - m

fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S (S n)) = (fib (S n)) + (fib n) 

fact :: Nat -> Nat 
fact O = (S O)
fact (S n) = (S n) * (fact n)

double :: Nat -> Nat 
double O = O
double (S n) = S(S(double n))

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ m = m

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S (S n)) = ev n

od :: Nat -> Bool
od O = False
od (S O) = True 
od (S (S n)) = od n

isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S O) = False
isMul3 (S (S O)) = False
isMul3 (S (S (S n))) = isMul3 n

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

divides ::  Nat -> Nat -> Bool
divides O O = True
divides O (S n) = False
divides m n = (rem m n) == O