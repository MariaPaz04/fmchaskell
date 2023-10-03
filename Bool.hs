module Bool where

import Prelude hiding (Bool, True, False)

data Bool = False | True
    deriving ( Eq , Show )

if_then_else_2 :: Bool -> Bool -> Bool -> Bool
if_then_else_2 True n _ = n
if_then_else_2 False _ m = m