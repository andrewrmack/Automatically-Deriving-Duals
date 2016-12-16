{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Test where

import Language.Haskell.TH
import Co

class Monad' m where
    unit :: a -> m a
    join :: m (m a) -> m a

data Product a b c d where
    Product :: (a,b,c,d) -> Product a b c d

data Sum a b where
    Sum1 :: a -> Sum a b
    Sum2 :: b -> Sum a b

data Id a where
    Id :: a -> Id a

mkDualC ''Monad'
mkDualT ''Product
