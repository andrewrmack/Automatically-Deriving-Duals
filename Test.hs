{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module Test where

import Language.Haskell.TH
import Co
import Prelude hiding (Monad)

class Monad m where
    unit :: a -> m a
    join :: m (m a) -> m a

mkDuals ''Monad
