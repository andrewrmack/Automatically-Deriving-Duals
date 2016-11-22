module Co where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Data.Char

{-
monadDef =
    ClassD [] Test.Monad [KindedTV m (AppT (AppT ArrowT StarT) StarT)] []
       [SigD Test.unit
           (ForallT
               [KindedTV m (AppT (AppT ArrowT StarT) StarT)]
               [AppT (ConT Test.Monad) (VarT m)]
               (ForallT [KindedTV a StarT] []
                   (AppT (AppT ArrowT (VarT a))
                         (AppT (VarT m) (VarT a))
                   )
               )
           ),
        SigD Test.join
            (ForallT
                [KindedTV m (AppT (AppT ArrowT StarT) StarT)]
                [AppT (ConT Test.Monad) (VarT m)]
                (ForallT [KindedTV a StarT] []
                    (AppT (AppT ArrowT (AppT (VarT m) (AppT (VarT m) (VarT a))))
                    (AppT (VarT m) (VarT a)))))]
-}

-- Makes dual typeclass from given name
mkDuals :: Name -> Q [Dec]
mkDuals name = do
    dec <- extractDec <$> reify name
    return . map (stripConstraints . flipArrows . coNames name) $ [dec]

-- extracts the declaration from a reified name
extractDec :: Info -> Dec
extractDec (ClassI d _) = d
extractDec _ = error "not a supported type of structure"

-- puts a "co" or "Co" in front of every name
coNames :: Name -> Dec -> Dec
coNames n (ClassD cxt name vars deps funs) =
    ClassD cxt (prefixName "Co" name) vars deps (map (coNames n) funs)
coNames n (SigD name typ) = SigD (prefixName "co" name) (coTypes n typ)
coNames n x = x

coTypes :: Name -> Type -> Type
coTypes n (ForallT vars cxt t) = ForallT vars (map (coTypes n) cxt) t
coTypes n (AppT t1 t2) = AppT (coTypes n t1) (coTypes n t2)
coTypes n (SigT t1 t2) = SigT (coTypes n t1) (coTypes n t2)
coTypes n (ConT t) = ConT $ if t == n then prefixName "Co" t else t
coTypes n t = t

-- prefixes name with string; use for types and constructors
prefixName :: String -> Name -> Name
prefixName s n = mkName $ s ++ nameBase n

-- Dualize declarations
flipArrows :: Dec -> Dec
flipArrows (ClassD cxt name vars deps funs) = ClassD cxt name vars deps (map flipArrows funs)
flipArrows (SigD name typ) = SigD name (flipType typ)
flipArrows x = x

-- Dualize the type signature of typeclass functions
flipType :: Type -> Type
flipType (AppT (AppT ArrowT t1) t2) = AppT (AppT ArrowT (flipType t2)) (flipType t1)
flipType (ForallT vs c t) = ForallT vs c (flipType t)
flipType (AppT t1 t2) = AppT (flipType t1) (flipType t2)
flipType (SigT t k) = SigT (flipType t) k
flipType t = t

{-
 - For some reason, the reified typeclass declarations get a bunch of extra costraints
 - which result in error when I try to dualize. With monad, I get the following type for counit
 -
 - counit :: forall m. CoMonad m => forall m. Monad m => forall a. m a -> a
 -
 - This function strips the broken constraints
 -}

stripConstraints :: Dec -> Dec
stripConstraints (ClassD cxt name vars deps funs) =
    ClassD cxt name vars deps (map stripConstraints funs)
stripConstraints (SigD n t) = SigD n (stripCons t)
stripConstraints x = x

-- helps stripConstraints to with Types
stripCons :: Type -> Type
stripCons (ForallT vars cxt typ) = ForallT vars [] (stripCons typ)
stripCons t = t
