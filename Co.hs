module Co where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Data.Char

-- Makes dual typeclass from given name
mkDualC :: Name -> Q [Dec]
mkDualC name = do
    dec <- extractDec <$> reify name
    return . map (stripConstraints . flipArrows . coNames name) $ [dec]

-- Makes dual datatypes from given name
mkDualT :: Name -> Q [Dec]
mkDualT name = do
    dec <- extractDec <$> reify name
    return . map (flipConstructor . coNames name) $ [dec]

-- extracts the declaration from a reified name
extractDec :: Info -> Dec
extractDec (ClassI d _) = d
extractDec (TyConI d) = d
extractDec _ = error "not a supported type of structure"

-- puts a "co" or "Co" in front of every name
coNames :: Name -> Dec -> Dec
coNames n (ClassD cxt name vars deps funs) =
    ClassD cxt (prefixName "Co" name) vars deps (map (coNames n) funs)
coNames n (SigD name typ) = SigD (prefixName "co" name) (coTypes n typ)
coNames n (DataD cxt1 name vars k cons cxt2) =
    DataD cxt1 (prefixName "Co" name) vars k (map (coCons n) cons) cxt2
coNames n x = x

-- Helps coNames with Type data
coTypes :: Name -> Type -> Type
coTypes n (ForallT vars cxt t) = ForallT vars (map (coTypes n) cxt) t
coTypes n (AppT t1 t2) = AppT (coTypes n t1) (coTypes n t2)
coTypes n (SigT t1 t2) = SigT (coTypes n t1) (coTypes n t2)
coTypes n (ConT t) = ConT $ if t == n then prefixName "Co" t else t
coTypes n t = t

-- Helps coNames with Con data
coCons :: Name -> Con -> Con
coCons n (NormalC name types) = NormalC (prefixName "Co" name) types
coCons n (RecC name types) = RecC (prefixName "Co" name) types
coCons n (InfixC type1 name type2) = InfixC type1 (prefixName "Co" name) type2
coCons n (ForallC types cxt con) = ForallC types cxt (coCons n con)
coCons n (GadtC names btypes t) = GadtC (map (prefixName "Co") names) btypes t
coCons n (RecGadtC names btypes t) = RecGadtC (map (prefixName "Co") names) btypes t

-- prefixes name with string; use for types and constructors
prefixName :: String -> Name -> Name
suffixName :: String -> Name -> Name
prefixName s n = mkName $ s ++ nameBase n
suffixName s n = mkName $ nameBase n ++ s

-- Dualize declarations
flipArrows :: Dec -> Dec
flipArrows (ClassD cxt name vars deps funs) =
    ClassD cxt name vars deps (map flipArrows funs)
flipArrows (SigD name typ) = SigD name (flipType typ)
flipArrows x = x

-- Dualize the type signature of typeclass functions
flipType :: Type -> Type
flipType (AppT (AppT ArrowT t1) t2) = AppT (AppT ArrowT (flipType t2)) (flipType t1)
flipType (ForallT vs c t) = ForallT vs c (flipType t)
flipType (AppT t1 t2) = AppT (flipType t1) (flipType t2)
flipType (SigT t k) = SigT (flipType t) k
flipType t = t

-- Dualize a data constructor
flipConstructor :: Dec -> Dec
flipConstructor (DataD cxt1 name vars k cons cxt2) =
    DataD cxt1 name vars k (flipCons 1 cons) cxt2

-- Helps flipConstructor with Con data
flipCons :: Int -> [Con] -> [Con]
flipCons _ [] = []
flipCons n [NormalC name btypes@((b,t):bts)] = zipWith NormalC newNames newlist
    where
        totalf (AppT t _) = 1 + totalf t
        totalf _ = 0
        total = totalf t
        names = replicate (total+1) name
        nums  = [n .. n+total]
        newNames = zipWith suffixName (map show nums) names
        newlist = map (:[]) (concatMap flipBangType btypes)

flipCons _ _ = undefined

flipBangType :: BangType -> [BangType]
flipBangType (bang, typ) = [(bang, typ') | typ' <- expandType typ]

-- Reads types to determine structure of constructor
expandType :: Type -> [Type]
expandType (AppT t1 t2) = expandType t1 ++ expandType t2
expandType (TupleT _) = []
expandType t = [t]

-- strip bogus constraint while dualizing
stripConstraints :: Dec -> Dec
stripConstraints (ClassD cxt name vars deps funs) =
    ClassD cxt name vars deps (map stripConstraints funs)
stripConstraints (SigD n t) = SigD n (stripCons t)
stripConstraints x = x

-- helps stripConstraints to with Types
stripCons :: Type -> Type
stripCons (ForallT vars cxt typ) = typ
stripCons t = t
