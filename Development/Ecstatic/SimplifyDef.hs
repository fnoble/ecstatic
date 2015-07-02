{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Development.Ecstatic.SimplifyDef (
  simplify,
  isSum, isMul, isAtom, isPrim
) where
import Development.Ecstatic.Types()
import qualified Simplify as S
import Language.C
import qualified Data.Map.Strict as M

-- Exported function
simplify :: CExpr -> CExpr
simplify = simplify'
simplify' :: CExpr -> CExpr
simplify' = rebuild . (S.simplify expr_def)

-- CExpr builder
rebuild :: S.Poly CExpr Int -> CExpr
rebuild poly =
  case map toProd (M.toList poly) of
    [] -> 0
    ps -> foldl1 (+) ps
 where
  toProd :: ([(CExpr, Int)], Int) -> CExpr
  toProd (ts, coef) =
    let coef' = fromIntegral coef
    in
    case ts of
      [] -> coef'
      _ -> foldl1 (*) $
        ((if coef == (S.one expr_def) then [] else [coef']) ++
          concatMap (\(n, exp) -> replicate exp n) ts)

-- module Simplify definitions
expr_def :: S.Expr CExpr CExpr Int
expr_def = S.Expr
  { S.isSum = isSum
  , S.isMul = isMul
  , S.isAtom = isAtom
  , S.isPrim = isPrim
  , S.zero = 0
  , S.one = 1
  }

isSum :: CExpr -> Maybe [CExpr]
isSum (CBinary CAddOp t1 t2 _) = Just [t1, t2]
isSum (CBinary CSubOp t1 t2 _) =
  Just [t1, CUnary CMinOp t2 undefNode]
isSum _ = Nothing

isMul :: CExpr -> Maybe [CExpr]
isMul (CBinary CMulOp t1 t2 _) = Just [t1, t2]
isMul _ = Nothing

isAtom :: CExpr -> Maybe CExpr
isAtom (CBinary CMulOp _ _ _) = Nothing
isAtom (CBinary CAddOp _ _ _) = Nothing
isAtom (CConst (CIntConst (CInteger _ _ _) _)) = Nothing
isAtom t = Just t

isPrim' :: CExpr -> Maybe Int
isPrim' (CConst (CIntConst (CInteger number _ _) _)) = Just (fromIntegral number)
isPrim' _ = Nothing
isPrim :: CExpr -> Maybe Int
isPrim x | Just n <- isPrim' x = Just n
isPrim (CUnary CMinOp x _) | Just n <- isPrim' x = Just (-n)
isPrim _ = Nothing

deriving instance Ord CExpr
deriving instance Eq CExpr
deriving instance Ord (CBuiltinThing NodeInfo)
deriving instance Eq (CBuiltinThing NodeInfo)
deriving instance Ord (CPartDesignator NodeInfo)
deriving instance Eq (CPartDesignator NodeInfo)
deriving instance Ord (CConstant NodeInfo)
deriving instance Eq  (CConstant NodeInfo)
deriving instance Ord (CDeclaration NodeInfo)
deriving instance Eq (CDeclaration NodeInfo)
deriving instance Ord (CDeclarationSpecifier NodeInfo)
deriving instance Eq (CDeclarationSpecifier NodeInfo)
deriving instance Ord (CTypeQualifier NodeInfo)
deriving instance Eq (CTypeQualifier NodeInfo)
deriving instance Ord (CDeclarator NodeInfo)
deriving instance Eq (CDeclarator NodeInfo)
deriving instance Ord (CStatement NodeInfo)
deriving instance Eq (CStatement NodeInfo)
deriving instance Ord (CAssemblyStatement NodeInfo)
deriving instance Eq (CAssemblyStatement NodeInfo)
deriving instance Ord (CDerivedDeclarator NodeInfo)
deriving instance Eq (CDerivedDeclarator NodeInfo)
deriving instance (Ord (CInitializer NodeInfo))
deriving instance (Eq (CInitializer NodeInfo))
deriving instance (Ord (CTypeSpecifier NodeInfo))
deriving instance (Eq (CTypeSpecifier NodeInfo))
deriving instance (Ord (CCompoundBlockItem NodeInfo))
deriving instance (Eq (CCompoundBlockItem NodeInfo))
deriving instance (Ord (CAssemblyOperand NodeInfo))
deriving instance (Eq (CAssemblyOperand NodeInfo))
deriving instance (Ord (CArraySize NodeInfo))
deriving instance (Eq (CArraySize NodeInfo))
deriving instance (Ord (CAttribute NodeInfo))
deriving instance (Eq (CAttribute NodeInfo))
deriving instance (Ord (CEnumeration NodeInfo))
deriving instance (Eq (CEnumeration NodeInfo))
deriving instance (Ord (CFunctionDef NodeInfo))
deriving instance (Eq (CFunctionDef NodeInfo))
deriving instance (Ord (CStringLiteral NodeInfo))
deriving instance (Eq (CStringLiteral NodeInfo))
deriving instance (Ord (CStructureUnion NodeInfo))
deriving instance (Eq (CStructureUnion NodeInfo))
deriving instance (Ord (CStructTag))
