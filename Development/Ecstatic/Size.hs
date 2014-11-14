-- TODO
-- various TypeName sizes
-- struct packing
-- union typedefs
-- local static declarations
--
-- eventually: replace show with pretty
module Development.Ecstatic.Size where
import Development.Ecstatic.Utils()

import Language.C
import Language.C.Analysis
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

sizeOfDecl :: GlobalDecls -> CDecl -> CExpr
-- Empty list corresponds to type used outside a declaration?
-- e.g. sizeof(double)
sizeOfDecl _ (CDecl _ [] _) = 0
sizeOfDecl g (CDecl ds ((d,initializer,expr):vs) n1) =
  f [ts | CTypeSpec ts <- ds] * maybe (fromInteger 1) modifier d
    + sizeOfDecl g (CDecl ds vs n1)
  where f :: [CTypeSpec] -> CExpr
        f [ts] = sizeOf g ts
        -- "Implicit int rule", should never occur in C99
        f []   = sizeOf g (CIntType undefined)
        f [_, t2] = sizeOf g t2 -- TODO does this handle "unsigned [foo]"?
        f ts = error $ "Declaration with too many type specifiers?: " ++ show ts

        modifier (CDeclr _ [] _ _ _) = (fromInteger 1)
        modifier (CDeclr ident (dd:dds) sl a n2) =
          modifier' dd * modifier (CDeclr ident dds sl a n2)

        modifier' (CArrDeclr _ (CNoArrSize _) _) = 
          case initializer of
            Just (CInitList es _) -> fromIntegral $ length es
            Just (CInitExpr (CConst (CStrConst str _)) _) -> fromIntegral $ length $ getCString str
            _ -> error $ "Unknown array size at: " ++ show n1 ++ "\n" ++ show initializer ++ ":::" ++ show expr ++ "\n"
        modifier' (CArrDeclr _ (CArrSize _ sz) _) = sz
        modifier' _ = fromInteger 1

-- TODO: Check these for our platform!!
sizeOf :: GlobalDecls -> CTypeSpecifier NodeInfo -> CExpr
sizeOf _ (CVoidType _) = 0
sizeOf _ (CCharType _) = 1
sizeOf _ (CShortType _) = 2
sizeOf _ (CIntType _) = 4
sizeOf _ (CLongType _) = 8
sizeOf _ (CFloatType _) = 4
sizeOf _ (CDoubleType _) = 8
sizeOf _ (CSignedType _) = 4
sizeOf _ (CUnsigType _) = 4
sizeOf _ (CBoolType _) = 1
sizeOf _ (CComplexType _) = 16
sizeOf _ (CEnumType _ _) = 4
sizeOf _ (CTypeOfExpr _ _) = error "sizeOf. Unsupported type: CTypeOfExpr"
sizeOf _ (CTypeOfType _ _) = error "sizeOf. Unsupported type: CTypeOfType"

-- TODO does not take into account packing of fields
sizeOf g (CSUType (CStruct CStructTag _ decl _ _) _) =
  case decl of
    Nothing -> 0
    Just ds -> 
      let s = sum $ map (sizeOfDecl g) ds
      in s
-- TODO add a call to MAX here
sizeOf g ty@(CSUType (CStruct CUnionTag _ decl _ _) _) =
  case decl of 
    Just (x : _) ->
      sizeOfDecl g x -- TODO FIX
    Just [] -> 0
    _ -> error $ "sizeOf. CSUType. ???: " ++ show ty
--  case decl of
--    Nothing -> 0
--    Just ds -> maximum $ map sizeOfDecl ds

sizeOf g (CTypeDef ident _) = 
  case M.lookup ident (gTypeDefs g) of
    Nothing -> error $ "unknown typedef ident: " ++ show ident
    Just (TypeDef _ ty _ _) ->
      sizeOfType g ty

sizeOfType :: GlobalDecls -> Type -> CExpr
sizeOfType g t@(TypeDefType (TypeDefRef _ mtype _) _ _) =
  case mtype of
    Nothing -> error $ "unknown TypeRef: " ++ show t
    Just ty -> sizeOfType g ty
sizeOfType g (DirectType ty _ _) =
  sizeOfTypeName g ty
-- TODO FunctionType
sizeOfType _ (PtrType _ _ _) = ptrSize
 where
   ptrSize = 4
sizeOfType g (ArrayType ty (ArraySize _ expr) _ _) = sizeOfType g ty * expr
-- Assume unknown length array is "flexible member array" of struct
sizeOfType g (ArrayType ty (UnknownArraySize _) _ _) = sizeOfType g ty
sizeOfType _ ty = error $ "sizeOfType. unsupported type: " ++ show ty

-- TODO replace the big sizeof enumeration with a call to this?
sizeOfTypeName :: GlobalDecls -> TypeName -> CExpr
sizeOfTypeName g t@(TyComp (CompTypeRef ref _ _)) =
  case M.lookup ref (gTags g) of
    Nothing -> error $ "unknown TypeName: " ++ show t
    Just (CompDef (CompType _ _ decls _ _)) ->
      sum $ map (sizeOfMemberDecl g) decls
    -- TODO add value for this case
    Just (EnumDef{}) -> error "union types unsupported"
sizeOfTypeName _ (TyIntegral ty) =
  case ty of
    TyBool -> 1
    TyInt -> 4
    TyChar -> 1
    TySChar -> 1 -- TODO ?
    TyUChar -> 1
    TyShort -> 2 -- TODO ?
    _ -> error $ "sizeOfTypeName. unknown integral type: " ++ show ty
sizeOfTypeName _ (TyFloating ty) =
  case ty of
    TyFloat -> 4
    TyDouble -> 8
    TyLDouble -> 16 -- TODO ??
-- TODO currently returns 0
sizeOfTypeName g t@(TyEnum (EnumTypeRef ref _)) =
  case M.lookup ref (gTags g) of
    Nothing -> error $ "unknown TypeName: " ++ show t
    Just x -> trace ("sizeOfTypeName. TODO: " ++ show (pretty x)) $ 0
sizeOfTypeName _ ty = error $ "sizeOfTypeName. unsupported type: " ++ show ty

sizeOfMemberDecl :: GlobalDecls -> MemberDecl -> CExpr
sizeOfMemberDecl _ (AnonBitField{}) = 0 -- TODO
sizeOfMemberDecl g (MemberDecl (VarDecl _ _ ty) _ _) = sizeOfType g ty
