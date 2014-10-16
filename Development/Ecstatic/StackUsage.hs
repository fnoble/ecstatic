-- TODO
--
-- FILL IN TypeName sizes
--
-- C stdlib
--
-- hash_map cache
--
-- merge globals from list of files?
--
-- UI:
--  config file?
--  REPL interface / DOT
--
-- simplify conditional CExpr's

{-# LANGUAGE ScopedTypeVariables #-}

module Development.Ecstatic.StackUsage where

import Development.Ecstatic.Utils
import qualified Development.Ecstatic.Simplify as S

import Language.C
import Language.C.Data.Ident
import Language.C.Analysis
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import System.Console.ANSI
import qualified Text.PrettyPrint as PP

num_dds :: Integer
num_dds = 10

type Assumption = (String, Integer)

assumptions :: [Assumption]
assumptions = [("num_dds", num_dds),
               ("res_dim", 2*num_dds - 3),
               ("dd_dim", 2*num_dds),
               ("num_sats", num_dds+1),
               ("state_dim", num_dds),
               ("new_state_dim", num_dds),
               ("new_state_dim", num_dds),
               ("lwork", 22)]

applyAssumptions :: Expr -> CExpr
applyAssumptions e = foldr sub e assumptions
  where sub :: (String, Integer) -> CExpr -> CExpr
        sub (s, x) expr = subByName s ((fromInteger x)::CExpr) expr


data CallGraph = CG { localStack :: CExpr
                    , totalStack :: CExpr
                    , children   :: [(String, CallGraph)] }
  deriving (Show)

doStackUsage :: GlobalDecls -> CStat -> IO CExpr
doStackUsage g s = do
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "Stack Usage:"
  let su = stackUsage g s
      su_string = PP.render . pretty $ su
  putStrLn su_string
  setSGR [Reset]

  return su

printCallGraph :: GlobalDecls -> CStat -> IO CallGraph
printCallGraph g s = do
  let cg = callGraph g s
  putStrLn $ ppCallGraph "" cg
  return cg

ppCallGraph :: String -> CallGraph -> String
ppCallGraph prefix (CG ls ts calls) =
  init . unlines $ [prefix ++ "Local: " ++ (show $ pretty ls),
                    prefix ++ "Total: " ++ (show $ pretty ts)]
          ++ map ppBinding calls
 where
  ppName name =
    setSGRCode [SetColor Foreground Dull Blue] ++
    name ++ setSGRCode [Reset]

  ppBinding (name, call) =
    prefix ++ (ppName name) ++ ":\n" ++ ppCallGraph ((replicate 4 ' ') ++ prefix) call
          

stackUsage :: GlobalDecls -> CStat -> CExpr
stackUsage g = totalStack . callGraph g

callGraph :: GlobalDecls -> CStat -> CallGraph
callGraph g s =
  -- TODO don't recompute stack usage at every node
  -- (memoize)
  CG vars_size total_stack func_calls

  where
    total_stack = S.simplify $
      vars_size + sum (map (totalStack . snd) func_calls)
    -- Total size of all the local variables
    --
    vars_size = S.simplify $ sum [sizeOfDecl g d | d :: CDecl <- universeBi s]

    -- Total of function calls
    func_calls = [f func args | CCall func args _ :: CExpr <- universeBi s]

    f :: CExpr -> [CExpr] -> (String, CallGraph)
    f c@(CVar ident@(Ident name _ _) _) args =
      case funcStackUsage g ident args of
        Nothing -> (name, CG c c [])
          --error $ "Call to unknown function: " ++ (show $ pretty id)
        Just e -> (name, e)
    f _ _ = error "Call to non-constant function?"

subInCallGraph :: Ident -> CExpr -> CallGraph -> CallGraph
subInCallGraph ident e (CG e1 t1 cs) =
  CG (substitute ident e e1) 
     (substitute ident e t1) 
     $ map (mapSnd (subInCallGraph ident e)) cs

funcStackUsage :: GlobalDecls -> Ident -> [CExpr] -> Maybe CallGraph
funcStackUsage g i args = M.lookup i (gObjs g) >>= f
  where f :: IdentDecl -> Maybe CallGraph
        f (FunctionDef (FunDef
            (VarDecl _ _ (FunctionType (FunType _ params _) _))
            s _)) =
          Just $ foldr sub (callGraph g s) subs
            where
              ids = [ident | (ParamDecl (VarDecl (VarName ident _) _ _) _) <- params]
              subs = zip ids args
              sub (ident, arg) stmt = subInCallGraph ident arg stmt
        -- Not a function definition!?
        f _ = Nothing

sizeOfDecl :: GlobalDecls -> CDecl -> CExpr
-- Empty list corresponds to type used outside a declaration?
-- e.g. sizeof(double)
sizeOfDecl _ (CDecl _ [] _) = 0
sizeOfDecl g (CDecl ds ((d,_,_):vs) n1) =
  f [ts | CTypeSpec ts <- ds] * maybe (fromInteger 1) modifier d
    + sizeOfDecl g (CDecl ds vs n1)
  where f :: [CTypeSpec] -> CExpr
        f [ts] = sizeOf g ts
        -- "Implicit int rule", should never occur in C99
        f []   = sizeOf g (CIntType undefined)
        f _    = error "Declaration with more than one type specifier?"

        modifier (CDeclr _ [] _ _ _) = (fromInteger 1)
        modifier (CDeclr ident (dd:dds) sl a n2) =
          modifier' dd * modifier (CDeclr ident dds sl a n2)

        modifier' (CArrDeclr _ (CNoArrSize _) _) = error "Unknown array size"
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
sizeOf _ (CTypeOfExpr _ _) = error "Unsupported type: typeof()"
sizeOf _ (CTypeOfType _ _) = error "Unsupported type: typeof()"

-- TODO does not take into account packing of fields
sizeOf g (CSUType (CStruct CStructTag _ decl _ _) _) =
  case decl of
    Nothing -> 0
    Just ds -> 
      let s = sum $ map (sizeOfDecl g) ds
      in s
-- TODO add a call to MAX here
-- (need c function rather than maximum since return type is CExpr)
sizeOf _ ty@(CSUType (CStruct CUnionTag _ decl _ _) _) =
  error $ "unsupported type: " ++ show ty
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
-- TODO PtrType ArrayType FunctionType
sizeOfType _ ty = error $ "unsupported type: " ++ show ty

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
    -- TODO TyChar TySChar TyUChar TyShort
sizeOfTypeName _ (TyFloating ty) =
  case ty of
    TyFloat -> 4
    TyDouble -> 8
    TyLDouble -> 16 -- TODO ??
-- TODO
sizeOfTypeName _ ty = error $ "unsupported type: " ++ show ty

sizeOfMemberDecl :: GlobalDecls -> MemberDecl -> CExpr
sizeOfMemberDecl _ (AnonBitField{}) = 0 -- TODO
sizeOfMemberDecl g (MemberDecl (VarDecl _ _ ty) _ _) = sizeOfType g ty
