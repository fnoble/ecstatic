-- TODO
--
-- struct sizes
-- follow type defs
--  - pass globals to sizeof
--
-- C stdlib
--
-- hash_map cache
--
-- merge globals from list of files?
--
-- clean up ' fns
--
-- UI:
--  config file?
--  REPL interface / DOT
--
-- TODO BUGS
--
-- simplify conditionals

{-# LANGUAGE ScopedTypeVariables #-}

module Development.Ecstatic.StackUsage where

import Development.Ecstatic.Utils
import qualified Development.Ecstatic.Simplify as S

import Language.C
import Language.C.Pretty
import Language.C.Data.Ident
import Language.C.Analysis
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import System.Console.ANSI
import Control.Monad
import Text.Printf
import qualified Text.PrettyPrint as PP
import Debug.Trace

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

applyAssumptions :: [Assumption] -> CExpr -> CExpr
applyAssumptions assumptions e = foldr sub e assumptions
  where sub :: (String, Integer) -> CExpr -> CExpr
        sub (s, x) expr = subByName s ((fromInteger x)::CExpr) expr

doStackUsage :: GlobalDecls -> CStat -> IO CExpr
doStackUsage g s = do
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "Stack Usage:"
  let su = stackUsage g s
  --    s1 = PP.render . pretty . simplify $ su
      su_string = PP.render . pretty . S.simplify $ su
  print . pretty $ su
  --appendFile "su1.txt" (s1 ++ "\n")
  writeFile "amb_su.txt" (su_string ++ "\n")
  setSGR [Reset]

  return su

doStackUsage' :: GlobalDecls -> CStat -> IO CallGraph
doStackUsage' g s = do
  let su = stackUsage' g s
  putStrLn $ ppCallGraph "" su
  return su

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
          


data CallGraph = CG { localStack :: CExpr
                    , totalStack :: CExpr
                    , children :: [(String, CallGraph)] }
  deriving (Show)

stackUsage' :: GlobalDecls -> CStat -> CallGraph
stackUsage' g s =
  -- Simple stack usage model:
  -- usage = size of automatic vars + stack usage of function calls

  --S.simplify $ applyAssumptions assumptions $ vars_size + func_call_size
  CG (S.simplify vars_size) (stackUsage g s) func_calls

  where
    -- Total size of all the local variables
    --
    vars_size = sum [sizeOfDecl d | d :: CDecl <- universeBi s]

    -- Total of function calls
    func_calls = [f func args | CCall func args _ :: CExpr <- universeBi s]

    f :: CExpr -> [CExpr] -> (String, CallGraph)
    f c@(CVar id@(Ident name _ _) _) args =
      case funcStackUsage' g id args of
        Nothing -> (name, CG c c [])
          --error $ "Call to unknown function: " ++ (show $ pretty id)
        Just e -> (name, e)
    f _ _ = error "Call to non-constant function?"

mapSnd f (a, b) = (a, f b)

subInCallGraph :: Ident -> CExpr -> CallGraph -> CallGraph
subInCallGraph id e (CG e1 t1 cs) =
  CG (substitute id e e1) 
     (substitute id e t1) 
     $ map (mapSnd (subInCallGraph id e)) cs

funcStackUsage' :: GlobalDecls -> Ident -> [CExpr] -> Maybe CallGraph
funcStackUsage' g i args = M.lookup i (gObjs g) >>= f
  where f :: IdentDecl -> Maybe CallGraph
        f (FunctionDef (FunDef
            (VarDecl _ _ (FunctionType (FunType _ params _) _))
            s _)) =
          Just $ foldr sub (stackUsage' g s) subs
            where
              ids = [id | (ParamDecl (VarDecl (VarName id _) _ _) _) <- params]
              subs = zip ids args
              sub (id, arg) stmt = subInCallGraph id arg stmt
        -- Not a function definition!?
        f _ = Nothing

stackUsage :: GlobalDecls -> CStat -> CExpr
stackUsage g s =
  -- Simple stack usage model:
  -- usage = size of automatic vars + stack usage of function calls

  --S.simplify $ applyAssumptions assumptions $ vars_size + func_call_size
  S.simplify $ vars_size + func_call_size

  where
    -- Total size of all the local variables
    --
    vars_size = sum [sizeOfDecl d | d :: CDecl <- universeBi s]

    -- Total of function calls
    func_call_size = sum [f func args | CCall func args _ :: CExpr <- universeBi s]

    f :: CExpr -> [CExpr] -> CExpr
    f c@(CVar id _) args =
      case funcStackUsage g id args of
        Nothing -> c
          --error $ "Call to unknown function: " ++ (show $ pretty id)
        Just e -> e
    f _ _ = error "Call to non-constant function?"

funcStackUsage :: GlobalDecls -> Ident -> [CExpr] -> Maybe CExpr
funcStackUsage g i args = M.lookup i (gObjs g) >>= f
  where f :: IdentDecl -> Maybe CExpr
        f (FunctionDef (FunDef
            (VarDecl _ _ (FunctionType (FunType _ params _) _))
            s _)) =
          Just $ foldr sub (stackUsage g s) subs
            where
              ids = [id | (ParamDecl (VarDecl (VarName id _) _ _) _) <- params]
              subs = zip ids args
              sub (id, arg) stmt = substitute id arg stmt
        -- Not a function definition!?
        f _ = Nothing

sizeOfDecl :: CDecl -> CExpr
-- Empty list corresponds to type used outside a declaration?
-- e.g. sizeof(double)
sizeOfDecl (CDecl _ [] _) = 0
sizeOfDecl (CDecl ds ((d,i,e):vs) n1) =
  f [ts | CTypeSpec ts <- ds] * maybe (fromInteger 1) modifier d
    + sizeOfDecl (CDecl ds vs n1)
  where f :: [CTypeSpec] -> CExpr
        f [ts] = fromInteger $ sizeOf ts
        -- "Implicit int rule", should never occur in C99
        f []   = fromInteger $ sizeOf (CIntType undefined)
        f _    = error "Declaration with more than one type specifier?"

        modifier (CDeclr _ [] _ _ _) = (fromInteger 1)
        modifier (CDeclr id (dd:dds) sl a n2) =
          modifier' dd * modifier (CDeclr id dds sl a n2)

        modifier' (CArrDeclr _ (CNoArrSize _) _) = error "Unknown array size"
        modifier' (CArrDeclr _ (CArrSize _ sz) _) = sz
        modifier' _ = fromInteger 1

-- TODO: Check these for our platform!!
sizeOf :: CTypeSpecifier a -> Integer
sizeOf (CVoidType _) = 0
sizeOf (CCharType _) = 1
sizeOf (CShortType _) = 2
sizeOf (CIntType _) = 4
sizeOf (CLongType _) = 8
sizeOf (CFloatType _) = 4
sizeOf (CDoubleType _) = 8
sizeOf (CSignedType _) = 4
sizeOf (CUnsigType _) = 4
sizeOf (CBoolType _) = 1
sizeOf (CComplexType _) = 16
sizeOf (CEnumType _ _) = 4
sizeOf (CTypeOfExpr _ _) = error "Unsupported type: typeof()"
sizeOf (CTypeOfType _ _) = error "Unsupported type: typeof()"
{-
sizeOf (CSUType (CStruct CStructTag _ decl _ _) _) =
  case decl of
    Nothing -> 0
    Just ds -> sum $ map sizeOfDecl ds
sizeOf (CSUType (CStruct CUnionTag _ decl _ _) _) =
  case decl of
    Nothing -> 0
    Just ds -> maximum $ map sizeOfDecl ds
-}

-- TODO: Should really find all the typedefs and calculate the size based on
-- the mapped type, for now just match known common typedefs.
-- HINT - we can pull the typedefs out of GlobalDecls
sizeOf (CTypeDef (Ident "u64" _ _) _) = 8
sizeOf (CTypeDef (Ident "s64" _ _) _) = 8
sizeOf (CTypeDef (Ident "u32" _ _) _) = 4
sizeOf (CTypeDef (Ident "s32" _ _) _) = 4
sizeOf (CTypeDef (Ident "u16" _ _) _) = 2
sizeOf (CTypeDef (Ident "s16" _ _) _) = 2
sizeOf (CTypeDef (Ident "u8" _ _) _) = 1
sizeOf (CTypeDef (Ident "s8" _ _) _) = 1
sizeOf (CTypeDef (Ident "integer" _ _) _) = 4
sizeOf (CTypeDef (Ident "systime_t" _ _) _) = 4
-- TODO
sizeOf (CTypeDef _ _) = 22
sizeOf (CTypeDef (Ident s _ _) _) = error $ "Unknown typedef: " ++ s

