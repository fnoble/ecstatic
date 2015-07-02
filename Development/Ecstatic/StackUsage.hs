-- TODO
-- jenkins
-- check for missing assumptions
--
-- substitute local vars
--
-- figure out if track.c ambiguity matters
-- generate warnings
--   - for external functions, _max, local vars, function pointers
-- substitute function pointers
--
-- make a linker?
--
-- external functions: printf, ch*, blas
-- UI:
--  config file?
--  REPL interface / DOT?
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Development.Ecstatic.StackUsage where
import Development.Ecstatic.CallGraph
import Development.Ecstatic.Size
import Development.Ecstatic.SimplifyDef as S

import Language.C
import Language.C.Analysis
import Language.C.Data.Ident

import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Control.Monad.State

import Debug.Trace (trace)

type StackMap = M.Map String CallGraph

evalStack :: State StackMap a -> a
evalStack m = evalState m M.empty

makeMaximum :: [CExpr] -> CExpr
makeMaximum es = CCall (CVar (Ident "_max" (-1) undefNode) undefNode) es undefNode

defStackUsage :: GlobalDecls -> (CStat, [ParamDecl]) -> State StackMap CallGraph
defStackUsage g (s, params) = do
  fs <- sequence func_calls
  let total_stack =
        local_size +
        (makeMaximum $ 
          (map (totalStack . snd) fs))
  return $ reduceCG $ CG local_size total_stack fs
  where
    local_size = S.simplify $ vars_size + arg_size
    -- Total size of all the local variables
    vars_size = S.simplify $ sum [sizeOfDecl g d | d :: CDecl <- universeBi s]
    -- Total of function calls (monadic)
    func_calls = [callStackUsage g func args | CCall func args _ :: CExpr <- universeBi s]
    -- Total argument stack usage
    arg_size = S.simplify $ sum [sizeOfTypeAsArg g ty | ParamDecl (VarDecl _ _ ty) _ <- params]

sizeOfTypeAsArg _ (ArrayType ty _ _ _) = 4
sizeOfTypeAsArg g t = sizeOfType g t

funParams :: VarDecl -> Maybe [ParamDecl]
funParams (VarDecl _ _ (FunctionType (FunType _ params _) _)) = Just params
funParams _ = Nothing

callStackUsage :: GlobalDecls -> CExpr -> [CExpr] -> State StackMap (String, CallGraph)
callStackUsage g expr@(CVar ident _) args = do
  cg <- doCG
  return (name, cg)
 where
  name = identToString ident
  addStackVal :: String -> CallGraph -> State StackMap ()
  addStackVal name cg = modify (M.insert name cg)
  doCG = do
    mval <- gets (M.lookup name)
    case mval of
      -- Value already available
      Just cg -> return cg
      Nothing ->
        case M.lookup ident (gObjs g) of
          -- Have global decl
          Just (FunctionDef (FunDef vd def _))
               | Just params <- funParams vd -> do
            cg <- defStackUsage g (def, params)
            -- Record general value in StackMap
            addStackVal name cg
            -- Substitute arguments
            let ids = [id | (ParamDecl (VarDecl (VarName id _) _ _) _) <- params]
            return (subsInCallGraph (zip ids args) cg)
          -- Only have a declaration
          Just (Declaration (Decl (VarDecl (VarName _ _) _ _) _)) -> 
            returnVar -- TODO link somehow?
          -- Shouldn't happen:
          Just x -> error $ "callStackUsage. : " ++ show x
          -- Unknown function, return abstract size
          Nothing -> returnVar
  returnVar = do
    let cg = CG expr expr []
    addStackVal name cg
    return cg
-- TODO resolve function pointers
callStackUsage _ expr _ = return ("FN_PTR", CG expr expr [])
callStackUsage _ expr args = error $ "callStackUsage. :" ++ show expr ++ show args

