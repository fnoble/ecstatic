-- TODO now
-- - parse/input stack limit
-- - if function exceeds, print bad stack traces
--
--
-- TODO
-- figure out if track.c ambiguity matters
-- generate warnings
--   - for external functions, _max, local vars, function pointers
-- substitute function pointers
-- substitute local vars
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
import Development.Ecstatic.Types
import Development.Ecstatic.Utils
import Development.Ecstatic.Size
import qualified Development.Ecstatic.SimplifyDef as S
import Development.Ecstatic.SimplifyDef

import Language.C
import Language.C.Analysis
import Language.C.Data.Ident

import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import Data.Maybe
import Data.List (nub)
import System.Console.ANSI
import Control.Monad.State
import Debug.Trace (trace)

type StackMap = M.Map String CallGraph

evalStack :: State StackMap a -> a
evalStack m = evalState m M.empty

type Assumption = (String, Integer)

num_dds :: Integer
num_dds = 10
assumptions :: [Assumption]
assumptions = [("num_dds", num_dds),
               ("res_dim", 2*num_dds - 3),
               ("dd_dim", 2*num_dds),
               ("num_sats", num_dds+1),
               ("state_dim", num_dds),
               ("new_state_dim", num_dds),
               ("new_state_dim", num_dds),
               ("lwork", 22)]

cgMap f (CG l t cs) = CG (f l) (f t) (map (mapSnd (cgMap f)) cs)

--cgSimplify = transformBi S.simplify
cgSimplify = cgMap S.simplify
conjSimpl f = S.simplify . f . S.simplify

fullSimplExpr :: CExpr -> CExpr
fullSimplExpr = conjSimpl reduceConditionals . conjSimpl (transformBi (subAllNames 0))

reduceCG :: CallGraph -> CallGraph
reduceCG cg =
  cgMap fullSimplExpr $
    foldr sub cg assumptions
  where
    sub (s, x) = subByName s ((fromInteger x)::CExpr)

applyAssumptions :: CExpr -> CExpr
applyAssumptions e = foldr sub e assumptions
  where sub :: (String, Integer) -> CExpr -> CExpr
        sub (s, x) expr = subByName s ((fromInteger x)::CExpr) expr

makeMax :: CExpr -> CExpr -> CExpr
makeMax e1 e2 = CCond (CBinary CGrOp e1 e2 undefNode) (Just e1) e2 undefNode
makeMaximum :: [CExpr] -> CExpr
makeMaximum es = CCall (CVar (Ident "_max" (-1) undefNode) undefNode) es undefNode

defStackUsage :: GlobalDecls -> (CStat, [ParamDecl]) -> State StackMap CallGraph
defStackUsage g (s, params) = do
  fs <- sequence func_calls
  let total_stack =
        local_size +
        --(sum $
        (makeMaximum $ 
        --(foldr makeMax 0 $
          (map (totalStack . snd) fs))
  return $ reduceCG $ CG local_size total_stack fs
  where
    local_size = S.simplify $ vars_size + arg_size
    -- Total size of all the local variables
    vars_size = S.simplify $ sum [sizeOfDecl g d | d :: CDecl <- universeBi s]
    -- Total of function calls (monadic)
    func_calls = [callStackUsage g func args | CCall func args _ :: CExpr <- universeBi s]
    -- Total argument stack usage
    arg_size = S.simplify $ sum [sizeOfType g ty | ParamDecl (VarDecl _ _ ty) _ <- params]

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
    stackm <- get
    mval <- gets (M.lookup name)
    case mval of
      -- Value already available
      Just cg -> return cg
      Nothing ->
        case M.lookup ident (gObjs g) of
          -- Have global decl
          Just fd@(FunctionDef (FunDef vd def _))
               | Just params <- funParams vd -> do
            cg <- defStackUsage g (def, params)
            -- Record general value in StackMap
            addStackVal name cg
            -- Substitute arguments
            let ids = [id | (ParamDecl (VarDecl (VarName id _) _ _) _) <- params]
            return (subsInCallGraph (zip ids args) cg)
          -- Only have a declaration
          Just (Declaration (Decl (VarDecl (VarName ident' _) _ _) _)) -> 
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

subInCallGraph :: Ident -> CExpr -> CallGraph -> CallGraph
subInCallGraph ident e (CG e1 t1 cs) =
  CG (substitute ident e e1) 
     (substitute ident e t1) 
     $ map (mapSnd (subInCallGraph ident e)) cs
subsInCallGraph :: [(Ident, CExpr)] -> CallGraph -> CallGraph
subsInCallGraph subs cg = foldr (uncurry subInCallGraph) cg subs

-- Prints CallGraph, using ANSI color codes
ppCallGraph :: CallGraph -> String
ppCallGraph = ppCallGraph' ""
ppCallGraph' :: String -> CallGraph -> String
ppCallGraph' prefix (CG ls ts calls) =
  init . unlines $ [prefix ++ "Local: " ++ (ppStack ls),
                    prefix ++ "Total: " ++ (ppStack ts)]
          -- ++ map ppBinding calls
 where
  ppStack = show . pretty
  --ppStack s | Just n <- isPrim s = show n
  --ppStack _ = "[symbol]"
  ppName name =
    setSGRCode [SetColor Foreground Dull Blue] ++
    name ++ setSGRCode [Reset]
  ppBinding (name, call) =
    prefix ++ (ppName name) ++ ":\n" ++ ppCallGraph' ((replicate 4 ' ') ++ prefix) call

