-- TODO
-- function pointers
--
-- use linker maps
--
-- max over calls instead of sum
--
-- external functions: printf, ch*, blas
-- UI:
--  config file?
--  REPL interface / DOT
--
-- simplify conditional CExpr's
{-# LANGUAGE ScopedTypeVariables #-}

module Development.Ecstatic.StackUsage where

import Development.Ecstatic.Utils
import Development.Ecstatic.Size
import qualified Development.Ecstatic.SimplifyDef as S

import Language.C
import Language.C.Analysis
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Data.Maybe
import System.Console.ANSI
import Control.Monad.State
import Debug.Trace (trace)

data CallGraph = CG
  { localStack :: CExpr
  , totalStack :: CExpr
  , children   :: [(String, CallGraph)] }
  deriving (Show)

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

applyAssumptions :: Expr -> CExpr
applyAssumptions e = foldr sub e assumptions
  where sub :: (String, Integer) -> CExpr -> CExpr
        sub (s, x) expr = subByName s ((fromInteger x)::CExpr) expr

defStackUsage :: GlobalDecls -> (CStat, [ParamDecl]) -> State StackMap CallGraph
defStackUsage g (s, params) = do
  fs <- sequence func_calls
  let total_stack = S.simplify $
        local_size + sum (map (totalStack . snd) fs)
  return $ CG local_size total_stack fs
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

-- TODO remove trace calls
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
  init . unlines $ [prefix ++ "Local: " ++ (show $ pretty ls),
                    prefix ++ "Total: " ++ (show $ pretty ts)]
          -- ++ map ppBinding calls
 where
  ppName name =
    setSGRCode [SetColor Foreground Dull Blue] ++
    name ++ setSGRCode [Reset]
  ppBinding (name, call) =
    prefix ++ (ppName name) ++ ":\n" ++ ppCallGraph' ((replicate 4 ' ') ++ prefix) call

