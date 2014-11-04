-- TODO
-- Max over calls instead of sum
-- Function pointers
--
-- UI:
--  config file?
--  REPL interface / DOT
--
-- simplify conditional CExpr's

{-# LANGUAGE ScopedTypeVariables #-}

module Development.Ecstatic.StackUsage where

import Development.Ecstatic.Utils
import Development.Ecstatic.Size
import qualified Development.Ecstatic.Simplify as S

import Language.C
import Language.C.Analysis
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import System.Console.ANSI
import Control.Monad.State

data CallGraph = CG
  { localStack :: CExpr
  , totalStack :: CExpr
  , children   :: [(String, CallGraph)] }
  deriving (Show)

type StackMap = M.Map Ident CallGraph

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

addStackVal :: Ident -> CallGraph -> State StackMap ()
addStackVal name cg = modify (M.insert name cg)

defStackUsage :: GlobalDecls -> CStat -> State StackMap CallGraph
defStackUsage g s = do
  fs <- sequence func_calls
  let total_stack = S.simplify $
        vars_size + sum (map (totalStack . snd) fs)
  return $ CG vars_size total_stack fs
  where
    --total_stack = S.simplify $
    -- Total size of all the local variables
    vars_size = S.simplify $ sum [sizeOfDecl g d | d :: CDecl <- universeBi s]
    -- Total of function calls
    func_calls = [callStackUsage g func args | CCall func args _ :: CExpr <- universeBi s]

callStackUsage :: GlobalDecls -> CExpr -> [CExpr] -> State StackMap (String, CallGraph)
callStackUsage g expr@(CVar ident _) args = do
  cg <- doCG
  return (name, cg)
 where
  name = identToString ident
  doCG = do
    mval <- gets (M.lookup ident)
    case mval of
      -- Value already available
      Just cg -> return cg
      Nothing ->
        case M.lookup ident (gObjs g) of
          -- Have global decl
          Just (FunctionDef (FunDef
                              (VarDecl _ _ (FunctionType (FunType _ params _) _))
                              def _)) -> do
            cg <- defStackUsage g def
            addStackVal ident cg
            let ids = [id | (ParamDecl (VarDecl (VarName id _) _ _) _) <- params]
            return (subsInCallGraph (zip ids args) cg)
          -- Unknown function, return abstract size
          Just x  -> returnVar -- TODO is this case right?
          Nothing -> returnVar
  returnVar = do
    let cg = CG expr expr []
    addStackVal ident cg
    return cg
-- TODO resolve function pointers
callStackUsage _ expr _ = return ("FN_PTR", CG expr expr [])
callStackUsage g expr args = error $ "callStackUsage. :" ++ show expr ++ show args

subInCallGraph :: Ident -> CExpr -> CallGraph -> CallGraph
subInCallGraph ident e (CG e1 t1 cs) =
  CG (substitute ident e e1) 
     (substitute ident e t1) 
     $ map (mapSnd (subInCallGraph ident e)) cs
subsInCallGraph :: [(Ident, CExpr)] -> CallGraph -> CallGraph
subsInCallGraph subs cg = foldr (uncurry subInCallGraph) cg subs

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
          

