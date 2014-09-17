-- Main functions are csmul and csadd
module Development.Ecstatic.Simplify (
  simplify
) where

import Language.C hiding (Name)
import Language.C.Data.Ident

import qualified Data.Map.Strict as M

-- Language.C expression type
type CTerm = CExpression NodeInfo

-- Product of variables
type Name = String
data Prod = Prod [Name]
  deriving (Show, Eq, Ord)

-- Product of arbitrary CTerms, variables, integer
-- No Eq instance, only partial simplification
data CProd = CProd [CTerm] [Name] Integer

type MulAcc = ([CTerm], M.Map Name Integer, Integer)
type SumAcc = ([CProd],  M.Map Prod Integer)
accEmpty = ([], M.empty)

-- Simplify Multiplication
csmul :: [MulAcc] -> CTerm -> [MulAcc]
csmul accs (CConst (CIntConst (CInteger number _ _) _)) =
  map (\(ts, m, n) -> (ts, m, n * number)) accs
csmul accs (CVar (Ident name _ _) _) =
  map (\(ts, m, n) -> (ts, M.insertWith (+) name 1 m, n)) accs
csmul accs (CBinary CMulOp t1 t2 _) =
  csmul (csmul accs t1) t2
csmul accs (CBinary CAddOp t1 t2 _) =
  csmul accs t1 ++ csmul accs t2
csmul accs (CBinary CSubOp t1 t2 ni) =
  csmul accs (CBinary CAddOp t1 (CUnary CMinOp t2 undefined) ni)
csmul accs (CUnary CMinOp t2 _) =
  csmul (map (\(ts, m, n) -> (ts, m, n * (-1))) accs)
        t2
csmul accs t =
  map (\(ts, m, n) -> (t : ts, m, n)) accs

toProd :: MulAcc -> CProd
toProd (ts, m, n) = CProd ts (foldl f [] $ M.toList m) n
 where
  f acc (name, num) =
    replicate (fromIntegral num) name ++ acc 

-- Simplify Addition
csadd :: SumAcc -> CTerm -> SumAcc
csadd (ts, acc) (CConst (CIntConst (CInteger number _ _) _)) =
  (ts, M.insertWith (+) (Prod []) number acc)
csadd acc (CBinary CAddOp t1 t2 _) =
  csadd (csadd acc t1) t2
csadd acc t =
  let accs = csmul [([], M.empty, 1)] t
      ps   = map toProd accs
      f :: SumAcc -> CProd -> SumAcc
      f (cts, acc) prod@(CProd ts' names n) =
        case prod of
          -- Simple product of variables
          CProd [] names n ->
            (cts, M.insertWith (+) (Prod names) n acc)
          _ ->
            (prod : cts, acc)
  in  
    foldl f acc ps

-- TODO
-- Okay for just pretty-printing
nodeinfo :: NodeInfo
nodeinfo = undefined

-- Print CExpressions
ct x y  = CBinary CMulOp x y nodeinfo -- Nodeinfo
cp x y  = CBinary CAddOp x y nodeinfo
ci n    = CConst (CIntConst (CInteger n DecRepr noFlags) nodeinfo) 
cv name = CVar (Ident name 0 nodeinfo) nodeinfo

prodToExpr :: Prod -> Integer -> CTerm
prodToExpr (Prod prod) i =
  foldl1 ct (ci i : (map cv prod))

cprodToExpr :: CProd -> CTerm
cprodToExpr (CProd ts prod i) =
  foldl1 ct (ci i : (ts ++ (map cv prod)))

-- Convert normalized sum into CExpression
ctoExpr :: SumAcc -> CTerm
ctoExpr (ts, m) =
  let terms = (map cprodToExpr ts) ++
              (map (uncurry prodToExpr) (M.toList m))
  in
    foldl1 cp terms

simplify :: CTerm -> CTerm
simplify = ctoExpr . csadd accEmpty
