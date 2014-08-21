
module Main where

import Language.C
import Language.C.Pretty
import Language.C.System.GCC
import Language.C.Analysis
import qualified Data.Map as M
import Debug.Trace
import Language.C.Data.Ident
import Text.Printf
import Math.CommutativeAlgebra.Polynomial
import Math.Algebras.VectorSpace
import Math.Core.Field
import Control.Monad
import Data.List
import Control.Exception
import Data.Generics.Uniplate.Data
import System.Console.ANSI


includes = [
  "-I../../SwiftNav/piksi_firmware/libswiftnav/include/libswiftnav",
  "-I../../SwiftNav/piksi_firmware/libswiftnav/clapack-3.2.1-CMAKE/INCLUDE",
  "-I../../SwiftNav/piksi_firmware/libswiftnav/CBLAS/include" ]

parseFile :: FilePath -> IO CTranslUnit
parseFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing includes input_file
     checkResult "[Parsing]" parse_result

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

extractFuncs :: DeclEvent -> Trav [FunDef] ()
extractFuncs (DeclEvent (FunctionDef f)) = do
  modifyUserState (\x -> f:x)
  return ()
extractFuncs _ = return ()

declHeader :: VarDecl -> String
declHeader d =
  printf "%s (%s:%d):" name file line
  where
    i = declIdent d
    name = identToString i
    p = case i of
      (Ident _ _ (NodeInfo pos _ _)) -> pos
    line = posRow p
    file = posFile p

analyseStmt :: CStatement NodeInfo -> IO ()
analyseStmt (CExpr (Just expr) _) = analyseExpr expr --print $ pretty expr
analyseStmt (CCompound _ cbis _) = mapM_ f cbis
  where f (CBlockStmt s) = analyseStmt s
        f _ = return ()
--(CFor (Either (Maybe (CExpression a)) (CDeclaration a)) (Maybe (CExpression a)) (Maybe (CExpression a)) (CStatement a) a)
analyseStmt cf@(CFor _ _ _ s _) = analyseStmt s
analyseStmt _ = return ()


analyseExpr :: CExpression NodeInfo -> IO ()
analyseExpr (CComma a1 a2) = mapM_ analyseExpr a1
analyseExpr (CAssign a1 a2 a3 a4) = do
  analyseExpr a2
  analyseExpr a3
analyseExpr (CCond a1 (Just a2) a3 a4) = do
  analyseExpr a1
  analyseExpr a2
  analyseExpr a3
analyseExpr (CBinary a1 a2 a3 a4) = do
  analyseExpr a2
  analyseExpr a3
analyseExpr (CCast a1 a2 a3) = analyseExpr a2
analyseExpr (CUnary a1 a2 a3) = analyseExpr a2
-- analyseExpr (CSizeofExpr a1 a2) = analyseExpr a1
analyseExpr (CAlignofExpr a1 a2) = analyseExpr a1
analyseExpr (CComplexReal a1 a2) = analyseExpr a1
analyseExpr (CComplexImag a1 a2) = analyseExpr a1
analyseExpr (CCall (CVar i n) a2 a3) = do
  let fname = identToString i
  if fname == "__builtin___memset_chk" ||
     fname == "__inline_memset_chk" ||
     fname == "__builtin___memcpy_chk" ||
     fname == "__inline_memcpy_chk"
    then do
      putStr $ "Called: " ++ fname
      print $ map pretty a2
    else mapM_ analyseExpr a2
analyseExpr (CMember a1 a2 a3 a4) = analyseExpr a1
analyseExpr (CStatExpr a1 a2) = analyseStmt a1

analyseExpr (CIndex a1 a2 (NodeInfo pos _ _)) = do
  expr <- return $ case exprToPoly a2 of
    Nothing -> show $ pretty a2
    (Just p) -> show p
  printf "%s:%d: %s [%s]\n" (posFile pos) (posRow pos)
    (show $ pretty a1) expr
  --analyseExpr a1
  --analyseExpr a2
analyseExpr _ = return ()
-- analyseExpr (CVar a1 a2) = return ()
-- analyseExpr (CCompoundLit a1 a2 a3) = return ()
-- analyseExpr (CLabAddrExpr a1 a2) = return ()
-- analyseExpr (CBuiltinExpr a1) = return ()
-- analyseExpr (CConst a1) = return ()
-- analyseExpr (CAlignofType a1 a2) = return ()
-- analyseExpr (CSizeofType a1 a2) = analyseExpr a1

exprToPoly :: (Show a) => CExpression a -> Maybe (GrevlexPoly Q String)
exprToPoly = (liftM snd) . exprToPoly' M.empty

exprToPoly' :: (Show a) => M.Map Ident (GrevlexPoly Q String) -> CExpression a
                 -> Maybe (M.Map Ident (GrevlexPoly Q String), GrevlexPoly Q String)
exprToPoly' m (CConst (CIntConst (CInteger i _ _) _)) = Just (m, fromInteger i)
exprToPoly' m (CVar i _) = case M.lookup i m of
  (Just x) -> Just (m, x)
  Nothing -> Just (M.insert i x m, x)
    where x = grevlexvar (identToString i)
exprToPoly' m (CBinary CMulOp e1 e2 _) = do
  (m1, x1) <- exprToPoly' m e1
  (m2, x2) <- exprToPoly' m1 e2
  return (m2, x1 * x2)
exprToPoly' m (CBinary CDivOp e1 e2 _) = do
  (m1, x1) <- exprToPoly' m e1
  (m2, x2) <- exprToPoly' m1 e2
  return (m2, x1 / x2)
exprToPoly' m (CBinary CAddOp e1 e2 _) = do
  (m1, x1) <- exprToPoly' m e1
  (m2, x2) <- exprToPoly' m1 e2
  return (m2, x1 + x2)
exprToPoly' m (CBinary CSubOp e1 e2 _) = do
  (m1, x1) <- exprToPoly' m e1
  (m2, x2) <- exprToPoly' m1 e2
  return (m2, x1 - x2)
exprToPoly' m (CCast d e _) = exprToPoly' m e
exprToPoly' m other = Nothing


polyBigger :: GrevlexPoly Q String -> GrevlexPoly Q String -> Maybe Bool
polyBigger a b = polyBigger' (terms a) (terms b)

polyBigger' :: [(Grevlex String, Q)] -> [(Grevlex String, Q)] -> Maybe Bool
-- a > b ?
polyBigger' [] bs = polyBigger' (terms 0) bs
polyBigger' [(na,a)] [(nb,b)] = do
  if na == nb then return (a > b)
              else Nothing
polyBigger' as [(n,b)] = do
  a <- n `lookup` as
  return (a > b)
polyBigger' [(n, a)] bs = do
  b <- n `lookup` bs
  return (a > b)
polyBigger' as ((n,b):bs) = do
  a <- n `lookup` as
  rest <- polyBigger' as bs
  if rest == (a > b)
    then Just rest
    else Nothing

lowestOrder :: Monomial m => [Vect t m] -> Vect t m
lowestOrder = minimumBy (\a b -> compare (deg a) (deg b))

reducePolySet [] nps = nps
reducePolySet (p:ps) [] = []
reducePolySet (p:ps) (np:nps) = case polyBigger p np of
    (Just True)  -> p : reducePolySet ps nps
    (Just False) -> np : reducePolySet ps nps
    Nothing      -> np : p : reducePolySet ps nps

mapStmt :: (CStatement a -> CStatement a) ->
           (CExpression a -> CExpression a) ->
           (CDeclaration a -> CDeclaration a) ->
             CStatement a -> CStatement a
mapStmt fs fe fd (CLabel i s as n) = CLabel i (fs s) as n
mapStmt fs fe fd (CCase e s n) = CCase (fe e) (fs s) n
mapStmt fs fe fd (CCases e1 e2 s n) = CCases (fe e1) (fe e2) (fs s) n
mapStmt fs fe fd (CDefault s n) = CDefault (fs s) n
mapStmt fs fe fd (CExpr me n) = CExpr (fmap fe me) n
mapStmt fs fe fd (CCompound is cbis n) =
  CCompound is (map (mapCBI fs fe fd) cbis) n
mapStmt fs fe fd (CIf e s ms n) = CIf (fe e) (fs s) (fmap fs ms) n
mapStmt fs fe fd (CSwitch e s n) = CSwitch (fe e) (fs s) n
mapStmt fs fe fd (CWhile e s b n) = CWhile (fe e) (fs s) b n
mapStmt fs fe fd (CFor (Left me1) me2 me3 s n) =
  CFor (Left (fmap fe me1)) (fmap fe me2) (fmap fe me3) (fs s) n
mapStmt fs fe fd (CFor (Right d) me1 me2 s n) =
  CFor (Right (fd d)) (fmap fe me1) (fmap fe me2) (fs s) n
mapStmt fs fe fd (CGoto i n) = CGoto i n
mapStmt fs fe fd (CGotoPtr e n) = CGotoPtr (fe e) n
mapStmt fs fe fd (CCont n) = CCont n
mapStmt fs fe fd (CBreak n) = CBreak n
mapStmt fs fe fd (CReturn me n) = CReturn (fmap fe me) n
mapStmt fs fe fd (CAsm ass n) = CAsm ass n

mapCBI :: (CStatement a -> CStatement a) ->
          (CExpression a -> CExpression a) ->
          (CDeclaration a -> CDeclaration a) ->
            CCompoundBlockItem a -> CCompoundBlockItem a
mapCBI fs fe fd (CBlockStmt s) = CBlockStmt (fs s)
mapCBI fs fe fd (CBlockDecl d) = CBlockDecl (fd d)
mapCBI fs fe fd (CNestedFunDef _) =
  error "Nested function definitions not supported"

mapExpr :: (CStatement a -> CStatement a) ->
           (CExpression a -> CExpression a) ->
           (CDeclaration a -> CDeclaration a) ->
             CExpression a -> CExpression a

mapExpr fs fe fd (CComma es n) = CComma (map fe es) n
mapExpr fs fe fd (CAssign op e1 e2 n) = CAssign op (fe e1) (fe e2) n
mapExpr fs fe fd (CCond e1 me e2 n) = CCond (fe e1) (fmap fe me) (fe e2) n
mapExpr fs fe fd (CBinary op e1 e2 n) = CBinary op (fe e1) (fe e2) n
mapExpr fs fe fd (CCast d e n) = CCast (fd d) (fe e) n
mapExpr fs fe fd (CUnary op e n) = CUnary op (fe e) n
mapExpr fs fe fd (CSizeofExpr e n) = CSizeofExpr (fe e) n
mapExpr fs fe fd (CSizeofType d n) = CSizeofType (fd d) n
mapExpr fs fe fd (CAlignofExpr e n) = CAlignofExpr (fe e) n
mapExpr fs fe fd (CAlignofType d n) = CAlignofType (fd d) n
mapExpr fs fe fd (CComplexReal e n) = CComplexReal (fe e) n
mapExpr fs fe fd (CComplexImag e n) = CComplexImag (fe e) n
mapExpr fs fe fd (CIndex e1 e2 n) = CIndex (fe e1) (fe e2) n
mapExpr fs fe fd (CCall e es n) = CCall (fe e) (map fe es) n
mapExpr fs fe fd (CMember e i b n) = CMember (fe e) i b n
mapExpr fs fe fd (CVar i n) = CVar i n
mapExpr fs fe fd (CConst c) = CConst c
-- TODO: map over il
mapExpr fs fe fd (CCompoundLit d il n) = CCompoundLit (fd d) il n
mapExpr fs fe fd (CStatExpr s n) = CStatExpr (fs s) n
mapExpr fs fe fd (CLabAddrExpr i n) = CLabAddrExpr i n
-- TODO: Map over bi
mapExpr fs fe fd (CBuiltinExpr bi) = CBuiltinExpr bi

substitute :: Ident -> CExpression a -> CStatement a -> CStatement a
substitute a b = mapStmt (substitute a b) fe id
  where fe (CVar i n) = if i == a then b else (CVar i n)
        fe e = mapExpr (substitute a b) fe id e

substitute' :: Ident -> CExpression a -> CStatement a -> CStatement a
substitute' i e = transformBi f
  where f :: CExpression a -> CExpression a
        f (CVar i' n) = if i == i' then e else (CVar i n)
        f x = x


subFor :: (Show a) => (Ident -> CExpression a -> Maybe (CExpression a) ->
                        Maybe (CExpression a) -> CStatement a -> CStatement a)
          -> CStatement a -> CStatement a
-- for loop of the form: for (var = init; cond; step) { s; }
subFor f (CFor (Left (Just (CAssign CAssignOp (CVar var n1) init n2))) cond step s n3) =
  CFor (Left (Just (CAssign CAssignOp (CVar var n1) init n2))) cond step s' n3
  where s' = f var init cond step s
-- for loop of the form: for (type var = init; cond; step) { s; }
subFor f (CFor (Right (CDecl a1 [(Just (CDeclr (Just var) a2 a3 a4 n1), (Just (CInitExpr init n2)), n3)] n4)) cond step s n5) =
  CFor (Right (CDecl a1 [(Just (CDeclr (Just var) a2 a3 a4 n1), (Just (CInitExpr init n2)), n3)] n4)) cond step s' n5
  where s' = f var init cond step s
subFor _ f@(CFor _ _ _ _ _) = error $ "subFor: Unsupported for loop format:\n\n" ++ show f
subFor f s = mapStmt (subFor f) fe id s
  where fe = mapExpr (subFor f) fe id

subForU :: (Show a) => CStatement a -> CStatement a
subForU = (subFor subForU')
subForL :: (Show a) => CStatement a -> CStatement a
subForL = (subFor subForL')

-- for loop of the form: for (i = lower; i < upper; i++) { s; }
subForU' i lower (Just (CBinary CLeOp (CVar ii n1) upper n2)) (Just (CUnary CPostIncOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute upper bound on i == upper - 1
    then subForU $ substitute i (CBinary CSubOp upper (CConst (CIntConst (cInteger 1) n1)) n1) s
    else s
-- for loop of the form: for (i = lower; i <= upper; i++) { s; }
subForU' i lower (Just (CBinary CLeqOp (CVar ii n1) upper n2)) (Just (CUnary CPostIncOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute upper bound on i == upper
    then subForU $ substitute i upper s
    else s
-- for loop of the form: for (i = upper; i > lower; i--) { s; }
subForU' i upper (Just (CBinary CGrOp (CVar ii n1) lower n2)) (Just (CUnary CPostDecOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute upper bound on i == upper
    then subForU $ substitute i upper s
    else s
-- for loop of the form: for (i = upper; i >= lower; i--) { s; }
subForU' i upper (Just (CBinary CGeqOp (CVar ii n1) lower n2)) (Just (CUnary CPostDecOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute upper bound on i == upper
    then subForU $ substitute i upper s
    else s
subForU' _ _ cond step _ = error $ "subForU': Unsupported for loop format:\n\n" ++ show cond ++ "\n\n" ++ show step

-- for loop of the form: for (i = lower; i < upper; i++) { s; }
subForL' i lower (Just (CBinary CLeOp (CVar ii n1) upper n2)) (Just (CUnary CPostIncOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute lower bound on i == lower
    then subForL $ substitute i lower s
    else s
-- for loop of the form: for (i = lower; i <= upper; i++) { s; }
subForL' i lower (Just (CBinary CLeqOp (CVar ii n1) upper n2)) (Just (CUnary CPostIncOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute lower bound on i == lower
    then subForL $ substitute i lower s
    else s
-- for loop of the form: for (i = upper; i > lower; i--) { s; }
subForL' i upper (Just (CBinary CGrOp (CVar ii n1) lower n2)) (Just (CUnary CPostDecOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute lower bound on i == lower + 1
    then subForL $ substitute i (CBinary CAddOp lower (CConst (CIntConst (cInteger 1) n1)) n1) s
    else s
-- for loop of the form: for (i = upper; i >= lower; i--) { s; }
subForL' i upper (Just (CBinary CGeqOp (CVar ii n1) lower n2)) (Just (CUnary CPostDecOp (CVar iii n3) n4)) s =
  if ii == i && iii == i
    -- substitute lower bound on i == lower
    then subForL $ substitute i lower s
    else s
subForL' _ _ cond step _ = error $ "subForL': Unsupported for loop format:\n\n" ++ show cond ++ "\n\n" ++ show step

analyseFunc :: GlobalDecls -> FunDef -> IO ()
analyseFunc g f@(FunDef d s i) = do
  -- Print function name header
  setSGR [SetColor Foreground Dull Red]
  putStrLn $ declHeader d
  setSGR [Reset]

  print $ pretty d
  --print $ pretty s
  --print $ pretty $ subForU s
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "Upper Bounds:"
  setSGR [Reset]
  analyseStmt $ subForU s
  --print $ pretty $ subForL s
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "Lower Bounds:"
  setSGR [Reset]
  analyseStmt $ subForL s

main :: IO ()
main = do
  ast <- parseFile "test.c"
  --ast <- parseFile "test2.c"
  --ast <- parseFile "../../SwiftNav/piksi_firmware/libswiftnav/src/amb_kf.c"
  (globals, funcs) <- checkResult "[Analysis]" . runTrav [] $
    withExtDeclHandler (analyseAST ast) extractFuncs
  mapM_ (analyseFunc globals) $ userState funcs

