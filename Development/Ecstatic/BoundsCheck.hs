module Development.Ecstatic.BoundsCheck where

import Development.Ecstatic.Utils

import Language.C
import Language.C.Pretty
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data
import System.Console.ANSI
import Control.Monad
import Text.Printf

boundsCheck :: CStatement NodeInfo -> IO ()
boundsCheck s = do
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "Upper Bounds:"
  setSGR [Reset]
  analyseBounds $ subForU s
  --print $ pretty $ subForL s
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "Lower Bounds:"
  setSGR [Reset]
  analyseBounds $ subForL s

analyseBounds :: CStatement NodeInfo -> IO ()
analyseBounds s = void $ transformBiM f s
  where
    f :: CExpression NodeInfo -> IO (CExpression NodeInfo)
    f e = g e >> return e
    g :: CExpression NodeInfo -> IO ()
    g e@(CCall (CVar i n) a2 a3) = do
      let fname = identToString i
      when (fname == "__builtin___memset_chk" ||
            fname == "__inline_memset_chk" ||
            fname == "__builtin___memcpy_chk" ||
            fname == "__inline_memcpy_chk")
        $ do
          putStr $ "Called: " ++ fname
          print $ map pretty a2
    g (CIndex a1 a2 (NodeInfo pos _ _)) = do
      printf "%s:%d: %s [%s]\n" (posFile pos) (posRow pos)
        (show $ pretty a1) (show $ pretty a2)
    g x = return ()




subFor :: (Data a, Typeable a, Show a) =>
            (Ident -> CExpression a -> Maybe (CExpression a)
               -> Maybe (CExpression a) -> CStatement a -> CStatement a)
            -> CStatement a -> CStatement a
subFor f = transform g
  where --g
        -- for loop of the form: for (var = init; cond; step) { s; }
        g (CFor
            inits@(Left (Just (CAssign CAssignOp (CVar var _) inite _)))
            cond step s n
          ) = CFor inits cond step s' n
          where s' = f var inite cond step s
        -- for loop of the form: for (type var = init; cond; step) { s; }
        g (CFor
            inits@(Right (CDecl _ [
              (Just (CDeclr (Just var) _ _ _ _),
              (Just (CInitExpr inite _)
            ), _)] _))
            cond step s n
          ) = CFor inits cond step s' n
          where s' = f var inite cond step s
        g fl@(CFor _ _ _ _ _) = error $
          "subFor: Unsupported for loop format:\n\n" ++ show fl
        g x = x

subForU :: (Data a, Typeable a, Show a) => CStatement a -> CStatement a
subForU = (subFor subForU')
subForL :: (Data a, Typeable a, Show a) => CStatement a -> CStatement a
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

