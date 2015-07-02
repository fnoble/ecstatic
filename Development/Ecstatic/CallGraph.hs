{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Development.Ecstatic.CallGraph where
import Development.Ecstatic.Assumptions
import Development.Ecstatic.Utils
import qualified Development.Ecstatic.SimplifyDef as S
import System.Console.ANSI
import Language.C
import Data.Typeable
import Data.Data
import Data.Maybe (mapMaybe)
import Data.Generics.Uniplate.Data

type FnName = String
data CallGraph = CG
  { localStack :: CExpr
  , totalStack :: CExpr
  , children   :: [(FnName, CallGraph)] }
  deriving (Show,Data,Typeable)

-- Simplification
cgMap :: (CExpr -> CExpr) -> CallGraph -> CallGraph
cgMap f (CG l t cs) = CG (f l) (f t) (map (mapSnd (cgMap f)) cs)

conjSimpl :: (CExpr -> CExpr) -> CExpr -> CExpr
conjSimpl f = S.simplify . f . S.simplify

fullSimplExpr :: CExpr -> CExpr
fullSimplExpr = conjSimpl reduceConditionals . conjSimpl (transformBi (subAllNames 0))

reduceCG :: CallGraph -> CallGraph
reduceCG cg =
  cgMap fullSimplExpr $
    foldr sub cg assumptions
  where
    sub (s, x) = subByName s ((fromIntegral x)::CExpr)

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
  -- Simpler alternative
  --ppStack' s | Just n <- S.isPrim s = show n
  --ppStack' _ = "[symbol]"
  ppName name =
    setSGRCode [SetColor Foreground Dull Blue] ++
    name ++ setSGRCode [Reset]
  ppBinding (name, call) =
    prefix ++ (ppName name) ++ ":\n" ++ ppCallGraph' ((replicate 4 ' ') ++ prefix) call

data Trace
  = Trace
  { trName :: FnName
  , trLocal :: Int
  , trTotal :: Int
  , trLimit :: Int 
  , trCalls :: [Trace]
  } deriving (Eq, Show)

-- Returns:
--  - Left: some subvalue was symbolic, ecstatic error condition
--  - Right (Trace{..}): potential overflow
--  TODO always output max trace?
--       verbose mode?
maxTraces :: Int -> (FnName, CallGraph)
          -> Either (FnName, CallGraph) Trace
maxTraces maxStack (name, (CG local total calls))
  | Just l <- S.isPrim local
  , Just t <- S.isPrim total =
  let
    cs :: [((FnName, CallGraph), Int)]
    cs = takeWhile (\(_, stack) -> stack + l >= maxStack)
         . reverse
         . sortWith snd
         . mapMaybe justSnd
         . map (\pair -> (pair, S.isPrim . totalStack . snd $ pair))
         $ calls
  in do
    subTraces <- mapM (maxTraces (maxStack - l)) (map fst cs)
    return $ Trace name l t maxStack subTraces 
 where
  justSnd (a, Just b) = Just (a, b)
  justSnd _ = Nothing
maxTraces _ pair = Left pair

isOverflow :: Trace -> Bool
isOverflow Trace { trTotal, trLimit } = trTotal > trLimit

ppTrace :: Trace -> String
ppTrace = ppTrace' ""
ppTrace' :: String -> Trace -> String
ppTrace' prefix Trace{..} =
  init . unlines $ [ prefix ++ trName ++ ": " ++ show trLocal ++ " " ++ show trTotal]
                   ++ map (ppTrace' (prefix ++ "  ")) trCalls

