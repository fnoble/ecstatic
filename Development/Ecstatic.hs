--TODO
-- Proper command line flag parsing
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Development.Ecstatic.Types
import Development.Ecstatic.Utils
import Development.Ecstatic.StackUsage
import Development.Ecstatic.Link
import Development.Ecstatic.SimplifyDef

import Language.C
import Language.C.Analysis
import Text.Printf
import System.Console.ANSI
import qualified Text.PrettyPrint as PP
import System.Environment (getArgs)
import Control.Monad.State
import Control.Applicative((<$>))
import qualified Data.Map.Strict as M
import Data.List (nub)

declHeader :: VarDecl -> String
declHeader d =
  printf "%s (%s:%d):" name file line
  where
    i = declIdent d
    name = identToString i
    p = posOfNode . nodeInfo $ i
    line = posRow p
    file = posFile p

ppDeclCG :: (VarDecl, CallGraph, CallGraph) -> IO ()
ppDeclCG (d, cg1, cg2) = do
  setSGR [Reset]
  putStrLn $ declHeader d
  let str1 = ppCallGraph cg1
  let str2 = ppCallGraph cg2

  setSGR [SetColor Foreground Dull Red]
  --putStrLn $ str1
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ str2
  setSGR [Reset]

-- PP the parsed AST
reprint :: IO String
reprint = do
  ast <- parseFile "test2.c"
  return $ PP.render . prettyUsingInclude $ ast

analyzeFiles :: [FilePath] -> IO (Maybe [(VarDecl, CallGraph, CallGraph)])
analyzeFiles files = do
  mast <- parseASTFiles files
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file." >> return Nothing
    Just (globals, funcs) -> do
      let idents = M.keys $ gObjs globals
      let o = M.toList $ reformat globals
      mapM_ print $ map fst $ o
      print $ length o


      let pairs'' = evalStack $ mapM (go globals) funcs
      -- TODO make general filtering code
      let pairs''' = filter ((== "process_matched_obs") . identToString 
                                . declIdent . fst) pairs''
      let pairs = pairs''
      let pairs' = map (\(a, b) -> (a, b, reduceCG b)) pairs

      mapM_ ppDeclCG pairs'
      --let idents = nub . concat . map cgIdents . map snd $ pairs
      --putStrLn $ "idents (" ++ (show $ length idents) ++ "): "
      --mapM_ (\(a) -> putStrLn a) . nub . map fst $ idents
      --putStrLn $ "idents (" ++ (show $ length idents) ++ ")"
      --putStrLn $ "names (" ++ (show $ length $ nub . map fst $ idents) ++ ")"
      putStrLn $ "num functions: " ++ show (length funcs)
      return $ Just pairs'
  where
   go :: GlobalDecls -> FunDef -> State StackMap (VarDecl, CallGraph)
   go g (FunDef d s _) | Just params <- funParams d =
     (d,) <$> defStackUsage g (s, params)
   go _ f = error $ "analyzeFiles. not a FunDef?: " ++ show f

data FlagVal = NoFlag | Flag -- | FlagVal a
parseFlag :: [String] -> String -> (FlagVal, [String])
parseFlag args flag =
  let (front, back) = span (/= flag) args
  in
  case back of
    [] -> (NoFlag, front)
    _ : rest -> (Flag, front ++ rest)

-- For ghci
testMain = do
  doMain NoFlag ["test.c"]

printDeclSize (b,a,_,_) = 
  let label = identToString . declIdent $ a
  in putStrLn $ label ++ ": " ++ show b

g0 (a,_,_,_) = a
g1 (_,a,_,_) = a
g2 (_,_,a,_) = a
g3 (_,_,_,a) = a

printParse file = do
  mast <- parseASTFiles [file]
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file."
    Just (globals, funcs) -> do
      mapM_ print funcs
  
doMain :: FlagVal -> [FilePath] -> IO (Maybe [(Maybe Int, VarDecl, CallGraph, CallGraph)])
doMain flag files = do
  case flag of
    NoFlag -> return ()
    Flag -> do
      pps <- mapM preprocessFile files
      writeFile "pp-output.c" (concat pps)
  mtriples <- analyzeFiles files
  case mtriples of
    Nothing -> return Nothing
    Just triples -> do
      let ts0 = map (\(a,b,c) -> (isPrim (totalStack c), a,b,c)) triples
          ts1 = reverse $ sortWith g0 ts0
      mapM_ printDeclSize $ take 60 ts1
      return $ Just ts1

-- FLAGS:
--  -E: outputs total preprocessor output to file
main = do
  args <- getArgs
  let (flag, files) = parseFlag args "-E"
  doMain flag files
