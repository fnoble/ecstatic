--TODO
-- Proper command line flag parsing
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Development.Ecstatic.Utils
import Development.Ecstatic.StackUsage
import Development.Ecstatic.Link

import Language.C
import Language.C.Analysis
import Text.Printf
import System.Console.ANSI
import qualified Text.PrettyPrint as PP
import System.Environment (getArgs)
import Control.Monad.State
import Control.Applicative((<$>))
import qualified Data.Map.Strict as M

declHeader :: VarDecl -> String
declHeader d =
  printf "%s (%s:%d):" name file line
  where
    i = declIdent d
    name = identToString i
    p = posOfNode . nodeInfo $ i
    line = posRow p
    file = posFile p

ppDeclCG :: (VarDecl, CallGraph) -> IO ()
ppDeclCG (d, cg) = do
  setSGR [SetColor Foreground Dull Red]
  putStrLn $ declHeader d
  setSGR [Reset]
  putStrLn $ ppCallGraph cg

-- PP the parsed AST
reprint :: IO String
reprint = do
  ast <- parseFile "test2.c"
  return $ PP.render . prettyUsingInclude $ ast

analyzeFiles :: [FilePath] -> IO ()
analyzeFiles files = do
  mast <- parseASTFiles files
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file."
    Just (globals, funcs) -> do
      let idents = M.keys $ gObjs globals
      let o = M.toList $ reformat globals
      mapM_ print $ map fst $ o
      print $ length o
      let pairs = evalStack $ mapM (go globals) funcs
      mapM_ ppDeclCG pairs
      putStrLn $ "num functions: " ++ show (length funcs)
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

-- FLAGS:
--  -E: outputs total preprocessor output to file
main :: IO ()
main = do
  args <- getArgs
  let (flag, files) = parseFlag args "-E"
  case flag of
    NoFlag -> return ()
    Flag -> do
      pps <- mapM preprocessFile files
      writeFile "pp-output.c" (concat pps)
  analyzeFiles files
