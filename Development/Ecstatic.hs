--TODO
-- Proper command line flag parsing
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Development.Ecstatic.Utils
import Development.Ecstatic.StackUsage

import Language.C
import Language.C.Analysis
import Text.Printf
import System.Console.ANSI
import qualified Text.PrettyPrint as PP
import System.Environment (getArgs)
import Control.Monad.State
import Control.Applicative((<$>))

declHeader :: VarDecl -> String
declHeader d =
  printf "%s (%s:%d):" name file line
  where
    i = declIdent d
    name = identToString i
    p = posOfNode . nodeInfo $ i
    line = posRow p
    file = posFile p

--analyseFunc :: GlobalDecls -> FunDef -> IO ()
--analyseFunc g (FunDef d s _) = do
--  -- Print function name header
--  setSGR [SetColor Foreground Dull Red]
--  putStrLn $ declHeader d
--  setSGR [Reset]
--  _ <- printCallGraph g s
--  return ()

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
      let pairs = evalStack $ mapM (go globals) funcs
      mapM_ ppDeclCG pairs
      --mapM_ (analyseFunc globals) $ funcs
      putStrLn $ "num functions: " ++ show (length funcs)
  where
   go :: GlobalDecls -> FunDef -> State StackMap (VarDecl, CallGraph)
   go g (FunDef d s _) = (d,) <$> defStackUsage g s

data FlagVal = NoFlag | Flag -- | FlagVal a
parseFlag :: [String] -> String -> (FlagVal, [String])
parseFlag args flag =
  let (front, back) = span (/= flag) args
  in
  case back of
    [] -> (NoFlag, front)
    _ : rest -> (Flag, front ++ rest)
    --_ : val : rest ->
    --  ( FlagVal (read val)
    --  , front ++ rest)

-- FLAGS:
-- -E: outputs total preprocessor output to file
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
