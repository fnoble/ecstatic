--TODO
-- Proper command line flag parsing
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Development.Ecstatic.CallGraph
import Development.Ecstatic.Utils
import Development.Ecstatic.StackUsage
import qualified Development.Ecstatic.Assumptions as A

import Language.C
import Language.C.Analysis
import Text.Printf
import System.Console.ANSI
import qualified Text.PrettyPrint as PP
import System.Environment (getArgs)
import Control.Monad.State
import Control.Applicative((<$>))
import Data.Either (partitionEithers)

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
ppDeclCG (d, cg1) = do
  setSGR [Reset]
  putStrLn $ declHeader d
  let str1 = ppCallGraph cg1
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ str1
  setSGR [Reset]

-- PP the parsed AST
reprint :: IO String
reprint = do
  ast <- parseFile "test2.c"
  return $ PP.render . prettyUsingInclude $ ast

declString :: VarDecl -> String
declString = identToString . declIdent

filterCallGraphFn :: VarDecl -> Bool
filterCallGraphFn =
  (`elem` (map fst A.stack_limits)) . declString

-- TODO NOW overflow return code
analyzeFiles :: [FilePath] -> IO (Maybe (String, [(VarDecl, CallGraph)]))
analyzeFiles files = do
  mast <- parseASTFiles files
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file." >> return Nothing
    Just (globals, funcs) -> do
      -- Calculate graphs, keep only those with stack limits
      let pairs = filter (filterCallGraphFn . fst)
                  $ evalStack $ mapM (go globals) funcs

      --mapM_ ppDeclCG pairs

      -- Lookup stack limits
      let limitPairs =
            [ (limit, (name, cg))
            | (decl, cg) <- pairs
            , let name = declString decl
            , let Just limit = lookup name A.stack_limits]

      let traces = map (uncurry maxTraces) limitPairs
          (bad, good) = partitionEithers traces

      let nsString = "num_sats: " ++ show (A.num_dds+1) ++ "\n"
      lines <- mapM (ppTraceLimit False) good
      let outputString = unlines $ nsString : lines

      case bad of
        [] -> return ()
        _  -> do 
          putStrLn "analyzeFiles. error: symbolic values in "
          print bad

      putStrLn $ "num functions: " ++ show (length funcs)
      return $ Just (outputString, pairs)
  where
   go :: GlobalDecls -> FunDef -> State StackMap (VarDecl, CallGraph)
   go g (FunDef d s _) | Just params <- funParams d =
     (d,) <$> defStackUsage g (s, params)
   go _ f = error $ "analyzeFiles. not a FunDef?: " ++ show f

   ppTraceLimit :: Bool -> Trace -> IO String
   ppTraceLimit put trace = do
     let limitString = "(limit " ++ show (trLimit trace) ++ ")"
         traceString = ppTrace trace
     if put
       then do
       setSGR [SetColor Foreground Dull Blue]
       putStrLn limitString
       setSGR [Reset]
       putStrLn $ traceString
       else return ()
     return $ unlines [limitString, traceString]


data FlagVal = NoFlag | Flag -- | FlagVal a
parseFlag :: [String] -> String -> (FlagVal, [String])
parseFlag args flag =
  let (front, back) = span (/= flag) args
  in
  case back of
    [] -> (NoFlag, front)
    _ : rest -> (Flag, front ++ rest)

-- For ghci
testMain :: IO ()
testMain = do
  doMain NoFlag ["test.c"]
  return ()

printParse :: FilePath -> IO ()
printParse file = do
  mast <- parseASTFiles [file]
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file."
    Just (_, funcs) -> do
      mapM_ print funcs

doMain :: FlagVal -> [FilePath] -> IO (Maybe (String, [(VarDecl, CallGraph)]))
doMain flag files = do
  case flag of
    Flag -> do
      pps <- mapM preprocessFile files
      writeFile "pp-output.c" (concat pps)
      return Nothing
    NoFlag ->
      analyzeFiles files

-- FLAGS:
--  -E: outputs total preprocessor output to file
main :: IO ()
main = do
  args <- getArgs
  let (flag, files) = parseFlag args "-E"
  mpair <- doMain flag files
  case mpair of
    Just (str, _) -> writeFile "ecstatic-output" str
    Nothing -> return ()
