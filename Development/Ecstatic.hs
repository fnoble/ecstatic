-- TODO
-- Read from config file rather than flag
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
import System.Console.GetOpt

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

-- Pretty print a parsed AST
reprintAST :: CTranslUnit -> String
reprintAST ast = do
  PP.render . prettyUsingInclude $ ast

declString :: VarDecl -> String
declString = identToString . declIdent

filterCallGraphFn :: VarDecl -> Bool
filterCallGraphFn =
  (`elem` (map fst A.stack_limits)) . declString

-- TODO overflow return code?
analyzeFiles :: FilePath -> [FilePath] -> IO (Maybe (String, [(VarDecl, CallGraph)]))
analyzeFiles base files = do
  mast <- parseASTFiles base files
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
      lines <- mapM (ppTraceLimit False) good

      -- Output string
      let 
        nsString = "num_sats: " ++ show (A.num_dds+1) ++ "\n"
        errors = case bad of
                   [] -> ""
                   _  -> 
                     "analyzeFiles. error: symbolic values in\n"
                     ++ show bad
        overflows = if any isOverflow good then "OVERFLOW DETECTED" else ""
        outputString = unlines $ overflows : errors : nsString : lines

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

printParse :: FilePath -> FilePath -> IO ()
printParse base file = do
  mast <- parseASTFiles base [file]
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file."
    Just (_, funcs) -> do
      mapM_ print funcs

doParse base output files = do
  pps <- mapM (preprocessFile base) files
  writeFile output (concat pps)
  return Nothing

doSimpleMode files = error "doSimpleMode. not implemented."

doMain :: FilePath -> FilePath -> [FilePath] -> IO (Maybe (String, [(VarDecl, CallGraph)]))
doMain base output files = do
  mpair <- analyzeFiles base files
  case mpair of
    Just (str, _) -> writeFile output ("<pre>\n" ++ str ++ "</pre>")
    Nothing -> return ()
  return mpair

data Options = Options
  { optBaseDir :: FilePath
  , optOutputFile :: FilePath
  , optOnlyParse :: Bool
  , optSimpleMode :: Bool
  }
defaultOptions = Options
  { optBaseDir = "./piksi_firmware"
  , optOutputFile = "ecstatic-output"
  , optOnlyParse = False
  , optSimpleMode = False
  }
type FlagMod = Options -> Options
mainOptions :: [OptDescr FlagMod]
mainOptions =
  [ Option "E" ["preprocess"]
     (NoArg $ \o -> o { optOnlyParse = True })
     "Write preprocessor output and stop."
--  , Option "s" ["simple"]
--     (NoArg $ \o -> o { optSimpleMode = True })
--     "Analyze all functions in a single file."
  , Option "o" ["output"]
     (ReqArg (\s o -> o { optOutputFile = s }) "FILE")
     "Output file."
  , Option "b" ["src"]
     (ReqArg (\s o -> o { optBaseDir = s }) "DIR")
     "Location of piksi_firmware source."
  ]

main = do
  args <- getArgs
  case getOpt Permute mainOptions args of
    (flags, files, []) ->
      let options = foldr ($) defaultOptions flags
          base = (optBaseDir options)
          output = (optOutputFile options)
      in
      if optOnlyParse options
        then do
          doParse base output files
        else if optSimpleMode options
             then doSimpleMode files
             else doMain base output files

    (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo optHeader mainOptions
 where
   optHeader = "Usage: "
