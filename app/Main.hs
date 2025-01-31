module Main where

import Prelude as P
import Graphics.Image as I
import System.Random
import AST
import Render
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Char

data Options = Options
  { optDepth :: Int
  , optPixels :: Int
  , optOutput :: String
  , optSeed :: Int
  } deriving Show

defaultOpts = Options
  { optDepth = 10
  , optPixels = 200
  , optOutput = "out.png"
  , optSeed = 20250101
  }


opts :: [OptDescr (Options -> Options)]
opts =
  [Option ['d'] ["depth"] (ReqArg (\ d opts -> opts {optDepth = read d}) "DEPTH") "depth of tree"
  ,Option ['p'] ["pixels"] (ReqArg (\ p opts -> opts {optPixels = read p} ) "PIXELS") "pixel dimensions"
  ,Option ['o'] ["output"] (ReqArg (\ o opts -> opts {optOutput = o}) "OUTPUT") "output filepath"
  ,Option ['s'] ["seed"] (ReqArg (\ s opts -> opts {optSeed = P.sum $ P.map ord s}) "SEED") "initial seed"]


parseArgs :: [String] -> IO (Options)
parseArgs argv = case getOpt Permute opts argv of
  (o, [], []) -> return (foldl (flip id) defaultOpts o)
  (_, _, errs) -> ioError (userError (concat errs ++ usage))

-- TODO: make grammar configurable
-- grammar def start
ruleA = RuleNode [RandNode, RandNode, XNode, YNode]
ruleC = RuleNode
  [ruleA
  ,ruleA
  ,SinNode (AddNode ruleC ruleC)
  ,CosNode (AddNode ruleC ruleC)
  ,TanNode (AddNode ruleC ruleC)
  ,SinNode (MultNode ruleC ruleC)
  ,CosNode (MultNode ruleC ruleC)
  ,TanNode (MultNode ruleC ruleC)
 ]
ruleE = RuleNode [TripleNode ruleC ruleC ruleC]
grammar = [ruleA, ruleC, ruleE] :: Grammar
-- grammar def end

usage :: String
usage = "Usage:\n  randomart -h\n  randomart [--d <depth>] [--p <pixels>] [-o <filepath>] [-s <seed>]"

main :: IO ()
main = do
  flags <- getArgs >>= parseArgs
  putStrLn $ "Received flags: " ++ show flags
  let stdGen = mkStdGen (optSeed flags)
  let ast = fst (treeGen grammar ruleE (optDepth flags) stdGen)
  let evalAst (x, y) = nodeGet (nodeEval ast x y)
  let astImage = createImage (optPixels flags) evalAst
  writeImage (optOutput flags) astImage
