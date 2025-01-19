module Main where

import Control.Monad
import Prelude as P
import Graphics.Image as I
import System.Random
import AST
import Render
import System.Environment
import System.Exit
import Data.Char

usage :: IO ()
usage = do
  putStrLn "Usage:\n  randomart -h\n  randomart <filepath> <seed>"

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1)
    (do putStrLn "Error: incorrect arguments."
        usage
        exitFailure)
  when (head args == "-h")
    (do usage
        exitSuccess)
  when (length args /= 2)
    (do putStrLn "Error: incorrect arguments."
        usage
        exitFailure)
  let filePath = args !! 0
  let seed = P.sum $ P.map ord (args !! 1)
  let stdGen = mkStdGen seed

  let ruleA = RuleNode [RandNode, XNode, YNode]
  let ruleC = RuleNode [ruleA,
                        ruleA,
                        AddNode ruleC ruleC,
                        AddNode ruleC ruleC,
                        AddNode ruleC ruleC,
                        MultNode ruleC ruleC,
                        MultNode ruleC ruleC,
                        MultNode ruleC ruleC]
  let ruleE = RuleNode [TripleNode ruleC ruleC ruleC]
  let grammar = [ruleA, ruleC, ruleE] :: Grammar
  
  let depth = 24
  let dim = 200
  let ast = fst (treeGen grammar ruleE depth stdGen)
  let evalAst (x, y) = nodeGet (nodeEval ast x y)
  let astImage = createImage dim evalAst
  writeImage filePath astImage
