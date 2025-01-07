module Main where

--import Control.Monad.Random
import Prelude as P
import Graphics.Image as I
import System.Random
import AST
import Render

randList :: Int -> [Double]
randList seed = randoms (mkStdGen seed) :: [Double]


-- Ideas for getting random rule using IO Monad
--
-- 1. generate sizable list of random values beforehand [0, 1] - has to be done w/i IO monad
-- 2. pass into type constructor, then
--    a. select a random number from list
--    b. use `randRange` to map to [0, arity-1], getting an index
--    c. select grammar rule using index
main :: IO ()
main = do
  let seed = 52
  let randNums = take 100 $ randList seed
  --let tree = (treeGen grammar ruleE 8 randNums 0)
  putStrLn "hello world"
