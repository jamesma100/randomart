module Render where

import Prelude as P
import Graphics.Image as I
import Data.Fixed

-- 
-- Renders an image given a pixel mapping function. Note that the mapping function must
-- map [-1, 1]^2 -> [-1, 1]^3 but the Graphics.Image package expects [0, dim]^2 -> [0, 1]^3
-- so the function must rescale the output from [-1, 1]^3 to [0, 1]^3. The input rescaling
-- is done by `mapOnto`.
--
-- Example usage:
-- ghci> let grayImage = createImage 200 grayGrad
-- ghci> writeImage "../images/gray.png" grayImage
--
-- Alternatively, instead of passing in a function, you can interpret an AST node.
-- ghci> let grayGradAST = TripleNode (MultNode (AddNode XNode (NumberNode 1.0)) (NumberNode 0.5)) (MultNode (AddNode XNode (NumberNode 1.0)) (NumberNode 0.5)) (MultNode (AddNode XNode (NumberNode 1.0)) (NumberNode 0.5))
-- ghci> evalGrayGradAST (x, y) = nodeGet (nodeEval grayGradAST x y)
-- let grayImage = createImage 200 evalGrayGradAST
-- ghci> writeImage "../images/gray.png" grayImage

-- normalizes pixel input [0, dim]^2 -> [-1, 1]^2 and flips (row, col) -> (x, y)
mapOnto :: Int -> (Int, Int) -> (Double, Double)
mapOnto dim (r, c) = (
  ((fromIntegral c) - halfDim) / halfDim,
  ((fromIntegral r) - halfDim) / halfDim)
  where halfDim = fromIntegral dim / 2.0

-- example function: gray gradient, normalizes output from [-1, 1]^3 to [0, 1]^3
grayGrad :: (Double, Double) -> (Double, Double, Double)
grayGrad (x, y) = ((x + 1) / 2.0, (x + 1) / 2.0, (x + 1) / 2.0)

-- example function: color gradient, normalizes output from [-1, 1]^3 to [0, 1]^3
colorGrad :: (Double, Double) -> (Double, Double, Double)
colorGrad (x, y)
  | x*y >= 0 = ((x + 1) / 2.0, (y + 1) / 2.0, 1.0)
  | otherwise = (((mod' x y) + 1) / 2.0, ((mod' x y) + 1) / 2.0, ((mod' x y) + 1) / 2.0)

-- creates RGB pixel, takes in function :: [-1, 1]^2 -> [0, 1]^3
mkPixel :: (Double, Double, Double) -> Pixel RGB Double
mkPixel (x, y, z) = PixelRGB x y z

createImage0 :: Int -> ((Double, Double) -> (Double, Double, Double)) -> Image VU RGB Double
createImage0 dim func = makeImageR VU (dim, dim) (mkPixel . func . mapOnto dim)

createImage :: Int -> ((Double, Double) -> (Double, Double, Double)) -> Image VU RGB Double
createImage dim func = fromLists [
  [(mkPixel . func . mapOnto dim) (i, j) | j <- [1..dim+1]]
  | i <- [1..dim+1]]
