module AST where
import Data.Fixed
import Data.List
import System.Random
import Control.Monad.Random

data Node =
  NumberNode Double |
  BoolNode Bool |
  RandNode |
  SinNode Node |
  CosNode Node |
  TanNode Node |
  XNode |
  YNode |
  AddNode Node Node |
  MultNode Node Node |
  ModNode Node Node |
  TripleNode Node Node Node |
  GTNode Node Node |
  GTENode Node Node |
  LTNode Node Node |
  LTENode Node Node |
  IfNode Node Node Node | 
  NormNode Node |
  NullNode |
  RuleNode [Node]
  deriving Show

-- A rule is terminal if none of its children are rules
isTerminal :: Node -> Bool
isTerminal node = not (isRule node) && not (elem True [isRule child | child <- getRules node])

isRule :: Node -> Bool
isRule node = case node of
  RuleNode _ -> True
  SinNode first -> isRule first
  CosNode first -> isRule first
  TanNode first -> isRule first
  AddNode first second -> isRule first || isRule second
  MultNode first second -> isRule first || isRule second
  ModNode first second -> isRule first || isRule second
  TripleNode first second third -> isRule first || isRule second || isRule third
  GTNode lhs rhs -> isRule lhs || isRule rhs
  GTENode lhs rhs -> isRule lhs || isRule rhs
  LTNode lhs rhs  -> isRule lhs || isRule rhs
  LTENode lhs rhs -> isRule lhs || isRule rhs
  IfNode condExpr thenExpr elseExpr -> isRule condExpr || isRule thenExpr || isRule elseExpr
  otherwise -> False


getArity :: Node -> Int
getArity node = case node of
  RuleNode rules -> length rules
  otherwise -> 0

-- get rules from a Node
getRules :: Node -> [Node]
getRules node = case node of
  RuleNode rules -> rules
  _ -> [node]

-- maps value [0,1] to [0, index]
randRange :: Int -> Double -> Int
randRange maxVal randVal = round $ randVal * (fromIntegral maxVal)

type Grammar = [Node]

-- grammar G = [r1,..., rn]
-- initial rule i
-- depth d
-- list of random numbers randNums with length >= d+1
--
-- - Random number is selected based on depth
--
treeGen :: Grammar -> Node -> Int -> [Double] -> Int -> (Node, Int)
treeGen grammar initialRule depth randNums randIdx
  | depth <= 0 && isTerminal ((getRules initialRule) !! 0) = 
    case (getRules initialRule) !! 0 of
      RandNode -> (NumberNode ((randNums !! randIdx) * 2 - 1), randIdx + 1) -- resolve random node
      otherwise -> ((getRules initialRule) !! 0, randIdx+1)
  | otherwise =
    case () of
      () | isTerminal curNode -> case curNode of
          RandNode -> (NumberNode ((randNums !! randIdx) * 2 - 1), randIdx + 1) -- resolve random node
          otherwise -> (curNode, randIdx+1)
         | otherwise -> case curNode of
          SinNode first -> (SinNode (fst firstBranch), snd firstBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1)
          CosNode first -> (CosNode (fst firstBranch), snd firstBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1)
          TanNode first -> (TanNode (fst firstBranch), snd firstBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1)
          AddNode first second -> (AddNode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar second (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          MultNode first second -> (MultNode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1) 
                  secondBranch = treeGen grammar second (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          ModNode first second -> (ModNode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1) 
                  secondBranch = treeGen grammar second (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          TripleNode first second third -> (TripleNode (fst firstBranch) (fst secondBranch) (fst thirdBranch), snd thirdBranch)
            where firstBranch = treeGen grammar first (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar second (depth-1) randNums firstIdx
                  thirdBranch = treeGen grammar third (depth-1) randNums secondIdx
                  firstIdx = (snd firstBranch)
                  secondIdx = (snd secondBranch)
          GTNode lhs rhs -> (GTNode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar lhs (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar rhs (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          GTENode lhs rhs -> (GTENode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar lhs (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar rhs (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          LTNode lhs rhs -> (LTNode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar lhs (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar rhs (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          LTENode lhs rhs -> (LTENode (fst firstBranch) (fst secondBranch), snd secondBranch)
            where firstBranch = treeGen grammar lhs (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar rhs (depth-1) randNums firstIdx
                  firstIdx = (snd firstBranch)
          IfNode ifExpr thenExpr elseExpr -> (IfNode (fst firstBranch) (fst secondBranch) (fst thirdBranch), snd thirdBranch)
            where firstBranch = treeGen grammar ifExpr (depth-1) randNums (randIdx+1)
                  secondBranch = treeGen grammar thenExpr (depth-1) randNums firstIdx
                  thirdBranch = treeGen grammar elseExpr (depth-1) randNums secondIdx
                  firstIdx = (snd firstBranch)
                  secondIdx = (snd secondBranch)
          RuleNode rules -> treeGen grammar (RuleNode rules) (depth-1) randNums (randIdx+1)
          _ -> (NullNode, randIdx)
         where curNode =
                if (depth > 0) then (getRules initialRule) !! (randRange (max 0 (arity-1)) (randNums !! (randIdx)))
                else (getRules initialRule) !! 0
               arity = getArity initialRule
      
  
-- randomList :: Int -> Int -> [Int]
-- randomList seed sz = randomRs (0, sz) (mkStdGen seed) :: [Int]

nodeEval :: Node -> Double -> Double -> Node
nodeEval (NumberNode val) _ _ = NumberNode val
nodeEval (BoolNode val) _ _ = BoolNode val
nodeEval (SinNode expr) x y =
  case (nodeEval expr x y) of
    (NumberNode val) -> NumberNode (sin val)
    _ -> NullNode
nodeEval (CosNode expr) x y =
  case (nodeEval expr x y) of
    (NumberNode val) -> NumberNode (cos val)
    _ -> NullNode
nodeEval (TanNode expr) x y =
  case (nodeEval expr x y) of
    (NumberNode val) -> NumberNode (tan val)
    _ -> NullNode
nodeEval XNode x _ = NumberNode x
nodeEval YNode _ y = NumberNode y
nodeEval (AddNode lhs rhs) x y =
  case ((nodeEval lhs x y), (nodeEval rhs x y)) of
    (NumberNode first, NumberNode second) -> NumberNode (first + second)
    (_, _) -> NullNode
nodeEval (MultNode lhs rhs) x y =
  case ((nodeEval lhs x y), (nodeEval rhs x y)) of
    (NumberNode first, NumberNode second) -> NumberNode (first * second)
    (_, _) -> NullNode
nodeEval (ModNode lhs rhs) x y =
  case (nodeEval lhs x y, nodeEval rhs x y) of
    (NumberNode first, NumberNode second) -> NumberNode (mod' first second)
    (_, _) -> NullNode
nodeEval (TripleNode first second third) x y =
  case (nodeEval first x y, nodeEval second x y, nodeEval third x y) of
    (NumberNode a, NumberNode b, NumberNode c) ->
      TripleNode (NumberNode a) (NumberNode b) (NumberNode c)
    (_, _, _) -> NullNode
nodeEval (GTNode lhs rhs) x y = 
  case (nodeEval lhs x y, nodeEval rhs x y) of
    (NumberNode a, NumberNode b) ->
      if a > b then BoolNode True
      else BoolNode False
    (_, _) -> NullNode
nodeEval (GTENode lhs rhs) x y = 
  case (nodeEval lhs x y, nodeEval rhs x y) of
    (NumberNode a, NumberNode b) ->
      if a >= b then BoolNode True
      else BoolNode False
    (_, _) -> NullNode
nodeEval (LTNode lhs rhs) x y = 
  case (nodeEval lhs x y, nodeEval rhs x y) of
    (NumberNode a, NumberNode b) ->
      if a < b then BoolNode True
      else BoolNode False
    (_, _) -> NullNode
nodeEval (LTENode lhs rhs) x y = 
  case (nodeEval lhs x y, nodeEval rhs x y) of
    (NumberNode a, NumberNode b) ->
      if a <= b then BoolNode True
      else BoolNode False
    (_, _) -> NullNode
nodeEval (IfNode condExpr thenExpr elseExpr) x y =
  case (nodeEval condExpr x y, nodeEval thenExpr x y, nodeEval elseExpr x y) of
    (BoolNode condVal, TripleNode a b c, TripleNode i j k) ->
      if condVal then (TripleNode a b c)
      else (TripleNode i j k)
    (_, _, _) -> NullNode
--normalizes [-1, 1] -> [0, 1]
nodeEval (NormNode node) x y =
  case (nodeEval node x y) of
    (NumberNode val) -> (NumberNode ((val + 1) / 2.0))
    _ -> NullNode
nodeEval NullNode _ _ = NullNode
-- nodeEval (RuleNode rules idx) x y =
--   nodeEval (rules!!idx) x y
  

nodeGet :: Node -> (Double, Double, Double)
nodeGet (TripleNode first second third) =
  case (first, second, third) of
    (NumberNode firstVal, NumberNode secondVal, NumberNode thirdVal) ->
      (firstVal, secondVal, thirdVal)
    (_, _, _) -> (0.0, 0.0, 0.0)

nodePrint :: Node -> String
nodePrint (NumberNode val) = show val
nodePrint (BoolNode val) = show val
nodePrint RandNode = "rand(0, 1)"
nodePrint (SinNode first) =
  "sin(" ++
  (nodePrint first) ++
  ")"
nodePrint (CosNode first) =
  "cos(" ++
  (nodePrint first) ++
  ")"
nodePrint (TanNode first) =
  "tan(" ++
  (nodePrint first) ++
  ")"
nodePrint XNode = "X"
nodePrint YNode = "Y"
nodePrint (AddNode lhs rhs) =
  "add(" ++
  (nodePrint lhs) ++
  ", " ++
  (nodePrint rhs) ++
  ")"
nodePrint (MultNode lhs rhs) =
  "mult(" ++
  (nodePrint lhs) ++
  ", " ++
  (nodePrint rhs) ++
  ")"
nodePrint (ModNode lhs rhs) =
  (nodePrint lhs) ++ 
  " % " ++
  (nodePrint rhs)
nodePrint (TripleNode first second third) =
  "(" ++
  (nodePrint first) ++
  ", " ++
  (nodePrint second) ++
  ", " ++
  (nodePrint third) ++
  ")"
nodePrint (GTNode lhs rhs) =
  (nodePrint lhs) ++
  " > " ++
  (nodePrint rhs)
nodePrint (GTENode lhs rhs) =
  (nodePrint lhs) ++
  " >= " ++
  (nodePrint rhs)
nodePrint (LTNode lhs rhs) =
  (nodePrint lhs) ++
  " < " ++
  (nodePrint rhs)
nodePrint (LTENode lhs rhs) =
  (nodePrint lhs) ++
  " < " ++
  (nodePrint rhs)
nodePrint (IfNode cond thenVal elseVal) =
  "if (" ++
  (nodePrint cond) ++
  ") then " ++
  (nodePrint thenVal) ++
  " else " ++
  (nodePrint elseVal)
nodePrint (NormNode node) =
  "norm( " ++
  (nodePrint node) ++
  ")"
nodePrint NullNode = "NULL"
nodePrint (RuleNode rules) =
  intercalate " | " [(nodePrint a) | a <- rules]
