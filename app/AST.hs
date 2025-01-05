module AST where
import Data.Fixed
import Data.List
import System.Random
import Control.Monad.Random

data Node =
  NumberNode Double         |
  BoolNode Bool             |
  XNode                     |
  YNode                     |
  AddNode Node Node         |
  MultNode Node Node        |
  ModNode Node Node         |
  TripleNode Node Node Node |
  GTNode Node Node          |
  GTENode Node Node         |
  LTNode Node Node          |
  LTENode Node Node         |
  IfNode Node Node Node     |
  NormNode Node             |
  NullNode                  |
  RuleNode [Node] Int       
  deriving Show

-- TODO: don't hardcode this
isTerminal :: Node -> Bool
isTerminal node = case node of
  RuleNode rules _ -> False
  NumberNode _ -> True
  BoolNode _ -> True
  XNode -> True
  YNode -> True
  NullNode -> True
  otherwise        -> case (elem False [isTerminal child | child <- getRules node]) of
    True -> False
    _    -> True

getArity :: Node -> Int
getArity node = case node of
  RuleNode rules _ -> length rules
  otherwise        -> 0

-- get rules from a RuleNode
getRules :: Node -> [Node]
getRules node = case node of
  (RuleNode rules _) -> rules
  _                  -> []

-- maps value [0,1] to [0, index]
randRange :: Int -> Double -> Int
randRange maxVal randVal = round $ randVal * (fromIntegral maxVal)

type Grammar = [Node]

-- grammar G = [r1,..., rn]
-- initial rule i
-- depth d
-- list of random numbers randNums
--
-- - Random number is selected based on depth
--
-- Example:
-- ghci> let ruleA = RuleNode [XNode, YNode]
-- ghci> let ruleC = RuleNode [ruleA, AddNode ruleC ruleC, MultNode ruleC ruleC]
-- ghci> let ruleE = [TripleNode ruleC ruleC ruleC]
--
-- non-terminal node: RuleNode [RuleNode [XNode, YNode] 2] 1
treeGen :: Grammar -> Node -> Int -> [Double] -> Node
treeGen grammar initialRule depth randNums
  | depth <= 0 =
    case () of
      () | isTerminal firstRule -> firstRule 
         | otherwise            -> firstRule -- TODO
         where firstRule = (getRules initialRule) !! 0
  | otherwise =
    case () of
      () | isTerminal curNode -> curNode
         | otherwise          -> case curNode of
          AddNode first second -> AddNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          MultNode first second -> MultNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          ModNode first second -> ModNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          TripleNode first second third ->
            TripleNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums) (treeGen grammar third (depth-1) randNums)
          GTNode first second -> GTNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          GTENode first second -> GTENode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          LTNode first second -> LTNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          LTENode first second -> LTENode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums)
          IfNode first second third ->
            IfNode (treeGen grammar first (depth-1) randNums) (treeGen grammar second (depth-1) randNums) (treeGen grammar third (depth-1) randNums)
          _ -> NullNode
         where curNode = (getRules initialRule) !! (randRange arity (randNums !! depth))
         where arity = getArity initialRule
      
  
-- randomList :: Int -> Int -> [Int]
-- randomList seed sz = randomRs (0, sz) (mkStdGen seed) :: [Int]

nodeEval :: Node -> Double -> Double -> Node
nodeEval (NumberNode val) _ _ = NumberNode val
nodeEval (BoolNode val) _ _ = BoolNode val
nodeEval XNode x _ = NumberNode x
nodeEval YNode _ y = NumberNode y
--nodeEval (RandNode seed) _ _ = NumberNode (head (take 1 (randomList seed)))
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
nodeEval (RuleNode rules idx) x y =
  nodeEval (rules!!idx) x y
  

nodeGet :: Node -> (Double, Double, Double)
nodeGet (TripleNode first second third) =
  case (first, second, third) of
    (NumberNode firstVal, NumberNode secondVal, NumberNode thirdVal) ->
      (firstVal, secondVal, thirdVal)
    (_, _, _) -> (0.0, 0.0, 0.0)

nodePrint :: Node -> String
nodePrint (NumberNode val) = show val
nodePrint (BoolNode val) = show val
nodePrint XNode = "X"
nodePrint YNode = "Y"
nodePrint (AddNode lhs rhs) =
  "add("          ++
  (nodePrint lhs) ++
  ", "            ++
  (nodePrint rhs) ++
  ")"
nodePrint (MultNode lhs rhs) =
  "mult("         ++
  (nodePrint lhs) ++
  ", "            ++
  (nodePrint rhs) ++
  ")"
nodePrint (ModNode lhs rhs) =
  (nodePrint lhs) ++ 
  " % "           ++
  (nodePrint rhs)
nodePrint (TripleNode first second third) =
  "("                ++
  (nodePrint first)  ++
  ", "               ++
  (nodePrint second) ++
  ", "               ++
  (nodePrint third)  ++
  ")"
nodePrint (GTNode lhs rhs) =
  (nodePrint lhs) ++
  " > "           ++
  (nodePrint rhs)
nodePrint (GTENode lhs rhs) =
  (nodePrint lhs) ++
  " >= "          ++
  (nodePrint rhs)
nodePrint (LTNode lhs rhs) =
  (nodePrint lhs) ++
  " < "           ++
  (nodePrint rhs)
nodePrint (LTENode lhs rhs) =
  (nodePrint lhs) ++
  " < "           ++
  (nodePrint rhs)
nodePrint (IfNode cond thenVal elseVal) =
  "if ("              ++
  (nodePrint cond)    ++
  ") then "           ++
  (nodePrint thenVal) ++
  " else "            ++
  (nodePrint elseVal)
nodePrint (NormNode node) =
  "norm( "         ++
  (nodePrint node) ++
  ")"
nodePrint NullNode = "NULL"
nodePrint (RuleNode rules idx) =
  intercalate " | " [(nodePrint a) | a <- rules]
