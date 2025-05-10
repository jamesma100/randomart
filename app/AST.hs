module AST where
import Data.Fixed
import Data.List
import System.Random

-- TODO: try using GADTs here to separate nodes by their type s.t. it doesn't rely on
-- the user-provided grammar to enforce type safety
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
  ExpNode Double Node |
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
  SinNode a -> isRule a
  CosNode a -> isRule a
  TanNode a -> isRule a
  AddNode a b -> isRule a || isRule b
  MultNode a b -> isRule a || isRule b
  ModNode a b -> isRule a || isRule b
  ExpNode _ b -> isRule b
  TripleNode a b c -> isRule a || isRule b || isRule c
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
treeGen :: Grammar -> Node -> Int -> StdGen -> (Node, StdGen)
treeGen grammar initialRule depth stdGen
  | depth <= 0 && isTerminal ((getRules initialRule) !! 0) = 
    case (getRules initialRule) !! 0 of
      RandNode -> (NumberNode (randNum * 2 - 1), newGen) -- resolve random node
      otherwise -> ((getRules initialRule) !! 0, stdGen)
  | otherwise =
    case () of
      () | isTerminal curNode -> case curNode of
          RandNode -> (NumberNode (randNum * 2 - 1), newGen) -- resolve random node
          otherwise -> (curNode, newGen)
         | otherwise -> case curNode of
          SinNode a -> (SinNode (fst branch1), snd branch1)
            where branch1 = treeGen grammar a (depth-1) newGen
          CosNode a -> (CosNode (fst branch1), snd branch1)
            where branch1 = treeGen grammar a (depth-1) newGen
          TanNode a -> (TanNode (fst branch1), snd branch1)
            where branch1 = treeGen grammar a (depth-1) newGen
          AddNode a b -> (AddNode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar a (depth-1) newGen
                  branch2 = treeGen grammar b (depth-1) gen1
                  gen1 = (snd branch1)
          MultNode a b -> (MultNode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar a (depth-1) newGen
                  branch2 = treeGen grammar b (depth-1) gen1
                  gen1 = (snd branch1)
          ModNode a b -> (ModNode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar a (depth-1) newGen
                  branch2 = treeGen grammar b (depth-1) gen1
                  gen1 = (snd branch1)
          ExpNode base exp -> (ExpNode base (fst branch1), snd branch1)
            where branch1 = treeGen grammar exp (depth-1) newGen
          TripleNode a b c -> (TripleNode (fst branch1) (fst branch2) (fst branch3), snd branch3)
            where branch1 = treeGen grammar a (depth-1) newGen
                  branch2 = treeGen grammar b (depth-1) gen1
                  branch3 = treeGen grammar c (depth-1) gen2
                  gen1 = (snd branch1)
                  gen2 = (snd branch2)
          GTNode lhs rhs -> (GTNode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar lhs (depth-1) newGen
                  branch2 = treeGen grammar rhs (depth-1) gen1
                  gen1 = (snd branch1)
          GTENode lhs rhs -> (GTENode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar lhs (depth-1) newGen
                  branch2 = treeGen grammar rhs (depth-1) gen1
                  gen1 = (snd branch1)
          LTNode lhs rhs -> (LTNode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar lhs (depth-1) newGen
                  branch2 = treeGen grammar rhs (depth-1) gen1
                  gen1 = (snd branch1)
          LTENode lhs rhs -> (LTENode (fst branch1) (fst branch2), snd branch2)
            where branch1 = treeGen grammar lhs (depth-1) newGen
                  branch2 = treeGen grammar rhs (depth-1) gen1
                  gen1 = (snd branch1)
          IfNode ifExpr thenExpr elseExpr -> (IfNode (fst branch1) (fst branch2) (fst branch3), snd branch3)
            where branch1 = treeGen grammar ifExpr (depth-1) newGen
                  branch2 = treeGen grammar thenExpr (depth-1) gen1
                  branch3 = treeGen grammar elseExpr (depth-1) gen2
                  gen1 = (snd branch1)
                  gen2 = (snd branch2)
          RuleNode rules -> treeGen grammar (RuleNode rules) (depth-1) newGen
          _ -> (NullNode, stdGen)
         where curNode =
                if (depth > 0) then (getRules initialRule) !! (randRange (max 0 (arity-1)) randNum)
                else (getRules initialRule) !! 0
               arity = getArity initialRule
  where (randNum, newGen) = uniformR (0::Double, 1::Double) stdGen
      
  
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
    (NumberNode a, NumberNode b) -> NumberNode (a + b)
    (_, _) -> NullNode
nodeEval (MultNode lhs rhs) x y =
  case ((nodeEval lhs x y), (nodeEval rhs x y)) of
    (NumberNode a, NumberNode b) -> NumberNode (a * b)
    (_, _) -> NullNode
nodeEval (ModNode lhs rhs) x y =
  case (nodeEval lhs x y, nodeEval rhs x y) of
    (NumberNode a, NumberNode b) ->
      if b > 0 then NumberNode (mod' a b)
      else NumberNode 0
    (_, _) -> NullNode
nodeEval (ExpNode base exp) x y =
  case (nodeEval exp x y) of
    (NumberNode exp) -> NumberNode (base ** exp)
    _ -> NullNode
nodeEval (TripleNode a b c) x y =
  case (nodeEval a x y, nodeEval b x y, nodeEval c x y) of
    (NumberNode valA, NumberNode valB, NumberNode valC) ->
      TripleNode (NumberNode valA) (NumberNode valB) (NumberNode valC)
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
    (BoolNode condVal, NumberNode a, NumberNode b) ->
      if condVal then (NumberNode a)
      else (NumberNode b)
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
nodeGet (TripleNode a b c) =
  case (a, b, c) of
    (NumberNode valA, NumberNode valB, NumberNode valC) ->
      (valA, valB, valC)
    (_, _, _) -> (0.0, 0.0, 0.0)

nodePrint :: Node -> String
nodePrint (NumberNode val) = show val
nodePrint (BoolNode val) = show val
nodePrint RandNode = "rand(0, 1)"
nodePrint (SinNode a) =
  "sin(" ++
  (nodePrint a) ++
  ")"
nodePrint (CosNode a) =
  "cos(" ++
  (nodePrint a) ++
  ")"
nodePrint (TanNode a) =
  "tan(" ++
  (nodePrint a) ++
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
nodePrint (ExpNode base exp) =
  (show base) ++
  "^(" ++
  (nodePrint exp) ++ 
  ")"
nodePrint (TripleNode a b c) =
  "(" ++
  (nodePrint a) ++
  ", " ++
  (nodePrint b) ++
  ", " ++
  (nodePrint c) ++
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
