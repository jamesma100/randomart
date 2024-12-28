module AST where
import Data.Fixed

data Node =
  NumberNode Double |
  BoolNode Bool |
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
  NullNode
  deriving Show

nodeEval :: Node -> Double -> Double -> Node
nodeEval (NumberNode val) _ _ = NumberNode val
nodeEval (BoolNode val) _ _ = BoolNode val
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
  "add(" ++
  (nodePrint lhs) ++
  ", " ++
  (nodePrint rhs)
  ++
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
