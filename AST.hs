data Node =
  Number Int |
  X |
  Y |
  Add Node Node |
  Mult Node Node |
  Triple Node Node Node |
  Null
  deriving Show

node_eval :: Node -> Node
node_eval (Number val) = Number val
node_eval X = X
node_eval Y = Y
node_eval (Add lhs rhs) =
  case ((node_eval lhs), (node_eval rhs)) of
    (Number first, Number second) -> Number (first + second)
    (_, _) -> Null
node_eval (Mult lhs rhs) =
  case ((node_eval lhs), (node_eval rhs)) of
    (Number first, Number second) -> Number (first * second)
    (_, _) -> Null
node_eval (Triple first second third) = Triple first second third
node_eval Null = Null

node_print :: Node -> String
node_print (Number val) = show val
node_print X = "X"
node_print Y = "Y"
node_print (Add lhs rhs) =
  "add(" ++
  (node_print lhs) ++
  "," ++
  (node_print rhs)
  ++
  ")"
node_print (Mult lhs rhs) =
  "mult(" ++
  (node_print lhs) ++
  "," ++
  (node_print rhs) ++
  ")"
node_print (Triple first second third) =
  "(" ++
  (node_print first) ++
  "," ++
  (node_print second) ++
  "," ++
  (node_print third) ++
  ")"
node_print Null = "NULL"
