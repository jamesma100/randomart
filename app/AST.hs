data Node =
  Number Float |
  X |
  Y |
  Add Node Node |
  Mult Node Node |
  Triple Node Node Node |
  Null
  deriving Show

node_eval :: Node -> Float -> Float -> Node
node_eval (Number val) _ _ = Number val
node_eval X x _ = Number x
node_eval Y _ y = Number y
node_eval (Add lhs rhs) x y =
  case ((node_eval lhs x y), (node_eval rhs x y)) of
    (Number first, Number second) -> Number (first + second)
    (_, _) -> Null
node_eval (Mult lhs rhs) x y =
  case ((node_eval lhs x y), (node_eval rhs x y)) of
    (Number first, Number second) -> Number (first * second)
    (_, _) -> Null
node_eval (Triple first second third) x y =
  case (node_eval first x y, node_eval second x y, node_eval third x y) of
    (Number a, Number b, Number c) -> Triple (Number a) (Number b) (Number c)
    (_, _, _) -> Null
node_eval Null _ _ = Null

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
