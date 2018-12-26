module Interpreter where

import Language

-- A type to represent the memory, that is, that association of
-- variable names with values
type Memory = [(String, Either Double Bool)]

-- initial memory
initialMemory :: Memory
initialMemory = []

-- Expression evaluation
eval :: Memory -> Expr -> Either Double Bool
eval _ (Boolean x) = Right (x == "true")

eval _ (Cte x) = Left x


eval m (Var v) =
  case lookup v m of
    Just x -> x
    Nothing -> Left 0

eval m (Bin op x y) =
  case op of
    Neg -> Left (negate dx)
    Not -> Right (not bx)
    Add -> Left (dx + dy)
    Sub -> Left (dx - dy)
    Mul -> Left (dx * dy)
    Div -> Left (dx / dy)
    Pow -> Left (dx ** dy)

    And -> Right (bx && by)
    Or  -> Right (bx || by)

    Eq  -> Right (dx == dy)
    Neq -> Right (dx /= dy)
    Gt ->  Right (dx > dy)
    Lt ->  Right (dx < dy)
    Gte -> Right (dx >= dy)
    Lte -> Right (dx <= dy)
  where
    dx = getDouble(eval m x)
    dy = getDouble(eval m y)

    bx = getBoolean(eval m x)
    by = getBoolean(eval m y)

    getBoolean (Right x) = x
    getBoolean (Left x) = x == 1.0

    getDouble (Left x) = x
    getDouble (Right x) = if x then 1.0 else 0

-- Command execution
execute :: Memory -> Cmd -> IO Memory
execute m (Assign v e) = return ((v, eval m e) : m)
execute m (Print e) = do print (eval m e)
                         return m
execute m (Sequence lista) = sequencia m lista
  where
    sequencia m [] = return m
    sequencia m (x:xs) = do newMemory <- execute m x
                            sequencia newMemory xs

execute m (Read str x) = do
  return ((str, x) : m)

execute m (Conditional e c1 c2) = do
    let bool = getBoolean (eval m e)
    if bool then execute m c1 else execute m c2

execute m (While e c1) = repeticao m e c1
  where
    repeticao m e c1 = do
      let bool = getBoolean (eval m e)
      if bool then do
        newMemory <- execute m c1
        repeticao newMemory e c1
        else return m
