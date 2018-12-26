module Language where

-- The type of expressions
-- TODO: power (right associative)
-- TODO: relational operators (==, !=, >, >=, <, <=)
-- TODO: logical operators (&&, ||)
-- TODO: unnary operator for changing the sign (-)
-- TODO: unnary operator for logical negation (!)
data Expr = Cte Double
          | Boolean String
          | Var String
          | Bin Op Expr Expr
          | Unr Op Expr
          deriving (Show)

data Op = Add | Sub | Mul
        | Div | Not | Pow
        | And | Or  | Eq
        | Neq | Gt  | Lt
        | Gte | Lte | Neg
        deriving (Show)

data Slot a = Value a
              deriving (Show)

-- The type of commands
-- TODO: read command: read <var>
-- TODO: block (sequence) command: { <c1> ; <c2> ; ... ; <cn> } (n >= 0)
-- TODO: conditional command: if <expr> then <cmd> else <cmd>
-- TODO: repetion command: while <expr> do <cmd>
data Cmd = Assign String Expr
         | Print Expr
         | Sequence [Cmd]
         | Conditional Expr Cmd Cmd
         | Read String (Either Double Bool)
         | While Expr Cmd
         deriving (Show)

getBoolean :: Either Double Bool -> Bool
getBoolean (Right x) = x
getBoolean (Left x) = x == 1.0

getDouble :: Either Double Bool -> Double
getDouble (Left x) = x
getDouble (Right x) = if x then 1.0 else 0
