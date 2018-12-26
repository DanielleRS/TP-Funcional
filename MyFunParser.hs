module MyFunParser where

import Control.Applicative (some, (<|>))
import Parser
import Language

-- A parser for double
double :: Parser Double
double =
  do x <- integer
     y <- decimalPart
     return (fromIntegral x + y)
  where
    decimalPart =
      do char '.'
         rest <- some digit
         return (read rest / 10 ^ (length rest))
      <|>
      return 0

-- A parser for constant expressions
cte :: Parser Expr
cte = Cte <$> token double

bool :: Parser String
bool = token (string "true") <|> token (string "false")

cteBool :: Parser Expr
cteBool = Boolean <$> token bool

-- A parser for variable expressions
var :: Parser Expr
var = Var <$> token identifier

-- A parser for atomic expressions, that is, the simplest expressions,
-- with the highest precedence
atomic :: Parser Expr
atomic = cte <|>
         var <|>
         token (char '(') *> expr <* token (char ')')

-- A parser for a binary operator
binop :: String -> (a -> a -> a) -> Parser (a -> a -> a)
binop name function = token (string name) *> pure function

-- A parser for unary operations
unrop :: String -> (a -> a) -> Parser (a -> a)
unrop name function = token (string name) *> pure function

-- A parser for power
pow :: Parser Expr
pow = chainl1 atomic (binop "**" (Bin Pow))

-- A parser for multiplication and division
mul :: Parser Expr
mul = chainl1 pow (binop "*" (Bin Mul) <|> binop "/" (Bin Div))

-- A parser for addition and subtraction
add :: Parser Expr
add = chainl1 mul (binop "+" (Bin Add) <|> binop "-" (Bin Sub))

-- A parser for Boolean operations
boolExpr :: Parser Expr
boolExpr = chainl1 add (binop "&&" (Bin And) <|> binop "||" (Bin Or) <|> binop "==" (Bin Eq) <|> binop "/=" (Bin Neq) <|> binop ">=" (Bin Gte) <|> binop "<=" (Bin Lte) <|> binop ">" (Bin Gt) <|> binop "<" (Bin Lt) <|> binop "!" (Bin Neg))

neg :: Parser Expr
neg = do
  token (char '-')
  ex <- expr
  return (Bin Neg ex ex)

no :: Parser Expr
no = do
  token (char '~')
  ex <- expr
  return (Bin Not ex ex)

-- A parser for expressions
expr :: Parser Expr
expr = neg <|> no <|> boolExpr

-- A parser for assignment command
assign :: Parser Cmd
assign = Assign <$> token identifier <*
                    token (string ":=") <*>
                    expr

-- A parser for print command
printcmd :: Parser Cmd
printcmd = Print <$> (token (string "print") *> expr)

-- A parser for if(<expr>) then <c1> else <c2> command
ifcmd :: Parser Cmd
ifcmd = do token (string "if")
           e <- expr
           token (string "then")
           c1 <- cmd
           token (string "else")
           c2 <- cmd
           return (Conditional e c1 c2)

-- A parser for while(<expr>) <c1> command
whilecmd :: Parser Cmd
whilecmd = do token (string "while")
              e <- expr
              command <- cmd
              return (While e command)

-- A parser for read command: read <var> -> gets value of <var> with getLine
readcmd :: Parser Cmd
readcmd = do token (string "read")
             id <- token identifier
             aux <- token spaces
             e <- token double
             return (Read id (Left e))

-- A parser for a sequence of commands separated by ';': {<c1>; <c2>; <cn>;}
seqcmd :: Parser Cmd
seqcmd = token (char '{') *> montagem <* token (char ';') <* token (char '}')
    where
      montagem = Sequence <$> chainl1 ((:[]) <$> cmd) (token (char ';') *> pure (++))


-- A parser for command
cmd :: Parser Cmd
cmd = assign <|> readcmd <|> printcmd <|> ifcmd <|> seqcmd <|> whilecmd
