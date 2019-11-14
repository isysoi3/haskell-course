data NuOperator = Push

data UnOperator = Negate 
                | Sqare
                | Sqrt

data BinOperator = Plus  
                 | Minus
                 | Divide
                 | Mult 

nuOperatorMap :: Floating a => NuOperator -> [a] -> [a]
nuOperatorMap Push stack = (head stack):stack

unOperatorMap :: Floating a => UnOperator -> (a -> a)
unOperatorMap Negate = (negate)
unOperatorMap Sqare = (^2)
unOperatorMap Sqrt = (sqrt)

binOperatorrMap :: Floating a => BinOperator -> (a -> a -> a)
binOperatorrMap Plus = (+)
binOperatorrMap Minus = (-)
binOperatorrMap Divide = (/)
binOperatorrMap Mult = (*)

data Operator = UnOperator UnOperator
              | BinOperator BinOperator
              | NuOperator NuOperator 

semantic :: Floating a => Operator -> [a] -> [a]
semantic (BinOperator op) (x1:x2:xs) = ((binOperatorrMap op) x1 x2):xs
semantic (BinOperator op) (_:[]) = error "No data to execute instuction: needed two value in stack"  
semantic (UnOperator op) (x:xs) = ((unOperatorMap op) x):xs
semantic (UnOperator op) [] = error "No data to execute instuction: needed one value in stack"
semantic (NuOperator op) xs = xs

data Instruction v = Operator Operator
                   | Value v

execute :: Floating a => [Instruction a] -> a
execute_helper [] [] = error "No result" 
execute_helper (x:[]) [] = x
execute_helper _ [] = error "No operators to manipulate with all data"  
execute_helper stack (i:is) = case i of 
    Value x -> execute_helper (x:stack) is 
    Operator op -> execute_helper (semantic op stack) is
execute = execute_helper []