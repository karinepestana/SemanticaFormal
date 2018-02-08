data E = Num Int | TRUE | FALSE | Soma E E | And E E | Leq E E
 deriving (Eq, Show)

data TIPO = INT | BOOL
 deriving(Eq, Show)
 
iTipo :: E -> TIPO
iTipo (Num n) = INT
iTipo TRUE = BOOL
iTipo FALSE = BOOL
iTipo (Soma e1 e2) = case (iTipo e1) of 
 INT -> case(iTipo e2) of
  INT -> INT 
  t->error ((show e2) ++ "Possui tipo " ++ (show t) ++ " Mas deveria possuir tipo Int")
 t->error ((show e1) ++ "Possui tipo " ++ (show t) ++ " Mas deveria possuir tipo Int")

  iTipo (And e1 e2) = case (iTipo e1) of 
 INT -> case(iTipo e2) of
  INT -> BOOL 
  t->error ((show e2) ++ "Possui tipo " ++ (show t) ++ " Mas deveria possuir tipo Int")
 t->error ((show e1) ++ "Possui tipo " ++ (show t) ++ " Mas deveria possuir tipo Int")

 iTipo (Leq e1 e2) = case (iTipo e1) of 
 INT -> case(iTipo e2) of
  INT -> BOOL 
  t->error ((show e2) ++ "Possui tipo " ++ (show t) ++ " Mas deveria possuir tipo Int")
 t->error ((show e1) ++ "Possui tipo " ++ (show t) ++ " Mas deveria possuir tipo Int")
 
 
main = return ()
  
ex1 :: E
ex1 = Soma (Soma (Num 3) (Num 2)) (Num 1)

exErro :: E
exErro = Soma (Num 3) TRUE