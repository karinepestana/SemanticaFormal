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
 BOOL -> case(iTipo e2) of
  BOOL -> BOOL 
  x->error ((show e2) ++ "Possui tipo " ++ (show x) ++ " Mas deveria possuir tipo Bool")
 x->error ((show e1) ++ "Possui tipo " ++ (show x) ++ " Mas deveria possuir tipo Bool")

iTipo (Leq e1 e2) = case (iTipo e1) of 
 INT -> case(iTipo e2) of
  INT -> BOOL 
  a->error ((show e2) ++ "Possui tipo " ++ (show a) ++ " Mas deveria possuir tipo Int")
 a->error ((show e1) ++ "Possui tipo " ++ (show a) ++ " Mas deveria possuir tipo Int")
 
 
main = return ()
  
exSoma :: E
exSoma = Soma (Soma (Num 3) (Num 2)) (Num 1)
-- Resultado: INT
exAnd :: E
exAnd = And TRUE FALSE
-- Resultado: BOOL
exLeq :: E
exLeq = Leq (Num 3) (Num 5)
-- Resultado: BOOL

exErro1 :: E
exErro1 = Soma (Num 3) TRUE

exErro2 :: E
exErro2 = And (Leq (Num 3) FALSE) TRUE

exErro3 :: E
exErro3 = Leq (Soma (Num 3) TRUE) (Num 6)

exErro4 :: E
exErro4 = And TRUE (Num 6)



