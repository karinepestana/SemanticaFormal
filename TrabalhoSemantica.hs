type Estado = [(String,Int)]

adiciona :: Estado -> String -> Int -> Estado
adiciona e s i = (s,i):e

procuraVar :: Estado -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
 | s == v     = i
 | otherwise  = procuraVar xs v

mudaVar :: Estado -> String -> Int -> Estado
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
 | s == v     = ((s,n):xs)
 | otherwise  = (s,i): mudaVar xs v n

data N = Num Int
 deriving(Show)

data AExp = N|Var String|Som AExp AExp|Sub AExp AExp|Mul AExp AExp
 deriving(Show)

data TF = TRUE|FALSE
 deriving(Show)

data BExp =	TF|Not BExp|And BExp BExp|Or BExp BExp|Ig AExp AExp
 deriving(Show)

data CExp = While BExp CExp| If BExp CExp CExp| Seq CExp CExp| Atrib AExp AExp|Skip
 deriving(Show)   
          

interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s) in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s) in (Som ef e2,s)

				
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s) in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s) = let (ef,_) = aSmallStep (e1, s) in (Sub ef e2,s)
					
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s) in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef,_) = aSmallStep (e1, s) in (Mul ef e2,s)


interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False

main = return ()

bSmallStep :: (BExp,Estado) -> (BExp,Estado)
bSmallStep (Not FALSE,s) = (TRUE,s)
bSmallStep (Not TRUE,s) = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s) in (Not bn,sn)
bSmallStep (And TRUE b2,s)	= (b2,s)
bSmallStep (And FALSE b2,s)	= (FALSE,s)
bSmallStep (And b1 b2,s) = let (bn,sn) = bSmallStep (b1,s) in (And bn b2,sn)
bSmallStep (Or TRUE b2,s) = (TRUE,s)
bSmallStep (Or FALSE b2,s) = (b2,s)
bSmallStep (Or b1 b2,s)	= let (bn,sn) = bSmallStep (b1,s) in (Or bn b2,sn)
					
bSmallStep (Ig (Num x) (Num y), s) = if x == y then (TRUE,s) else (FALSE,s)
bSmallStep (Ig (Num x) e2, s) = let (ef,_) = aSmallStep (e2,s) in (Ig (Num x) ef,s)
bSmallStep (Ig e1 e2,s) = let (ef,_) = aSmallStep (e1,s) in (Ig ef e2,s)					

interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False

cSmallStep :: (CExp,Estado) -> (CExp,Estado)

cSmallStep (Atrib (Var x) (Num n), s) = (Skip, (mudaVar s x n))
cSmallStep (Atrib (Var x) e, s) = let ((Num n),sn) = aSmallStep (e,s) in (Atrib (Var x) (Num n),sn)

cSmallStep (Seq Skip c, s) = cSmallStep(c, s)
cSmallStep (Seq c1 c2, s) = let (cn,sn) = cSmallStep (c1,s)	in (cSmallStep (Seq cn c2,sn))


cSmallStep (If TRUE c1 c2, s) = cSmallStep(c1,s)
cSmallStep (If FALSE c1 c2, s) = cSmallStep(c2,s)
cSmallStep (If b c1 c2, s) = let (bn,_) = bSmallStep(b,s) in (cSmallStep(If bn c1 c2, s))

cSmallStep (While b c, s) = cSmallStep(If b (Seq c (While b c)) Skip, s)

cSmallStep(Skip, s) = (Skip, s)


meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemploSom :: AExp
exemploSom = Som (Num 3) (Som (Var "x") (Var "y"))
exemploSub :: AExp
exemploSub = Sub (Num 5) (Sub (Var "x") (Var "y"))
exemploMul :: AExp
exemploMul = Mul (Num 5) (Mul (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemploAnd :: BExp
exemploAnd = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)
exemploOr :: BExp
exemploOr = Or (Or TRUE (Not FALSE)) (Or (Not (Not TRUE)) TRUE)
exemploIg :: BExp
exemploIg = Ig (Num 3) (Num 3)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

exemploAtrib :: CExp
exemploAtrib = Atrib (Var "y") (Num 3)
exemploSeq :: CExp
exemploSeq = Seq (Atrib (Var "y") (Num 3)) (Atrib (Var "z") (Num 2))
exemploIf :: CExp
exemploIf = If (And TRUE (Not TRUE)) (Atrib (Var "y") (Num 3)) (Atrib (Var "z") (Num 2))
exemploWhile :: CExp
exemploWhile = While (Ig (Var "y") (Var "z")) (Atrib (Var "y") (Num 3))

