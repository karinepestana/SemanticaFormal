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

data AExp = Num Int|Var String|Som AExp AExp|Sub AExp AExp|Mul AExp AExp
 deriving(Show)

data BExp =	TRUE|FALSE|Not BExp|And BExp BExp|Or BExp BExp|Ig AExp AExp
 deriving(Show)

data CExp = While BExp CExp| If BExp CExp CExp| Seq CExp CExp| Atrib AExp AExp| Skip| Try CExp CExp CExp| Catch| Throw
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

cSmallStep (Seq Skip c, s) = (c, s)
cSmallStep (Seq Throw c, s) = (Throw, s)
cSmallStep (Seq c1 c2, s) = let (cn,sn) = cSmallStep (c1,s)	in (Seq cn c2,sn)


cSmallStep (If TRUE c1 c2, s) = (c1,s)
cSmallStep (If FALSE c1 c2, s) = (c2,s)
cSmallStep (If b c1 c2, s) = let (bn,_) = bSmallStep(b,s) in (If bn c1 c2, s)

cSmallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)

cSmallStep (Try Skip Catch c, s) = (Skip, s)
cSmallStep (Try Throw Catch c, s) = (c, s)
cSmallStep (Try c1 Catch c2, s) = let (cn, sn) = cSmallStep(c1, s) in (Try cn Catch c2, sn)

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemploSom :: AExp
exemploSom = Som (Num 3) (Som (Var "x") (Var "y"))
-- Resultado: 6
exemploSub :: AExp
exemploSub = Sub (Num 5) (Sub (Var "x") (Var "y"))
-- Resultado: 2
exemploMul :: AExp
exemploMul = Mul (Num 5) (Mul (Var "x") (Var "y"))
-- Resultado: 0

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemploAnd :: BExp
exemploAnd = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)
-- Resultado: TRUE
exemploOr :: BExp
exemploOr = Or (Or TRUE (Not FALSE)) (Or (Not (Not TRUE)) TRUE)
-- Resultado: TRUE
exemploIg :: BExp
exemploIg = Ig (Num 3) (Num 3)
-- Resultado: TRUE

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

exemploAtrib :: CExp
exemploAtrib = Atrib (Var "y") (Num 3)
-- Resultado: x=3; y=3;z=0
exemploSeq :: CExp
exemploSeq = Seq (Atrib (Var "y") (Num 3)) (Atrib (Var "z") (Num 2))
-- Resultado: x=3;y=3;z=2
exemploIf :: CExp
exemploIf = If (And TRUE (Not TRUE)) (Atrib (Var "y") (Num 3)) (Atrib (Var "z") (Num 2))
-- Resultado: x=3;y=0;z=2
exemploWhile :: CExp
exemploWhile = While (Ig (Var "y") (Var "z")) (Atrib (Var "y") (Num 3))
-- Resultado: x=3;y=3;z-0
exemploTry1 :: CExp
exemploTry1 = Try (Seq Throw(Atrib (Var "y")(Num 5)))Catch (While (Ig (Var "y") (Var "z")) (Atrib (Var "y") (Num 3)))
-- Resultado: x=3;y=3;z=0
exemploTry2 :: CExp
exemploTry2 = Try (Atrib (Var "z") (Som (Num 1)(Num 6))) Catch (Atrib (Var "x")(Num 1))
-- Resultado: x=3;y=0;z=7
exemploTry3 :: CExp
exemploTry3 = Try (Seq Skip (Atrib (Var "z")(Num 1))) Catch (Atrib (Var "x")(Num 2))
-- Resultado: x=3;y=0;z=1