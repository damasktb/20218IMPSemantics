
------------------------- State

type Variable = String

type State = [(Variable,Integer)]

empty :: State
empty = []

set :: Variable -> Integer -> State -> State
set v n [] = [(v,n)]
set v n ((w,m):xs) | v == w = (v,n) : xs
                   | otherwise = (w,m) : set v n xs

get :: Variable -> State -> Integer
get v [] = 0
get v ((w,m):xs) | v == w = m
                 | otherwise = get v xs


------------------------- Sample program


factorial :: Comm
factorial = ("y" :=: Num 1) :>:
            While (Num 1 :<=: Var "x")
              ("y" :=: (Var "y" :*: Decr "x"))

runFactorial :: Integer -> Integer
runFactorial i = get "y" s
  where
    s = evalC factorial (set "x" i empty)




------------------------- Arithmetic expressions

data Aexp = Num Integer
          | Var Variable
          | Aexp :+: Aexp
          | Aexp :-: Aexp
          | Aexp :*: Aexp
          | Decr Variable
          | Incr Variable

evalA (Num n) s   = (s,n)
evalA (Var v) s   = (s,get v s)
evalA (Decr v) s = (set v x s, get v s)
                where (t,x) = evalA (Var v :-: Num 1) s
evalA (Incr v) s = (set v x s, get v s)
                where (t,x) = evalA (Var v :+: Num 1) s
evalA (a :+: b) s = (u,x+y)
                where
                  (t,x) = evalA a s
                  (u,y) = evalA b t
evalA (a :*: b) s = (u,x*y)
                where
                  (t,x) = evalA a s
                  (u,y) = evalA b t
evalA (a :-: b) s = (u,x-y)
                where
                  (t,x) = evalA a s
                  (u,y) = evalA b t


------------------------- Boolean expressions

data Bexp = Boolean Bool
          | Aexp :==: Aexp
          | Aexp :<=: Aexp
          | Neg Bexp
          | Bexp :&: Bexp
          | Bexp :|: Bexp

evalB (Boolean b) s = undefined
evalB (a :==: b)  s = undefined
evalB (a :<=: b)  s = undefined
evalB (Neg b)     s = undefined
evalB (a :&: b)   s = undefined
evalB (a :|: b)   s = undefined


------------------------- Commands

data Comm = Skip
          | Variable :=: Aexp
          | Comm :>: Comm
          | If Bexp Comm Comm
          | While Bexp Comm

evalC :: Comm -> State -> State
evalC Skip        s = s
evalC (v :=: a)   s = set v x t where (t,x) = evalA a s
evalC (c :>: d)   s = evalC d (evalC c s)
evalC (If b c d)  s | x         = evalC c t
                    | otherwise = evalC d t
                    where (t,x) = evalB b s
evalC (While b c) s | x         = evalC (While b c) (evalC c t) 
                    | otherwise = t
                    where (t,x) = evalB b s


