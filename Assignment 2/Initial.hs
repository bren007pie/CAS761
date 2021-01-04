{-# OPTIONS_GHC -Wall #-}
-- Model solution for A1.  Borrows from Jason's solution.

data Var = VZ | VS Var                         -- de Bruijn indices
  deriving Show

data Exp = V Var
         | B Bool                              -- Boolean literal
         | I Int                               -- Integer literal
         | L Exp                               -- Abstraction
         | A Exp Exp                           -- Application
         | AND_ Exp Exp                        -- AND
         | OR_ Exp Exp                         -- OR
         | NOT_ Exp                            -- NOT
         | ADD_ Exp Exp                        -- Addition
         | MULT_ Exp Exp                       -- Multiplication
         | NEG_ Exp                            -- Negation
         | LEQ_ Exp Exp                        -- Less than or equal to
         | LT_ Exp Exp                         -- Less than or equal to
         | EQ_ Exp Exp                         -- Equal to
         | IF_ Exp Exp Exp                     -- If-then-else
         | PAIR_ Exp Exp                       -- pair
         | FST_ Exp                            -- fst
         | SND_ Exp                            -- snd
  deriving Show

-- Lookup in the environment
lookp :: Var -> [p] -> p
lookp VZ []    = error "ran out of data for lookp VZ"
lookp VZ (x:_) = x
lookp (VS _) []      = error "ran out of data for lookp VS"
lookp (VS v) (_:env) = lookp v env

-- Universal type
--                           Pair    Function
data U = UB Bool | UI Int | UP U U | UA (U -> U)

-- For equality
instance Eq U where
  (UB b) == (UB b')      = b == b'
  (UI i) == (UI i')      = i == i'
  (UP a b) == (UP a' b') = a == a' && b == b'
  (UA _) == _            = error "cannot compare function for equality"
  _ == (UA _)            = error "cannot compare function for equality"
  _ == _                 = False

-- For debugging
instance Show U where
    show (UB x)   = "UB " ++ show x
    show (UI x)   = "UI " ++ show x
    show (UP a b) = "UP (" ++ show a ++ ", " ++ show b ++ ")"
    show (UA _)   = "UA <fun>"

-- The evaluator is more easily written via lots of helper functions
apply :: [U] -> U -> Exp -> U
apply env (UA f) e = f $ eval env e
apply _   e0     _ = error $ "cannot apply a " ++ show e0

fst_, snd_ :: U -> U
fst_ (UP a _) = a
fst_ x        = error $ "cannot take fst of not-a-pair " ++ show x
snd_ (UP _ b) = b
snd_ x        = error $ "cannot take snd of not-a-pair " ++ show x

bool1b :: (Bool -> Bool) -> U -> U
bool1b f (UB a) = UB $ f a
bool1b _ _      = error "boolean operation applied to non-boolean"

bool2b :: (Bool -> Bool -> Bool) -> U -> U -> U
bool2b f (UB a) (UB b) = UB $ f a b
bool2b _ _      _      = error "boolean operation applied to non-boolean"

int2b :: (Int -> Int -> Bool) -> U -> U -> U
int2b f (UI a) (UI b) = UB $ f a b
int2b _ _      _      = error "integer operation applied to non-integer"

int1i :: (Int -> Int) -> U -> U
int1i f (UI a) = UI $ f a
int1i _ _      = error "integer operation applied to non-integer"

int2i :: (Int -> Int -> Int) -> U -> U -> U
int2i f (UI a) (UI b) = UI $ f a b
int2i _ _      _      = error "integer operation applied to non-integer"

if_ :: [U] -> U -> Exp -> Exp -> U
if_ env (UB b) e1 e2 = eval env $ if b then e1 else e2
if_ _   x      _  _  = error $ "cannot choose on a non-boolean " ++ show x
eval :: [U] -> Exp -> U
eval env (V v)          = lookp v env
eval _   (B b)          = UB b
eval _   (I i)          = UI i
eval env (L e)          = UA (\x -> eval (x:env) e)
eval env (A e1 e2)      = apply env (eval env e1) e2
eval env (AND_ e1 e2)   = bool2b (&&) (eval env e1) (eval env e2)
eval env (OR_ e1 e2)    = bool2b (||) (eval env e1) (eval env e2)
eval env (NOT_ e)       = bool1b not (eval env e)
eval env (ADD_ e1 e2)   = int2i (+) (eval env e1) (eval env e2)
eval env (MULT_ e1 e2)  = int2i (*) (eval env e1) (eval env e2)
eval env (NEG_ e)       = int1i negate (eval env e)
eval env (LEQ_ e1 e2)   = int2b (<=) (eval env e1) (eval env e2)
eval env (LT_ e1 e2)    = int2b (<) (eval env e1) (eval env e2)
eval env (EQ_ e1 e2)    = UB $ eval env e1 == eval env e2
eval env (IF_ e1 e2 e3) = if_ env (eval env e1) e2 e3
eval env (PAIR_ e1 e2)  = UP (eval env e1) (eval env e2)
eval env (FST_ e)       = fst_ $ eval env e
eval env (SND_ e)       = snd_ $ eval env e

-- some useful functions for pretty-printing
parens :: String -> String
parens s = "(" ++ s ++ ")"

binop :: String -> [String] -> Exp -> Exp -> String
binop s env a b = parens $ view env a ++ s ++ view env b

unop, unop' :: String -> [String] -> Exp -> String
unop  s env a = s ++ id     (view env a)
unop' s env a = s ++ parens (view env a)

-- a view function. Note how the environment here is different than
-- for eval, it's not a list of U but of String!
view :: [String] -> Exp -> String
view env (V v) = lookp v env
view _   (B b) = show b
view _   (I i) = show i
view env (L e) =
  let l = show $ length env
  in "\\x" ++ l ++ " -> " ++ view (("x" ++ l):env) e
view env (A e1 e2)      = binop ") ("   env e1 e2
view env (AND_ e1 e2)   = binop " AND " env e1 e2
view env (OR_ e1 e2)    = binop " OR "  env e1 e2
view env (NOT_ e)       = unop' "NOT "  env e
view env (ADD_ e1 e2)   = binop " + "   env e1 e2
view env (MULT_ e1 e2)  = binop " * "   env e1 e2
view env (NEG_ e)       = unop' "- "    env e
view env (LEQ_ e1 e2)   = binop " <= "  env e1 e2
view env (LT_ e1 e2)    = binop " < "   env e1 e2
view env (EQ_ e1 e2)    = binop " == "  env e1 e2
view env (PAIR_ e1 e2)  = binop " , "   env e1 e2
view env (FST_ e)       = unop  "fst "  env e
view env (SND_ e)       = unop  "snd "  env e
-- this is an odd special case
view env (IF_ e1 e2 e3) = "(IF " ++ view env e1 ++ " THEN " ++ view env e2 ++  " ELSE " ++ view env e3 ++ ")"


-----------------------------------------------------------------------------------
-- testing area

prog_1, prog_2, prog_3, prog_4, prog_5, prog_6, prog_7, prog_8, prog_9,
  prog_10, prog_11, prog_12, prog_13, prog_14, prog_15, prog_16, prog_17,
  prog_18, prog_19, prog_20, prog_21, prog_22, prog_23, prog_24, prog_25,
  prog_26, prog_27, prog_28 :: Exp

prog_1 = A (L (V VZ)) (B True)
prog_2 = A (L (V VZ)) (A (L (V VZ)) (B True))
prog_3 = A (L (AND_ (V VZ) (V VZ))) (B True)
prog_4 = A (L (AND_ (V VZ) (V VZ))) (B False)
prog_5 = AND_ (B True) (B False)
prog_6 = OR_ (B True) (B False)
prog_7 = NOT_ (B True)
prog_8 = I 1
prog_9 = MULT_ (I 1) (I 200)
prog_10 = A (L (MULT_ (V VZ) (V VZ))) (I 2)
prog_11 = NEG_ $ A (L (MULT_ (V VZ) (V VZ))) (I 2)
prog_12 = LEQ_ (I 10) (NEG_ $ A (L (MULT_ (V VZ) (V VZ))) (I 2))
prog_13 = LEQ_ (NEG_ $ A (L (MULT_ (V VZ) (V VZ))) (I 2)) (I 10)
prog_14 = NOT_ $ LEQ_ (I 10) (NEG_ $ A (L (MULT_ (V VZ) (V VZ))) (I 2))
prog_15 = EQ_ (I 1) (I 2)
prog_16 = EQ_ (I 1) (I 1)
prog_17 = EQ_ (B True) (B True)
prog_18 = EQ_ (B True) (B False)
prog_19 = A (L (MULT_ (V VZ) (ADD_ (V VZ) (V VZ)))) (I 2)
prog_20 = IF_ (B False) (I 20) (I 0)
prog_21 = IF_ (EQ_ (ADD_ (I 12) (I 1)) (I 13)) (I 20) (I 0)
prog_22 = PAIR_ (B True) (I 1)
prog_23 = FST_ $ PAIR_ (B True) (I 1)
prog_24 = SND_ $ PAIR_ (B True) (I 1)
prog_25 = A (L $ MULT_ (V VZ) (V VZ)) (A (L $ MULT_ (V VZ) (V VZ)) (I 2))
prog_26 = IF_ (EQ_ (ADD_ (I 1) (I 2)) (I 3)) (A (L (MULT_ (V VZ) (V VZ))) (I 3)) (B False)
prog_27 = IF_ prog_16 prog_25 prog_26
prog_28 = A (L (MULT_ (V VZ) (A (L (V VZ)) (I 1)))) (I 2)


-- pre-filling env with empty env for view and eval functions
eval_ :: Exp -> U
eval_ prog = eval [] prog
view_ :: Exp -> String
view_ prog = view [] prog

main :: IO ()
main = do
       print (eval_ prog_1, view_ prog_1)
       print (eval_ prog_2, view_ prog_2)
       print (eval_ prog_3, view_ prog_3)
       print (eval_ prog_4, view_ prog_4)
       print (eval_ prog_5, view_ prog_5)
       print (eval_ prog_6, view_ prog_6)
       print (eval_ prog_7, view_ prog_7)
       print (eval_ prog_8, view_ prog_8)
       print (eval_ prog_9, view_ prog_9)
       print (eval_ prog_10, view_ prog_10)
       print (eval_ prog_11, view_ prog_11)
       print (eval_ prog_12, view_ prog_12)
       print (eval_ prog_13, view_ prog_13)
       print (eval_ prog_14, view_ prog_14)
       print (eval_ prog_15, view_ prog_15)
       print (eval_ prog_16, view_ prog_16)
       print (eval_ prog_17, view_ prog_17)
       print (eval_ prog_18, view_ prog_18)
       print (eval_ prog_19, view_ prog_19)
       print (eval_ prog_20, view_ prog_20)
       print (eval_ prog_21, view_ prog_21)
       print (eval_ prog_22, view_ prog_22)
       print (eval_ prog_23, view_ prog_23)
       print (eval_ prog_24, view_ prog_24)
       print (eval_ prog_25, view_ prog_25)
       print (eval_ prog_26, view_ prog_26)
       print (eval_ prog_27, view_ prog_27)
       print (eval_ prog_28, view_ prog_28)
