{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module Tagless where

class Lambda rep where
  lam      :: (rep a -> rep b)     -> rep (rep a -> rep b)
  app      :: rep (rep a -> rep b) -> rep a                -> rep b

class Boolean rep where
  bool     :: Bool                 -> rep Bool
  not_     :: rep Bool             -> rep Bool
  and_     :: rep Bool             -> rep Bool             -> rep Bool
  or_      :: rep Bool             -> rep Bool             -> rep Bool

class Ints rep where
  int      :: Int                  -> rep Int
  neg      :: rep Int              -> rep Int
  add      :: rep Int              -> rep Int              -> rep Int
  mult_    :: rep Int              -> rep Int              -> rep Int

class IntOrder rep where
  leq      :: rep Int              -> rep Int              -> rep Bool

class Conditional rep where
  if_      :: rep Bool             -> rep a                -> rep a        -> rep a

class Pairs rep where
  pair     :: rep a                -> rep b                 -> rep (a, b)
  fst_     :: rep (a, b)           -> rep a
  snd_     :: rep (a, b)           -> rep b

class Equality rep where
  eq_      :: Eq a => rep a        -> rep a                 -> rep Bool

-- Useful helper classes
class Liftable rep where
  lift :: a -> rep a

class Apply f where
  lift0 :: a                  -> f a
  lift1 :: (a -> b)           -> f a -> f b
  lift2 :: (a -> b -> c)      -> f a -> f b  -> f c
  lift3 :: (a -> b -> c -> d) -> f a -> f b  -> f c  -> f d

if__ :: Bool -> a -> a -> a
if__ c t e = if c then t else e

{- ---------------------------------------------------------------
 - Interpreter
 - ---------------------------------------------------------------
 -}
newtype R a = R a
unR :: R a -> a
unR (R x) = x

instance Apply R where
  lift0 a           = R a
  lift1 o e         = R $ o (unR e)
  lift2 o e1 e2     = R $ o (unR e1) (unR e2)
  lift3 o e1 e2 e3  = R $ o (unR e1) (unR e2) (unR e3)

instance Boolean R where
  bool    = lift0
  not_    = lift1 (not)
  and_    = lift2 (&&)
  or_     = lift2 (||)

instance Ints R where
  int     = lift0
  neg     = lift1 (negate)
  add     = lift2 (+)
  mult_   = lift2 (*)

instance IntOrder R where
  leq     = lift2 (<=)

instance Conditional R where
  if_  = lift3 (if__)

instance Pairs R where
  pair    = lift2 (,)
  fst_    = lift1 (fst)
  snd_    = lift1 (snd)

instance Lambda R where
  lam     = R
  app f x = unR f $ x

{- ---------------------------------------------------------------
 - Pretty Printer - String version
 - ---------------------------------------------------------------
 -}
data S a = S {unS :: Int -> String}

liftS0 :: Show a => a -> S a
liftS0 = S . const . show

liftS1 :: (String -> String) -> S a -> S b
liftS1 f a = S $ \n -> f (unS a n)

liftS2 :: (String -> String -> String) -> S a -> S b -> S c
liftS2 f a b = S $ \x -> f (unS a x) (unS b x)

liftS3 :: (String -> String -> String -> String) -> S a -> S b -> S c -> S d
liftS3 f a b c = S $ \x -> f (unS a x) (unS b x) (unS c x)

string0 :: Show a => a -> S a
string0 = liftS0

string1 :: String -> S a -> S b
string1 o = liftS1 (o ++)

string2 :: String -> S a -> S b -> S c
string2 o = let f = \a b -> "(" ++ a ++ o ++ b  ++ ")" in liftS2 f

instance Boolean S where
  bool      = string0
  not_      = string1 "!"
  and_      = string2 "&&"
  or_       = string2 "||"

instance Ints S where
  int       = string0
  neg       = string1 "-"
  add       = string2 "+"
  mult_     = string2 "*"

instance IntOrder S where
  leq       = string2 "<="

instance Conditional S where
  if_ = liftS3 (\a b c -> "(if " ++ a ++ " then " ++ b ++ " else " ++ c ++ ")")

instance Pairs S where
  pair      = string2 ","
  fst_      = string1 "[0]"
  snd_      = string1 "[1]"

instance Lambda S where
  lam f     = S $ \h -> let x = ("x" ++ (show h)) in "(\\" ++ x ++ " -> " ++ unS (f (S $ const x)) (succ h) ++ ")"
  app       = liftS2 (++)


--------------------------------------------------------------------------------------
prog_1 :: (Lambda rep, Boolean rep) => rep Bool
prog_1 = app (lam (\x -> x)) (bool True)

prog_2 :: (Lambda rep, Boolean rep) => rep Bool
prog_2 = app (lam (\x -> x)) (app (lam (\y -> y)) (bool True))

prog_3 :: (Lambda rep, Boolean rep) => rep Bool
prog_3 = app (lam (\x -> (and_ x x))) (bool True)

prog_4 :: (Lambda rep, Boolean rep) => rep Bool
prog_4 = app (lam (\x -> (and_ x x))) (bool False)

prog_5 :: (Boolean rep) => rep Bool
prog_5 = and_ (bool True) (bool False)

prog_6 :: (Boolean rep) => rep Bool
prog_6 = or_ (bool True) (bool False)

prog_7 :: (Boolean rep) => rep Bool
prog_7 = not_ (bool True)

prog_8 :: (Ints rep) => rep Int
prog_8 = (int 1)

prog_9 :: (Ints rep) => rep Int
prog_9 = mult_ (int 1) (int 200)

prog_10 :: (Ints rep, Lambda rep) => rep Int
prog_10 = app (lam (\x -> (mult_ x x))) (int 2)

prog_11 :: (IntOrder rep, Ints rep, Lambda rep) => rep Int
prog_11 = neg $ app (lam (\x -> (mult_ x x))) (int 2)

prog_12 :: (IntOrder rep, Ints rep, Lambda rep) => rep Bool
prog_12 = leq (int 10) (neg $ app (lam (\x -> (mult_ x x))) (int 2))

prog_13 :: (IntOrder rep, Ints rep, Lambda rep) => rep Bool
prog_13 = leq (neg $ app (lam (\x -> (mult_ x x))) (int 2)) (int 10)

prog_14 :: (Boolean rep, IntOrder rep, Ints rep, Lambda rep) => rep Bool
prog_14 = not_ $ leq (int 10) (neg $ app (lam (\x -> (mult_ x x))) (int 2))

prog_15 :: (Equality rep, Ints rep) => rep Bool
prog_15 = eq_ (int 1) (int 2)

prog_16 :: (Equality rep, Ints rep) => rep Bool
prog_16 = eq_ (int 1) (int 1)

prog_17 :: (Equality rep, Boolean rep) => rep Bool
prog_17 = eq_ (bool True) (bool True)

prog_18 :: (Equality rep, Boolean rep) => rep Bool
prog_18 = eq_ (bool True) (bool False)

prog_19 :: (Lambda rep, Ints rep) => rep Int
prog_19 = app (lam (\x -> (mult_ x (add x x)))) (int 2)

prog_20 :: (Conditional rep, Ints rep, Boolean rep) => rep Int
prog_20 = if_ (bool False) (int 20) (int 0)

prog_21 :: (Conditional rep, Ints rep, Equality rep) => rep Int
prog_21 = if_ (eq_ (add (int 12) (int 1)) (int 13)) (int 20) (int 0)

prog_22 :: (Pairs rep, Boolean rep, Ints rep) => rep (Bool, Int)
prog_22 = pair (bool True) (int 1)

prog_23 :: (Pairs rep, Boolean rep, Ints rep) => rep Bool
prog_23 = fst_ $ pair (bool True) (int 1)

prog_24 :: (Pairs rep, Boolean rep, Ints rep) => rep Int
prog_24 = snd_ $ pair (bool True) (int 1)

prog_25 :: (Lambda rep, Ints rep) => rep Int
prog_25 = app (lam (\x -> mult_ x x)) (app (lam (\x -> mult_ x x)) (int 2))

prog_26 :: (Lambda rep, Conditional rep, Equality rep, Ints rep) => rep Int
prog_26 = if_ (eq_ (add (int 1) (int 2)) (int 3)) (app (lam (\x -> (mult_ x x))) (int 3)) (int 0)

prog_27 :: (Conditional rep, Lambda rep, Equality rep, Ints rep) => rep Int
prog_27 = if_ prog_16 prog_25 prog_26

prog_28 :: (Ints rep, Lambda rep) => rep Int
prog_28 = app (lam (\x -> mult_ x (app (lam (\y -> y)) (int 1)))) (int 2)

prog_29 :: (Lambda rep, Ints rep) => rep (rep Int -> rep Int)
prog_29 = lam (\x -> mult_ x (app (lam (\y -> y)) (int 1)))

prog_30 :: (Boolean rep, Ints rep, Pairs rep) => rep Int
prog_30 = mult_ (int 3) (fst_ (pair (int 1) (bool False)))

prog_31 :: (Lambda rep) => rep (rep a -> rep a)
prog_31 = lam (\x -> x)

prog_32 :: (Lambda rep, Pairs rep) => rep (rep (a , b) -> rep (b, a))
prog_32 = lam (\x -> pair (snd_ x) (fst_ x))

prog_33 :: (Pairs rep) => rep (a, b) -> rep (b, a)
prog_33 = \x -> pair (snd_ x) (fst_ x)
