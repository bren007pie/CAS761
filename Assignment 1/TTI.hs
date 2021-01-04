--  2.1 we're doing the initial and final encodings with a first-order unityped language
-- Typing out each of the language and interpreter to give it a try and make sure stuff works

-- Initial embedding as a GADT with recursive definition
data Exp = Lit Int 
    | Neg Exp 
    | Add Exp Exp


-- Interpreter with pattern matching for initial encoding, also defined recursively. 
--Deconverts it from an expression to an integer to evaluate it
eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e)  = - eval e
eval (Add a b) = eval a + eval b


-- Running example of 8 + ( - (1 + 2 ))
ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))
-- for some reason you need to call it, ex) eval ti1

--View Function showing that at first initial embedding looks more general with ability to cast type
view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "(-" ++ view e ++ ")"
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"