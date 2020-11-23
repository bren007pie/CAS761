-- 2.1 Final encoding using type aliasing with repr aliased as int. Also recursively defined
-- arithmetic language by its value, or by a Haskell expression that computes that value. The computation is compositional
type Repr = Int

lit :: Int -> Repr -- identity mapping?
lit n = n

neg :: Repr -> Repr
neg e = - e

add :: Repr -> Repr -> Repr
add e1 e2 = e1 + e2


-- So this is kinda like a language and is using Haskell's evaluator through use of int

tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))
-- still equals 5

