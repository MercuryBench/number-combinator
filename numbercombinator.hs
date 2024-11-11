import Control.Monad
import Data.List

data Termtree a = Lit a | Add (Termtree a) (Termtree a) | Sub (Termtree a) (Termtree a) | Mult (Termtree a) (Termtree a) | Div (Termtree a) (Termtree a)
--	deriving (Show)
instance (Show a) => Show (Termtree a) where
	show (Lit a) = show a
	show (Add s t) = "(" ++ show s ++ "+" ++ show t ++ ")"
	show (Sub s t) = "(" ++ show s ++ "-" ++ show t ++ ")"
	show (Mult s t) = "(" ++ show s ++ "*" ++ show t ++ ")"
	show (Div s t) = "(" ++ show s ++ "/" ++ show t ++ ")"

t = Add (Lit 3.0) (Div (Sub (Lit 1.0) (Lit 4.0)) (Lit 6.0))


eval :: (Fractional a, Num a) => Termtree a -> a
eval (Lit a)= a
eval (Add t1 t2) = eval t1 + eval t2
eval (Sub t1 t2) = eval t1 - eval t2
eval (Mult t1 t2) = eval t1 * eval t2
eval (Div t1 t2) = eval t1 / eval t2

groups :: [a] -> [([a], [a])]
groups xs = [(ys, zs)| n <- [1..(length(xs)-1)], let ys = take n xs,  let zs = drop n xs]

arb :: [a] -> [Termtree a]
arb [] = []
arb (x:[]) = [Lit x]
arb (x:y:[]) = [Add (Lit x) (Lit y), Sub (Lit x) (Lit y), Mult (Lit x) (Lit y), Div (Lit x) (Lit y)]
arb xs = do
	(t1, t2) <- groups xs
	s1 <- arb t1
	s2 <- arb t2
	op <- [Add, Sub, Mult, Div]
	return $ op s1 s2

allTermTrees :: [a] -> [Termtree a]
allTermTrees = join . map arb . permutations 

-- for testing purposes:
-- filter ((==24) . eval) $ allTermTrees [1, 3, 4, 6]
