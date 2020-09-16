module Main where
import Data.List

data RegExpr = Chaine String | Classe String | MotVide | EnsembleVide | Union [RegExpr] | Concat [RegExpr] | Star RegExpr

instance Show RegExpr where
  show (Chaine str) = str
  show (Classe str) = "[" ++ str ++ "]"
  show (MotVide) = "ε"
  show (EnsembleVide) = "∅"
  show (Union exprs) = "(" ++ (foldl (++) "" $ intersperse "|" $ map show exprs) ++ ")"
  show (Concat exprs) = foldl (++) "" $ map show exprs
  show (Star expr) = "(" ++ show expr ++ ")*"

repeatExpr :: Int -> RegExpr -> RegExpr
repeatExpr n expr = Concat [ expr | i <- [1..n] ]

test :: RegExpr -> String -> Bool
test (Chaine str) str' = str == str'
test (Classe str) (c : "") = c `elem` str
test (Classe str) _ = False
test MotVide "" = True
test MotVide _ = False
test EnsembleVide _ = False
test (Union exprs) str = or $ map (\expr -> test expr str) exprs
test (Concat []) "" = True
test (Concat []) _ = False
test (Concat exprs) str = or [
    test (head exprs) (take n str) && test (Concat $ tail exprs) (drop n str)
    | n <- [0..length str]
  ]

-- /#[a-f]{6}/
expr1 = Concat [
    Chaine "#",
    repeatExpr 6 hex_expr
  ] where hex_expr = Classe "0123456789abcdef"

-- /oui|non/
expr2 = Union [
    Chaine "oui",
    Chaine "non"
  ]

main :: IO ()
main = putStrLn "Hello, Haskell!"
