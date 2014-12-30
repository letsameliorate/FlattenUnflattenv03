module Main (main) where

import Parser
import Printer

main :: IO ()

main = do
          putStrLn ""
          prettyPrint (parseExpr "g xs f v where g = \\xs f v. case xs of Nil -> v | Cons(x1,xs1) -> f x1 (g xs1 f v)")
          putStrLn ""
          prettyPrint (parseExpr "case x of C1 -> Nil | C2 -> f xs where f = \\xs. case xs of Nil -> Nil | Cons(x1,xs1) -> Cons(x1, f xs1)")
          putStrLn ""
          prettyPrint (parseExpr "f x1 x2 (let v3 = x3 in v3)")
          putStrLn ""
          prettyPrint (parseExpr "g x where g = \\x. case x of C1 -> Nil | C2 -> f xs where f = \\xs. case xs of Nil -> Nil | Cons(x1,xs1) -> Cons(g x1, f xs1)")
          --prettyPrint (prettyTerm (DFreeApp "f" [DFreeApp "x1" [], DFreeApp "x2" [], DFreeApp "x3" []]))


