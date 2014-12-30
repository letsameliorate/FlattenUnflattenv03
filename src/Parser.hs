module Parser where

import DTerm
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language


potDef = emptyDef
         { commentStart     = "{-|",
           commentEnd       = "|-}",
           commentLine      = "--",
           nestedComments   = True,
           identStart       = lower,
           identLetter      = do alphaNum <|> oneOf "_'",
           reservedNames    = ["case", "of", "let", "in", "where"],
           caseSensitive    = True
         }


{-|
    Parser of Pot to DTerm
|-}

lexer = T.makeTokenParser potDef

symbol      = T.symbol lexer
bracks      = T.parens lexer
semic       = T.semi lexer
comm        = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
natural     = T.natural lexer


list2ConsList [] = DConApp "Nil" []
list2ConsList (t:ts) = DConApp "Cons" [t, (list2ConsList ts)]

conName = do
             c <- upper
             cs <- many alphaNum
             return (c:cs)

makeWhere dt [] = dt
makeWhere dt fs = let (fnames, _) = unzip fs
                  in makeFuns fnames (DWhere dt fs)

makeFuns fnames (DFreeApp x dts) = if x `elem` fnames
                                   then DFunApp x (map (makeFuns fnames) dts)
                                   else DFreeApp x (map (makeFuns fnames) dts)
makeFuns fnames (DBoundApp i dts) = DBoundApp i (map (makeFuns fnames) dts)
makeFuns fnames (DConApp c dts) = DConApp c (map (makeFuns fnames) dts)
makeFuns fnames (DLambda x dt) = DLambda x (makeFuns fnames dt)
makeFuns fnames (DLet x dt0 dt1) = DLet x (makeFuns fnames dt0) (makeFuns fnames dt1)
makeFuns fnames (DCase csel bs) = DCase (makeFuns fnames csel) (map (\(c, xs, dt) -> (c, xs, makeFuns fnames dt)) bs)
makeFuns fnames (DFunApp f dts) = DFunApp f (map (makeFuns fnames) dts)
makeFuns fnames (DWhere dt fs) = DWhere (makeFuns fnames dt) (map (\(f, dt) -> (f, makeFuns fnames dt)) fs)


{-|
    Parsers
|-}

parseExpr = parse expr "(ERROR)"

expr = buildExpressionParser prec term

prec = []

term =     do
              x <- identifier
              as <- many atom
              fs <-     do
                           reserved "where"
                           fs <- sepBy1 fundef semic
                           return fs
                    <|> do
                           spaces
                           return []
              return (makeWhere (DFreeApp x as) fs)
       <|> do
              c <- conName
              es <-     do
                           es <- bracks (sepBy1 expr comm)
                           return es
                    <|> do
                           spaces
                           return []
              return (DConApp c es)
       <|> do
              symbol "\\"
              xs <- many1 identifier
              symbol "."
              e <- expr
              return (foldr (\x t -> (DLambda x t)) e xs)
       <|> do
              reserved "let"
              x <- identifier
              symbol "="
              e0 <- expr
              reserved "in"
              e1 <- expr
              return (DLet x e0 e1)
       <|> do
              reserved "case"
              e <- expr
              reserved "of"
              bs <- sepBy1 branch (symbol "|")
              return (DCase e bs)
       <|> do
              a <- atom
              return a

fundef =   do
              f <- identifier
              symbol "="
              e <- expr
              return(f, e)

atom =     do
              x <- identifier
              return (DFreeApp x [])
       <|> do
              symbol "["
              ts <- sepBy expr comm
              symbol "]"
              return (list2ConsList ts)
       <|> do
              e <- bracks expr
              return e

branch =   do
              c <- conName
              xs <-    do
                          xs <- bracks (sepBy1 identifier comm)
                          return xs
                   <|> do
                          spaces
                          return []
              symbol "->"
              e <- expr
              return (c, xs, e)


