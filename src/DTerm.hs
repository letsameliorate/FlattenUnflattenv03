module DTerm where

{-|
    Distilled Expression
|-}

data DTerm = DFreeApp String [DTerm] -- Free Variable Application
           | DBoundApp Int [DTerm] -- Bound Variable Application
           | DConApp String [DTerm] -- Constructor Application
           | DLambda String DTerm -- Lambda Abstraction
           | DLet String DTerm DTerm -- Let Expression
           | DCase DTerm [(String, [String], DTerm)] -- Case Expression
           | DFunApp String [DTerm] -- Function Application
           | DWhere DTerm [(String, DTerm)] -- Local Function Definition
  --deriving (Show)


instance Eq DTerm where
    (==) (DFreeApp x dts) (DFreeApp x' dts') = (x == x') && (dts == dts')
    (==) (DBoundApp i dts) (DBoundApp i' dts') = (i == i') && (dts == dts')
    (==) (DConApp c dts) (DConApp c' dts') = (c == c') && (dts == dts')
    (==) (DLambda x dt) (DLambda x' dt') = (x == x') && (dt == dt')
    (==) (DLet x dt0 dt1) (DLet x' dt0' dt1') = (dt0 == dt0') && (dt1 == dt1')
    (==) dt@(DCase csel bs) dt'@(DCase csel' bs') | match dt dt' = (csel == csel') && (all (\((c,xs,bt), (c',xs',bt')) -> (bt == bt')) (zip bs bs'))
    (==) (DFunApp f dts) (DFunApp f' dts') = (f == f') && (dts == dts')
    (==) (DWhere dt dts) (DWhere dt' dts') = (dt == dt') && (dts == dts')
    (==) dt dt1 = False


match (DFreeApp x dts) (DFreeApp x' dts') = (x == x') && (length dts == length dts')
match (DBoundApp i dts) (DBoundApp i' dts') = (i == i') && (length dts == length dts')
match (DConApp c dts) (DConApp c' dts') = (c == c') && (length dts == length dts')
match (DLambda x dt) (DLambda x' dt') = True
match (DLet x dt0 dt1) (DLet x' dt0' dt1') = True
match (DCase csel bs) (DCase csel' bs') = (length bs == length bs') && (all (\((c,xs,bt), (c',xs',bt')) -> ((c == c') && (length xs == length xs'))) (zip bs bs'))
match (DFunApp f dts) (DFunApp f' dts') = (f == f') && (length dts == length dts')
match (DWhere dt dts) (DWhere dt' dts') = (dts == dts')
match dt dt' = False

