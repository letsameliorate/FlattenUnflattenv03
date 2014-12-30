module Printer where

import DTerm
import Text.PrettyPrint.HughesPJ

instance Show DTerm where
    show dt = render (prettyTerm dt)


blank = Text.PrettyPrint.HughesPJ.space


stripLambda (DLambda x dt) = let (xs, dt') = stripLambda dt
                             in ((x:xs), dt')
stripLambda dt = ([],dt)


prettyTerm (DFreeApp x dts) = if dts == []
                              then (text x)
                              else parens ((text x) <+> (hcat (punctuate blank (map prettyTerm dts))))
prettyTerm (DBoundApp i dts) = if dts == []
                               then (int i)
                               else parens ((int i) <+> (hcat (punctuate blank (map prettyTerm dts))))
prettyTerm (DConApp c dts) = if dts == []
                             then text c
                             else (text c) <> (parens (hcat (punctuate comma (map prettyTerm dts))))
prettyTerm dt@(DLambda _ _) = let (xs, dt') = stripLambda dt
                              in (text "\\") <> (hsep (map text xs)) <> (text ".") <> (prettyTerm dt')
prettyTerm (DLet x dt0 dt1) = parens (((text "let") <+> (text x) <+> (text "=") <+> (prettyTerm dt0)) $$ ((text "in") <+> (prettyTerm dt1)))
prettyTerm (DCase csel (b:bs)) = hang ((text "case") <+> (prettyTerm csel) <+> (text "of")) 1 (blank <+> (prettyBranch b) $$ (vcat (map (\b -> (text "|" <+>) (prettyBranch b)) bs)))
                                 where
                                     prettyBranch (c,[],dt) = (text c) <+> (text "->") <+> (prettyTerm dt)
                                     prettyBranch (c,xs,dt) = (text c) <> (parens (hcat (punctuate comma (map text xs)))) <+> (text "->") <+> (prettyTerm dt)
prettyTerm (DFunApp f dts) = if dts == []
                             then (text f)
                             else parens ((text f) <+> (hcat (punctuate blank (map prettyTerm dts))))
prettyTerm (DWhere dt dts) = (prettyTerm dt) $$ (text "where") $$ (prettyDef dts)
                             where
                                 prettyDef [(f,dt)] = (text f) <+> (text "=") <+> (prettyTerm dt)
                                 prettyDef ((f,dt):dts) = (text f) <+> (text "=") <+> (prettyTerm dt) <> semi $$ (prettyDef dts)


prettyPrint (Left a) = print a
prettyPrint (Right a) = print a
