
module Data.ICalendar.Pretty where

import Data.ICalendar.Types
import Text.PrettyPrint.HughesPJ

iCalParam (k,vs) = text k <> char '=' <> brackets (hcat $ punctuate (char ',') (map text vs))

iCalProperty prop = text "P:" <> text (icpName prop)
                <+> (if null (icpParams prop)
                       then empty
                       else parens (hcat $ punctuate (char ',') (map iCalParam $ icpParams prop)))
                <+> char '='
                <+> text (icpValue prop)

iCalObject obj = text (icoName obj) <> char ':'
              $$ nest 5 (vcat $ map iCalProperty $ icoProperties obj)
              $$ nest 5 (vcat $ punctuate empty $ map iCalObject $ icoObjects obj)

