module Lambda.Pretty where

import Lambda

pretty :: (v -> String) -> Lambda v -> String
pretty f (Variable v) = f v
pretty f (Lambda v x) = concat ["(\\", f v, ".", pretty f x, ")"]
pretty f (Application x1 x2) = pretty f x1 ++ pretty f x2

-- vim: set ts=4 sts=4 sw=4 et:
