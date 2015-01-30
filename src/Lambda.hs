{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
module Lambda where

import Prelude hiding (foldr, maximum)

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
import           Data.Hashable
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Traversable
import           Data.Tuple

-- * Data Type

data Lambda a
    = Variable a
    | Lambda a (Lambda a)
    | Application (Lambda a) (Lambda a)
    deriving (Eq, Foldable, Functor, Show, Traversable)

foldLambda :: (a -> b) -> (a -> b -> b) -> (b -> b -> b) -> Lambda a -> b
foldLambda fv fl fa = go
  where
    go (Variable v)        = fv v
    go (Lambda v l)        = fl v (go l)
    go (Application l1 l2) = fa (go l1) (go l2)

intifyLambda :: (Eq a, Hashable a) => Lambda a -> (Lambda Int, IntMap a)
intifyLambda l = second (IM.fromList . map swap . H.toList . fst)
               $ runState (traverse intify l) (H.empty, 0)
  where
    intify :: (Eq a, Hashable a) => a -> State (HashMap a Int, Int) Int
    intify x = do
        (m, i) <- get
        case H.lookup x m of
            Just i' -> return i'
            Nothing -> put (H.insert x i m, i+1) >> return i

normalizeLambda :: Lambda Int -> IntMap a -> Maybe (Lambda a)
normalizeLambda x m = go x
  where
    go   = foldLambda fv fl fa
    fv v = Variable <$> IM.lookup v m
    fl v = liftA2 Lambda (IM.lookup v m)
    fa   = liftA2 Application

-- * Generic functions

substitute :: Eq a => a -> Lambda a -> Lambda a -> Lambda a
substitute o n = go
  where
    go p@(Variable v)      = if v /= o then p else n
    go p@(Lambda v l)      = if v == o then p else Lambda v (go l)
    go (Application l1 l2) = Application (go l1) (go l2)

substituteVar :: Eq a => a -> a -> Lambda a -> Lambda a
substituteVar o n = substitute o (Variable n)

rename :: Int -> Lambda Int -> Lambda Int
rename i l = let n = maximum l + 1 in substituteVar i n l

alpha :: Lambda Int -> Lambda Int
alpha l = case l of
    Application _ _ | (not.null) conflicts -> run (map rename conflicts) l
    _                                      -> l
  where
    run               = foldr (.) id
    Application l1 l2 = l
    free              = freeVariables l1
    bound             = boundVariables l2
    conflicts         = IS.toList (IS.intersection bound free)

beta :: Eq a => Lambda a -> Lambda a
beta = foldLambda fv fl fa
  where
    fv                 = Variable
    fl                 = Lambda
    fa (Lambda v x) x' = substitute v x' x
    fa l1 l2           = Application l1 l2

-- * Functions for bound / free variables

variables :: Lambda Int -> (IntSet, IntSet, IntSet)
variables l = (a, b, IS.difference a b)
  where
    a = allVariables l
    b = boundVariables l

allVariables :: Lambda Int -> IntSet
allVariables = foldLambda IS.singleton IS.insert IS.union

boundVariables :: Lambda Int -> IntSet
boundVariables = foldLambda (const IS.empty) (const . IS.singleton) IS.union

freeVariables :: Lambda Int -> IntSet
freeVariables = (\(_, _, x) -> x) . variables

{- {{{

-- * Generic functions

-- * Variable related functions
-- $ If you want to use more than one of the functions 'all', 'bound' and
-- 'unbound', you should consider using 'variables', because this allows
-- sharing of the expressions.

allVariables :: Lambda -> IntSet
allVariables (Variable v) = S.singleton v
allVariables (Lambda v x) = S.insert v (allVariables x)
allVariables (Application l r) = allVariables l `S.union` allVariables r

bound :: Lambda -> IntSet
bound (Variable _) = S.empty
bound (Lambda v _) = S.singleton v
bound (Application l r) = bound l `S.union` bound r

unbound :: Lambda -> IntSet
unbound = (\(_, _, a) -> a) . variables

variables :: Lambda -> (IntSet, IntSet, IntSet)
variables x = (all', bound', unbound')
  where
    all'     = allVariables x
    bound'   = bound x
    unbound' = all' `S.difference` bound'

reduce :: Eq v => Lambda v -> Lambda v
reduce (Variable v) = Variable v
reduce (Lambda v x) = Lambda v (reduce x)
reduce (Application (Lambda v x) right) = substitute v right x
reduce (Application left right) = Application left right

}}} -}

-- vim: set ts=4 sts=4 sw=4 et:
