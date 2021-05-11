{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}

module Algebra.Solver
    ( Expr(..)
    , (=*>), solveExpr
    , ExprEq
    , (=*=), solveEq
    ) where

import Data.Maybe
import Debug.Trace (trace)
import qualified Algebra.Field    as Field
import qualified Algebra.Field    as Alg ((/), recip)
import qualified Algebra.Ring     as Ring
import qualified Algebra.Ring     as Alg ((*), one)
import qualified Algebra.Additive as Additive
import qualified Algebra.Additive as Alg ((+), (-), negate, zero)

-- TODO: implement irrational numbers and functions (pi, sin, root, etc.)
data Expr v n
    = Const n
    | Var v
    | Neg (Expr v n) -- additive inverse
    | Rec (Expr v n) -- multiplicative inverse
    | Add (Expr v n) (Expr v n)
    | Mul (Expr v n) (Expr v n)
    deriving (Eq)

instance (Show v, Show n) => Show (Expr v n) where
    show e = case e of
        Const n -> show n
        Var v -> show v
        Neg e -> "neg("++ show e ++ ")"
        Rec e -> "rec("++ show e ++ ")"
        Add e1 e2 -> (show e1) ++ " + " ++ (show e2)
        Mul e1 e2 -> let
            e1' = if isSum e1 then "(" ++ (show e1) ++ ")" else show e1
            e2' = if isSum e2 then "(" ++ (show e2) ++ ")" else show e2
            in e1' ++ " * " ++ e2'
        where
            isSum (Add _ _) = True
            isSum _ = False


vars :: Expr v n -> [v]
vars expr = case expr of
    Const _ -> []
    Var v -> [v]
    Neg e -> vars e
    Rec e -> vars e
    Add e1 e2 -> (vars e1) ++ (vars e2)
    -- Sub e1 e2 -> (vars e1) ++ (vars e2)
    Mul e1 e2 -> (vars e1) ++ (vars e2)
    --Div e1 e2 -> (vars e1) ++ (vars e2)

consts :: Expr v n -> [n]
consts expr = case expr of
    Const c -> [c]
    Var _ -> []
    Neg e -> consts e
    Rec e -> consts e
    Add e1 e2 -> (consts e1) ++ (consts e2)
    -- Sub e1 e2 -> (consts e1) ++ (consts e2)
    Mul e1 e2 -> (consts e1) ++ (consts e2)
    --Div e1 e2 -> (consts e1) ++ (consts e2)


data Transform v n = Transform
    { name :: String
    , t :: Expr v n -> Maybe (Expr v n)
    }

-- TODO: product of sums (a + b)(c + d) <=> ac + ad + bc + bd

tCommute = Transform
        { name = "commute"
        , t = \e -> case e of
            Add e1 e2 -> Just $ Add e2 e1
            Mul e1 e2 -> Just $ Mul e2 e1
            _ -> Nothing
        }


tAssociate = Transform
    { name = "associate"
    , t = \e -> case e of
        Add e1 (Add e2 e3) -> Just $ Add (Add e1 e2) e3
        Mul e1 (Mul e2 e3) -> Just $ Mul (Mul e1 e2) e3
        _ -> Nothing
    }

tCancelNeg = Transform
    { name = "cancel negative"
    , t = \e -> case e of
        Neg (Neg e1) -> Just e1
        _ -> Nothing
    }

tMoveNegative = Transform
    { name = "MoveNegative"
    , t = \e -> case e of
        Mul (Neg e1) e2 -> Just $ Mul e1 (Neg e2)
        Mul e1 (Neg e2) -> Just $ Mul (Neg e1) e2
        _ -> Nothing
    }

tDistribute :: (Eq n, Eq v) => Transform v n
tDistribute = Transform
    { name = "distribute"
    , t = \e -> case e of
        Mul e1 (Add e2 e3) -> Just $ Add (Mul e1 e2) (Mul e1 e3)
        Add (Mul e1 e2) (Mul e3 e4) -> if e1 == e3
                                       then Just $ Mul e1 (Add e2 e4)
                                       else Nothing
        _ -> Nothing
    }

tFoldConstAdd :: Field.C n => Transform v n
tFoldConstAdd = Transform
    { name = "fold add"
    , t = \e -> case e of
        Add (Const n1) (Neg (Const n2)) -> Just $ Const $ n1 Alg.+ (Alg.negate n2)
        Add (Neg (Const n2)) (Const n1) -> Just $ Const $ n1 Alg.+ (Alg.negate n2)

        Add (Const n1) (Const n2) -> Just $ Const $ n1 Alg.+ n2
        Neg (Const n1) -> Just $ Const $ Alg.negate n1
        _ -> Nothing
    }

tFoldConstMul :: Field.C n => Transform v n
tFoldConstMul = Transform
    { name = "fold mul"
    , t = \e -> case e of
        Mul (Const n1) (Const n2) -> Just $ Const $ n1 Alg.* n2
        Rec (Const n1) -> Just $ Const $ Alg.recip n1
        _ -> Nothing
    }

tUnfoldConstAdd :: (Eq n, Eq v, Field.C n) => n -> Transform v n
tUnfoldConstAdd n = Transform
    { name = "unfold add"
    , t = \e -> case e of
        Const n1 -> Just $ Add (Add (Const n1) (Neg $ Const n)) (Const n)
        _ -> Nothing
    }

tUnfoldConstMul :: (Eq n, Eq v, Field.C n) => n -> Transform v n
tUnfoldConstMul n = Transform
    { name = "unfold mul"
    , t = \e -> case e of
        Const n1 -> Just $ Mul (Mul (Const n1) (Rec $ Const n)) (Const n)
        _ -> Nothing
    }


iterTransform :: (Expr v n -> Maybe (Expr v n))
               -> Expr v n
               -> [Expr v n]
iterTransform t expr = case expr of
    Const _ -> current
    Var _ -> current
    Neg e1 -> (Neg <$> r e1) ++ current
    Rec e1 -> (Rec <$> r e1) ++ current
    Add e1 e2 -> current ++ (flip Add e2 <$> r e1) ++ (Add e1 <$> r e2)
    -- Sub e1 e2 -> current ++ (flip Sub e2 <$> r e1) ++ (Sub e1 <$> r e2)
    Mul e1 e2 -> current ++ (flip Mul e2 <$> r e1) ++ (Mul e1 <$> r e2)
    -- Div e1 e2 -> current ++ (flip Div e2 <$> r e1) ++ (Div e1 <$> r e2)
    where
        r = iterTransform t
        current = case t expr of
            Just x -> [x]
            Nothing -> []


(=*>) :: (Eq v, Eq n, Field.C n)
      => Expr v n
      -> Expr v n
      -> Maybe [(Expr v n, String)]
(=*>) = solveExpr 50000

solveExpr :: (Eq v, Eq n, Field.C n)
       => Integer
       -> Expr v n
       -> Expr v n
       -> Maybe [(Expr v n, String)]
solveExpr n start goal =
    reverse <$> solveExpr' n transforms [[(start, "start")]] goal
    where
        consts' = (consts start) ++ (consts goal)
        unfold = concat $ map (flip map consts') [tUnfoldConstAdd, tUnfoldConstMul]
        transforms = [tCommute, tAssociate, tDistribute, tFoldConstAdd, tFoldConstMul] ++ unfold

solveExpr' :: (Eq v, Eq n) => Integer
            -> [Transform v n]
            -> [[(Expr v n, String)]]
            -> Expr v n
            -> Maybe [(Expr v n, String)]
solveExpr' _ _ [] _ = Nothing
solveExpr' n transforms (cur@((expr, _):_):queue) target
    | expr == target = Just cur
    | n == 0         = Nothing
    | otherwise      = solveExpr' (n-1) transforms next target
        where
            applyT (Transform {name, t}) =
                map (\x -> (x, name):cur) (iterTransform t expr)
            next = (++) queue $ concat $ map applyT transforms


type ExprEq v n = (Expr v n, Expr v n)

data TransformEq v n = TransformEq
    { name :: String
    , t :: ExprEq v n -> Maybe (ExprEq v n)
    }

exprTransToEqTrans :: Transform v n -> TransformEq v n
exprTransToEqTrans (Transform{name, t}) = TransformEq
    { name = name
    , t = \(e1, e2) -> fmap (\e1' -> (e1', e2)) (t e1)
    }

tEqFlip = TransformEq
    { name = "flip"
    , t = \(e1, e2) -> Just $ (e2, e1)
    }

tEqAdd = TransformEq
    { name = "move addition"
    , t = \e -> case e of
        (Add e1 e2, e3) -> Just $ (e1, Add (Neg e2) e3)
        _ -> Nothing
    }

tEqMul = TransformEq
    { name = "move multiplication"
    , t = \e -> case e of
        (Mul e1 e2, e3) -> Just $ (e1, Mul (Rec e2) e3)
        _ -> Nothing
    }


(=*=) :: (Eq v, Eq n, Field.C n)
       => Expr v n
       -> Expr v n
       -> Expr v n
       -> Maybe [(ExprEq v n, String)]
(=*=) = solveEq 50000

-- TODO: further deprioritize (un)folding
-- TODO: apply recursive fold after solution is found
solveEq :: (Eq v, Eq n, Field.C n)
       => Integer
       -> Expr v n
       -> Expr v n
       -> Expr v n
       -> Maybe [(ExprEq v n, String)]
solveEq n left right targetLeft =
    reverse <$> solveEq' n tEq tExpr unfold [[((left, right), "start")]] targetLeft
    where
        consts' = (consts left) ++ (consts right) ++ (consts targetLeft)
        unfold = concat $ map (flip map consts') [tUnfoldConstAdd, tUnfoldConstMul]
        tExpr = [tCommute, tAssociate, tDistribute, tFoldConstAdd, tFoldConstMul]
        tEq = [tEqFlip, tEqAdd, tEqMul]

solveEq' :: (Eq v, Eq n)
         => Integer
         -> [TransformEq v n]
         -> [Transform v n]
         -> [Transform v n]
         -> [[(ExprEq v n, String)]]
         -> Expr v n
         -> Maybe [(ExprEq v n, String)]
solveEq' n tEq tExpr unfold (cur@(((left, right),_):_):queue) target
 --   | (trace (show cur) left) == target = Just cur
    | left  == target = Just cur
 --   | (trace (show n) n) == 0             = Nothing
    | n == 0             = Nothing
    | otherwise          = solveEq' (n-1) tEq tExpr unfold next target
        where
            unfold' =
                if rem n 1000 == 0
                then unfold
                else []
            applyTExpr (Transform {name, t}) =
                map (\x -> ((x,  right), name):cur) (iterTransform t left)
            applyTEq (TransformEq {name, t}) =
                maybeToList $ fmap (\x -> (x, name):cur)  (t (left, right))
            next = queue ++
                (concat $ map applyTExpr (tExpr++unfold')) ++
                (concat $ map applyTEq tEq)

