{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Maybe
import Data.String
import Test.Tasty
import Test.Tasty.HUnit
import Algebra.Solver

main :: IO ()
main = defaultMain $ testGroup "solver" [exprSolver, eqSolver]

vA = Var "A"
vB = Var "B"
vC = Var "C"

exprSolver :: TestTree
exprSolver = testGroup "expr" $ map mkExprTest
    [ (vA, vA)
    , (Add vA vB, Add vB vA)
    , (Add vA (Const 12), Add (Const 12) vA)
    , (Add vA (Const 12), Add (Add (Const 1) (Const 11)) vA)
    , (Add (Add (Const 1) (Const 11)) vA, Add vA (Const 12))
    ]

eqSolver :: TestTree
eqSolver = testGroup "eq" $ map mkEqTest
    [ (vA, vB, vA, vB)
    , (vA, vB, vB, vA)
    , ( Add (Const 1) vA, vB
      , vA, Add (Neg (Const 1)) vB)
    , ( Add (Const 1) vA, vB
      , Add (Const 2) vA, Add (Neg $ Add (Const 1) (Neg $ Const 2)) vB)
    ]

type TestExpr = Expr PrettyString Rational

newtype PrettyString = Ps String
    deriving (Eq)

instance Show PrettyString where
    show (Ps x) = x

instance IsString PrettyString where
    fromString = Ps



mkExprTest :: (TestExpr, TestExpr) -> TestTree
mkExprTest (e1, e2) = testCase ((show e1) ++ " =*> " ++ (show e2)) $
    if isJust $ e1 =*> e2
    then return ()
    else assertFailure "could not find equivalence"

mkEqTest :: (TestExpr, TestExpr, TestExpr, TestExpr) -> TestTree
mkEqTest (e1, e2, targetLeft, expectRight) = testCase
    ((show e1) ++ " =*= " ++ (show e2) ++
        " $ " ++ (show targetLeft) ++ " /= " ++ (show expectRight)) $
        case e1 =*= e2 $ targetLeft of
            Nothing -> assertFailure "could not find solution"
            Just e3 -> let (_, e3' :: TestExpr) = fst $ head $ (reverse e3 :: [((TestExpr, TestExpr), String)])
                       in if e3' /= expectRight
                          then assertFailure $ "incorrect solution " ++ (show e3')
                          else return ()


