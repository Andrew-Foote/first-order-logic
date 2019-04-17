import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Logic
import SolSet

instance Arbitrary Var where
    arbitrary = Var <$> arbitrary

instance Arbitrary Formula where
    arbitrary = oneof
        [ (:<) <$> arbitrary <*> arbitrary,
        return Bottom,
        (:->) <$> arbitrary <*> arbitrary,
        Forall <$> arbitrary <*> arbitrary ]

prop_solveSubst x y p q | uniquelySolvable (solveSubst x p q) = (subst x y p == q) == (solveSubst x p q == Sol y)
                        | unsolvable (solveSubst x p q) = subst x y p /= q
                        | indeterminate (solveSubst x p q) = subst x y p == q

prop_subst_free :: Var -> Var -> Formula -> Bool
prop_subst_free x y p = (free x $ subst x y p) == (x == y && free x p)

prop_freshVar :: Formula -> Bool
prop_freshVar p = bound (freshVar p) p

prop_detach :: Formula -> Formula -> Bool
prop_detach p q = detach (p :-> q) p == Just q

prop_k :: Formula -> Formula -> Bool
prop_k p q = axiomK $ p :-> q :-> p

prop_s :: Formula -> Formula -> Formula -> Bool
prop_s p q r = axiomS $ (p :-> q :-> r) :-> (p :-> q) :-> p :-> r

prop_n :: Formula -> Bool
prop_n p = axiomN $ neg (neg p) :-> p

prop_u :: Var -> Formula -> Formula -> Bool
prop_u x p q = axiomU $ Forall x (p :-> q) :-> Forall x p :-> Forall x q

prop_i :: Var -> Var -> Formula -> Bool
prop_i x y p = axiomI $ Forall x p :-> subst x y p

prop_g :: Formula -> Bool
prop_g p = axiomG $ p :-> Forall (freshVar p) p

tests =
    [ testGroup "Tests"
        [ testProperty "prop_solveSubst" prop_solveSubst,
        testProperty "prop_subst_free" prop_subst_free,
        testProperty "prop_freshVar" prop_freshVar,
        testProperty "prop_detach" prop_detach,
        testProperty "prop_k" prop_k,
        testProperty "prop_s" prop_s,
        testProperty "prop_n" prop_n,
        testProperty "prop_u" prop_u,
        testProperty "prop_i" prop_i,
        testProperty "prop_g" prop_g ] ]

main :: IO ()
main = defaultMain tests 