module Logic where
import SolSet

-- | A variable.
newtype Var = Var Int
    deriving (Eq, Ord, Show)

-- | A formula.
data Formula = Var :< Var -- ^ A membership relation.
             | Bottom -- ^ The bottom formula.
             | Formula :-> Formula -- ^ A conditional formula.
             | Forall Var Formula -- ^ A formula beginning with a universal
                                  -- quantifier.
    deriving (Eq, Show)
infixr 8 :->

-- | Attempt to apply detachment to two formulas.
detach :: Formula -- ^ The major premise (expected to be of the form @p :-> q@)
       -> Formula -- ^ The minor premise (expected to be of the form @p@)
       -> Maybe Formula -- A 'Maybe' containing the conclusion if the inference
                        -- is valid
detach (p :-> q) r | p == r = Just q
                   | otherwise = Nothing
detach _ _ = Nothing

-- | Test whether a formula is an instance of axiom K.
axiomK :: Formula -> Bool
axiomK (p :-> q :-> r) = p == r
axiomK _ = False

-- | Test whether a formula is an instance of axiom S.
axiomS :: Formula -> Bool
axiomS ((p :-> q :-> r) :-> (s :-> t) :-> u :-> v) = p == s && p == u &&
                                                     q == t && r == v
axiomS _ = False

-- | Test whether a formula is an instance of axiom N.
axiomN :: Formula -> Bool
axiomN (((p :-> Bottom) :-> Bottom) :-> q) = p == q
axiomN _ = False

-- | Test whether a formula is an instance of axiom U.
axiomU :: Formula -> Bool
axiomU (Forall x (p :-> q) :-> Forall y r :-> Forall z s) = x == y && x == z &&
                                                            p == r && q == s
axiomU _ = False

-- | Substitute one variable in place of another within a formula.
subst :: Var -- ^ The variable to be replaced
      -> Var -- ^ The variable to use in place
      -> Formula -- ^ The formula before the replacement
      -> Formula -- ^ The formula after the replacement
subst x y s@(a :< b)     | x == a && x == b = y :< y
                         | x == a = y :< b
                         | x == b = a :< y
                         | otherwise = s
subst x y Bottom = Bottom
subst x y (p :-> q) = subst x y p :-> subst x y q
subst x y s@(Forall a p) | x == a = s
                         | otherwise = Forall a (subst x y p)

-- Solve the equation 
--
-- > subst x y p = q
-- 
-- where @x@, @p@ and @q@ are the arguments in order.
solveSubst :: Var -- ^ The variable to be replaced
           -> Formula -- ^ The formula before the replacement
           -> Formula -- ^ The formula after the replacement
           -> SolSet Var -- ^ The solutions for the variable after the
                         -- replacement
solveSubst x (a :< b) (c :< d) | x == a && x == b && c == d = Sol c
                               | x == a && x == b && c /= d = NoSol
                               | x == a && b == d = Sol b
                               | x == a && b /= d = NoSol
                               | x == b && a == c = Sol d
                               | x == b && a /= c = NoSol
                               | a == c && b == d = IndetSol
                               | otherwise = NoSol
solveSubst x (a :< b) _ = NoSol
solveSubst x Bottom Bottom = IndetSol
solveSubst x Bottom _ = NoSol
solveSubst x (p :-> q) (r :-> s) = intersection (solveSubst x p r) (solveSubst x q s)
solveSubst x (p :-> q) _ = NoSol
solveSubst x (Forall a p) (Forall b q) | x == a && x == b = IndetSol
                                       | a == b = solveSubst x p q 
                                       | otherwise = NoSol

-- | Test whether a formula is an instance of axiom I.
axiomI :: Formula -> Bool
axiomI (Forall x p :-> q) = solvable $ solveSubst x p q
axiomI _ = False

-- | Test whether a variable is free in a formula.
free :: Var -> Formula -> Bool
free x (a :< b) | x == a = True
                | x == b = True
                | otherwise = False
free x Bottom = False
free x (p :-> q) = free x p || free x q
free x (Forall a p) = x /= a && free x p

-- | Test whether a variable is bound in a formula.
bound :: Var -> Formula -> Bool 
bound x p = not (free x p)

-- | Test whether a formula is an instance of axiom G.
axiomG :: Formula -> Bool
axiomG (p :-> Forall x q) = p == q && bound x p
axiomG _ = False

-- | Return a variable which is not free in the given formula.
freshVar :: Formula -> Var
freshVar (Var m :< Var n) = Var (max m n + 1)
freshVar Bottom = Var 0
freshVar (p :-> q) = max (freshVar p) (freshVar q)
freshVar (Forall x p) = freshVar p
