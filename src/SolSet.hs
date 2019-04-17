module SolSet where

-- | A set which is either empty (if constructed by 'NoSol'), has a single
-- member (if constructed by 'Sol'), or contains everything of type 'a' (if
-- constructed by 'IndetSol').
data SolSet a = NoSol | Sol a | IndetSol

-- Test whether a 'SolSet' is nonempty.
solvable :: SolSet a -> Bool
solvable NoSol = False
solvable (Sol x) = True
solvable IndetSol = False

-- | Intersect two 'SolSets'.
intersection :: (Eq a) => SolSet a -> SolSet a -> SolSet a
intersection NoSol _ = NoSol
intersection _ NoSol = NoSol
intersection IndetSol s = s
intersection s IndetSol = s
intersection (Sol x) (Sol y) | x == y = Sol y
                             | otherwise = NoSol
