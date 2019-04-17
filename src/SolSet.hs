module SolSet where

-- | A set which is either empty (if constructed by 'NoSol'), has a single
-- member (if constructed by 'Sol'), or contains everything of type 'a' (if
-- constructed by 'IndetSol').
data SolSet a = NoSol | Sol a | IndetSol
    deriving (Eq, Show)

-- | Test whether a 'SolSet' is empty.
unsolvable :: SolSet a -> Bool
unsolvable NoSol = True
unsolvable _ = False

-- | Test whether a 'SolSet' is nonempty.
solvable :: SolSet a -> Bool
solvable s = not (unsolvable s)

-- | Test whether a 'SolSet' is a singleton.
uniquelySolvable :: SolSet a -> Bool
uniquelySolvable (Sol x) = True
uniquelySolvable _ = False

-- | Test whether a 'SolSet' is a universal set for a type.
indeterminate :: SolSet a -> Bool
indeterminate IndetSol = True
indeterminate _ = False

-- | Intersect two 'SolSets'.
intersection :: (Eq a) => SolSet a -> SolSet a -> SolSet a
intersection NoSol _ = NoSol
intersection _ NoSol = NoSol
intersection IndetSol s = s
intersection s IndetSol = s
intersection (Sol x) (Sol y) | x == y = Sol x
                             | otherwise = NoSol
