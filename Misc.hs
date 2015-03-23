module Misc
    ( cartProd
    , combination
    )
where

import Control.Applicative
{- Misc helper functions -}

-- crossproduct
cartProd :: [a] -> [b] -> [(a,b)]
cartProd as bs = (,) <$> as <*> bs

-- combination of sets
-- all possible combinations of single element from every set.
-- whats the exact name of the thing?
combination :: [[a]] -> [[a]]
combination [as] = fmap (:[]) as
combination (x:xs) = (:) <$> x <*> combination xs
