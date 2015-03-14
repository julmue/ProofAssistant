module Misc
    ( cartP
    , combine
    )
where

import Control.Applicative
{- Misc helper functions -}

-- crossproduct
cartP :: [a] -> [b] -> [(a,b)]
cartP as bs = (,) <$> as <*> bs

-- combination of two sets
-- whats the exact name of the thing?
combine :: [[a]] -> [[a]]
combine [as] = fmap (:[]) as
combine (x:xs) = (:) <$> x <*> combine xs
