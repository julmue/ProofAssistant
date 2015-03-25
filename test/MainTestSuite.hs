import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Semantics.SemanticsInternal

main = defaultMain tests

tests =
    [ testGroup "xxx"
        [ testProperty "identity" propIdentity ]
    ]


propIdentity :: Int -> Bool
propIdentity x = x == x
