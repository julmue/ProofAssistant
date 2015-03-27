import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Logic.Semantics.Prop.Internal

main = defaultMain tests

tests =
    [ testGroup "xxx"
        [ testProperty "identity" propIdentity ]
    ]


propIdentity :: Int -> Bool
propIdentity x = x == x
