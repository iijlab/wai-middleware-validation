-- {-# OPTIONS_GHC -F -pgmF doctest-driver -optF config.json #-}
import           Test.DocTest

main :: IO ()
main = doctest
    [ "-isrc"
    , "-XOverloadedStrings"
    , "src/Network/Wai/Middleware/Validation/Internal.hs"
    ]
