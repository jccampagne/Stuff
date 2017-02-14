{-# Language ScopedTypeVariables #-}

import Test.HUnit
import Test.TestQuaternion

main :: IO ()
main = do
    counts <- runTestTT Test.TestQuaternion.tests
    putStrLn $ show counts
    return ()
