import Test.HUnit
import Quaternion

pushTest  = TestCase (assertEqual "for (foo 3)," (1) (1))

qTest = TestCase (assertEqual "same" (Q 1 2 3 4) (Q 1 2 3 4))

main :: IO ()
main = do
    counts <- runTestTT $
        TestList [ TestLabel "aaa" pushTest
                 , TestLabel "qqq" qTest
                 ]
    putStrLn $ show counts
    return ()
