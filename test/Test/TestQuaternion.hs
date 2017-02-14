module Test.TestQuaternion (tests) where

{-# Language ScopedTypeVariables #-}

import Test.HUnit
import Quaternion

qTest =
    let v = Q 1 2 3 4 in
    let r = negate $ negate $ Q 1 2 3 4 in
    TestCase $ assertEqual "same" v r

qNormTest0 =
    let Q v 0 0 0 = abs $ Q 1 2 3 4 in
    let r = sqrt 30 in
    TestCase $ assertEqual "same" v r

qNormTest1 =
    let q = Q 1 2 3 4 in
    let v = norm q in
    let vv = v * v in
    let r = 1*1 + 2*2 + 3*3 + 4*4 in
    TestCase $ assertEqual "same" vv r

tests =
    TestList [ TestLabel "simple" qTest
             , TestLabel "qnorm" qNormTest0
             , TestLabel "qnorm" qNormTest1
             ]

