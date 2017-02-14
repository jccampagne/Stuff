{-# Language ScopedTypeVariables #-}

module Quaternion where
import Prelude as P

data Q n = Q n n n n

instance Functor Q where
    fmap f (Q a1 ai aj ak) = Q (f a1) (f ai) (f aj) (f ak)

instance Applicative Q where
    pure x = Q x x x x
    (Q f1 fi fj fk) <*> (Q a1 ai aj ak) = Q (f1 a1) (fi ai) (fj aj) (fk ak)

instance (RealFloat n) => Num (Q n) where
    (+) (Q a1 ai aj ak) (Q b1 bi bj bk) =
        Q c1 ci cj ck
        where
            c1 = a1 + b1
            ci = ai + bi
            cj = aj + bj
            ck = ak + aj
            (+) = (P.+)

    (*) (Q a1 ai aj ak) (Q b1 bi bj bk) =
        Q c1 ci cj ck
        where
            c1 = a1*b1 - ai*bi - aj*bj - ak*bk
            ci = a1*bi + ai*b1 + aj*bk - ak*bj
            cj = a1*bj - ai*bk + aj*b1 + ak*bi
            ck = a1*bk + ai*bj - aj*bi + ak*b1
            (+) = (P.+)

    abs (Q a1 ai aj ak) = Q b1 0 0 0
        where
            b1 = sqrt ( a1*a1 + ai*ai + aj*aj + ak*ak )

    fromInteger n = Q (fromInteger n) 0 0 0

    negate = fmap negate

    signum = undefined

conj (Q a1 ai aj ak) = Q a1 (-ai) (-aj) (-ak)

test :: Q Double
test =
    let qa = Q 1 2 3 4 in
    let qb = Q 1 2 3 4 in
    let q = fromInteger 1 in q

