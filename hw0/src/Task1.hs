{-# LANGUAGE TypeOperators #-}

module Task1
    ( distributivity
    , associator
    , eitherAssoc
    ) where

import Prelude

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (suffProof, neccProof)
 where
   suffProof :: Either a (Either b c) -> Either (Either a b) c
   suffProof (Left a) = Left (Left a)
   suffProof (Right (Left b)) = Left (Right b)
   suffProof (Right (Right c)) = Right c

   neccProof :: Either (Either a b) c -> Either a (Either b c)
   neccProof (Left (Left a)) = Left a
   neccProof (Left (Right b)) = Right (Left b)
   neccProof (Right c) = Right (Right c)
