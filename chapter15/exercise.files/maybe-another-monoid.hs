#!/usr/bin/env cabal
{- cabal:
build-depends:  base
              , QuickCheck
-}
import Test.QuickCheck


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a
      => Semigroup (Optional a) where
  (Only x)  <> (Only y) = Only (x <> y)
  Nada      <> (Only y) = Only y
  (Only x)  <> Nada     = Only x
  Nada      <> Nada     = Nada

instance Monoid a
      => Monoid (Optional a) where
  mempty      = Nada
  mappend x y = x <> y

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Only <$> arbitrary)]

newtype First' a =
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

instance Semigroup a
    => Semigroup (First' a) where
  (First' x) <> (First' y) = First' (x <> y)
  
instance Monoid a
      => Monoid (First' a) where
  mempty = First' Nada
  mappend x y = x <> y
  
instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary 

firstMappend  :: Monoid a
              => First' a
              -> First' a
              -> First' a
firstMappend  = mappend

type FirstMappend =
      First' String
  ->  First' String
  ->  First' String
  ->  Bool

type FstId =
  First' String -> Bool
  
monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                  => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)