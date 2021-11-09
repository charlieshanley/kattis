{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CreateInput where

import Main hiding (main)

import           Data.Coerce     (coerce)
import           Data.Foldable   (for_)
import qualified Data.Text       as T
import           Test.QuickCheck

instance Arbitrary Price where
    arbitrary = Price <$> chooseInt (0, 10000)

instance Arbitrary Situation where
    arbitrary = do
        current <- Paperwork <$> chooseInt (1, 100000)
        target <- Paperwork <$> chooseInt (1, coerce current)
        pure $ Situation current target

instance Arbitrary Agency where
    arbitrary = Agency <$> (T.pack <$> vectorOf 16 (elements ['A'..'Z']))
                       <*> (priceFun <$> arbitrary <*> arbitrary)

instance Arbitrary Case where
    arbitrary = Case <$> arbitrary <*> vector nAgencies

main :: IO ()
main = do
    cases <- generate $ vector @Case nCases
    putStrLn $ show nCases
    for_ cases $ \(Case (Situation current target) agencies) -> do
        putStrLn $ show current <> " " <> show target <> " " <> show nAgencies
        for_ agencies $ \(Agency nm opPrice) -> do
            putStrLn $ T.unpack nm <> ":" <> show (opPrice Pred) <> "," <> show (opPrice Halve)

nCases, nAgencies :: Int
nCases = 50
nAgencies = 100
