{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CreateInput where

import Main hiding (main)

import           Data.Coerce     (coerce)
import           Data.Foldable   (for_)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Text.Parsec     as P
import           Test.QuickCheck

instance Arbitrary Situation where
    arbitrary = do
        current <- chooseInt (1, 100000)
        target <- chooseInt (1, coerce current)
        pure $ Situation current target

instance Arbitrary Agency where
    arbitrary = do
        nameLength <- chooseInt (1, 16)
        name <- T.pack <$> vectorOf nameLength (elements ['A'..'Z'])
        opPrice <- priceFun <$> genPrice <*> genPrice
        pure $ Agency name opPrice

genPrice :: Gen Price
genPrice = chooseInt (1, 10000)

instance Arbitrary Case where
    arbitrary = Case <$> arbitrary <*> arbitrary

----------

class ToInput a where
    toInput :: a -> Text

instance ToInput Int where
    toInput = tshow

instance ToInput Agency where
    toInput (Agency nm opPrice) = nm <> ":" <> toInput (opPrice Pred) <> "," <> toInput (opPrice Halve)

instance ToInput Case where
    toInput (Case (Situation current target) agencies) = T.unlines $ header : fmap toInput agencies
        where header = T.unwords (toInput <$> [ current, target, length agencies ])

createInput :: [Case] -> Text
createInput cases = T.concat $
    toInput (length cases) : "\n" : fmap toInput cases

writeInput :: IO ()
writeInput = do
    cases <- generate $ vector @Case 50
    T.putStr $ createInput cases

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = do
    let parseInput = P.parse p_input
    quickCheck $ do
        cases <- arbitrary
        pure $ (parseInput "arbitrary" . createInput) cases === Right cases
    input1 <- T.readFile "./1.in"
    input2 <- T.readFile "./2.in"
    quickCheck $ conjoin
        [ (fmap createInput . parseInput "1.in") input1 === Right input1
        , (fmap createInput . parseInput "2.in") input2 === Right input2
        ]
