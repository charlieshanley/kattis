{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow         ((&&&))
import           Data.Char             (digitToInt)
import           Data.Foldable          (for_)
import           Data.Functor.Identity (Identity)
import           Data.List             (sortOn)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Coerce           (Coercible, coerce)
import           Text.Parsec           (Parsec, Stream, chainl1, char, count, digit, eof, letter, newline, parse, space, spaces, many1)

newtype Paperwork = Paperwork Int
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral, Bounded)

newtype Price = Price Int
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral, Bounded)

data Situation = Situation { _situationCurrent :: Paperwork, _situationTarget :: Paperwork }
    deriving (Show)

data Op = Pred | Halve

data Agency = Agency { _agencyName :: Text, _agencyOpPrice :: Op -> Price }

instance Show Agency where
    show (Agency nm opPrice) = "Agency " <> T.unpack nm <> show (opPrice Pred, opPrice Halve)

data Case = Case { _caseSituation :: Situation, _caseAgencies :: [Agency] }
    deriving (Show)

op :: Op -> Paperwork -> Paperwork
op Pred = pred
op Halve = (`div` 2)

----------
-- cost algorithm

minimumCost :: Situation -> Agency -> Price
minimumCost (Situation initial target) (Agency _ opPrice) = go initial (Price 0)
  where
    go :: Paperwork -> Price -> Price
    go !current !accCost | current <= target = accCost
      | op Halve current < target = next Pred
      | ppu Pred < ppu Halve      = next Pred
      | otherwise                 = next Halve
      where
        next o = go (op o current) (accCost + opPrice o)

        ppu Pred  = fromIntegral $ opPrice Pred
        ppu Halve = fromIntegral (opPrice Halve) / fromIntegral (current - op Halve current)

----------
-- parsing

p_input :: Parsec Text () [Case]
p_input = do
    nCases <- p_nat <* newline
    count nCases p_case <* eof

p_nat :: Parsec Text () Int
p_nat = chainl1 (digitToInt <$> digit) (pure (\(!x) (!y) -> x * 10 + y))

p_natlike :: (Coercible a Int)  => Parsec Text () a
p_natlike = coerce <$> p_nat

p_case :: Parsec Text () Case
p_case = do
    situation <- p_situation <* space
    nAgencies <- p_nat <* newline
    agencies <- count nAgencies p_agency
    pure $ Case situation agencies

p_situation :: Parsec Text () Situation
p_situation = Situation <$> p_natlike <*> (space *> p_natlike)

p_agency :: Parsec Text () Agency
p_agency = Agency <$> (T.pack <$> many1 letter <* char ':')
                  <*> (priceFun <$> p_natlike <* char ',' <*> p_natlike)
                  <*  newline

priceFun :: Price -> Price -> Op -> Price
priceFun a _ Pred  = a
priceFun _ b Halve = b

----------

main :: IO ()
main = do
    cases <- either (error . show) pure =<< parse p_input "input" <$> T.getContents
    for_ (zip cases [1..]) $ \(Case situation agencies, i) -> do
        T.putStrLn $ "Case " <> T.pack (show i)
        let agenciesAndCosts = (id &&& minimumCost situation) <$> agencies
        for_ (sortOn snd agenciesAndCosts) $ \(Agency name _, cost) -> do
            T.putStrLn $ name <> " " <> T.pack (show cost)
