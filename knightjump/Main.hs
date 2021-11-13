{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative   ((<|>), liftA2)
import           Data.Functor          (($>), (<&>))
import           Data.Functor.Identity (Identity)
import           Data.Bifunctor        (bimap, second)
import           Data.Maybe            (fromMaybe)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import qualified Text.Parsec           as P

main :: IO ()
main = do
    n <- readLn
    board <- either (error . show) pure . P.runParser (p_board n) initialCell "input" =<< getContents
    let mMinSteps = travel board
    putStrLn . show . fromMaybe (-1) $ mMinSteps

type Pos = (Int, Int)

data Board = Board { _n       :: Int
                   , _blocked :: Set Pos
                   , _movers  :: Set Pos
                   , _waiters :: Set Pos
                   , _steps   :: Int
                   }
                   deriving (Show)

----------
-- parsing

type Pars a = forall s. ( P.Stream s Identity Char ) => P.Parsec s Pos a

initialCell :: Pos
initialCell = (1, 1)

p_board :: Int -> Pars Board
p_board n = P.count n (p_line n) <&> mconcat >>= \case
    ([k], blocked) -> pure $
        Board n (Set.fromList blocked) (Set.singleton k) (Set.singleton initialCell) 0
    _ -> fail "Board does not have exactly one king."

p_line :: Int -> Pars ([Pos], [Pos])
p_line n = cells <* P.newline <* updatePos
    where updatePos = P.modifyState (bimap succ $ const 1)
          cells = mconcat <$> P.count n p_cell

p_cell, p_empty, p_block, p_knight :: Pars ([Pos], [Pos])
p_cell = (p_empty <|> p_block <|> p_knight) <* P.modifyState (second succ)
p_empty  = P.char '.' $> mempty
p_block  = P.char '#' *> ((mempty, ) . pure <$> P.getState)
p_knight = P.char 'K' *> ((, mempty) . pure <$> P.getState)

----------
-- computing

travel :: Board -> Maybe Int
travel (Board n blocked movers waiters steps) = if finished then Just steps else next
  where
    finished = not $ Set.disjoint movers waiters
    next = if (Set.null newMovers) then Nothing else travel $
        Board n (blocked <> movers) waiters newMovers (succ steps)
    newMovers = flip foldMap movers $ \k -> Set.fromList $ filter legal $ move k <$> moves
    legal k'@(r, c) = and [ r >= 1, c >= 1, r <= n, c <= n
                          , k' `Set.notMember` blocked, k' `Set.notMember` movers
                          ]

moves :: [(Int, Int)]
moves = liftA2 bimap sign sign <*> [(1, 2), (2, 1)]
    where sign = [id, negate]

move :: Pos -> (Int, Int) -> Pos
move (r, c) (rd, cd) = (r + rd, c + cd)
