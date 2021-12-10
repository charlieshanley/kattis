-- https://open.kattis.com/problems/moneymatters

-- The group of friends form a graph, where persons are nodes and edges are
-- remaining friendships. In order to determine if it's possible for the group
-- to settle up, we're interested in the connected components of the graph. If
-- two people are connected by friendship directly or indirectly, money can flow
-- between them. However, if no friendships connect two persons (IE, they occupy
-- different connected components, then money cannot flow between them.
--
-- So, the group can settle up if and only if every connected component of the
-- graph can settle up, IE the sum of their debts equals zero.

----------

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Control.Monad.State   (State, get, put, runState)
import           Data.Coerce           (coerce)
import           Data.Foldable         (fold)
import           Data.Functor          (void)
import           Data.Functor.Identity (Identity)
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IM
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IS
import qualified Data.Text.IO          as T
import qualified Text.Parsec           as P
import           Text.Read             (readEither)

type Crown = Int

newtype Friendships = Friendships { getFriendships :: IntMap IntSet }
    deriving (Eq, Show)

instance Semigroup Friendships where
    (<>) = coerce $ IM.unionWith IS.union

instance Monoid Friendships where
    mempty = Friendships IM.empty

data Friends = Friends
    { friendsDebts       :: IntMap Crown
    , friendsFriendships :: Friendships
    }
    deriving (Show)

----------
-- logic

connectedComponents :: Friendships -> [IntSet]
connectedComponents fs
  | fs == mempty = []
  | otherwise = component : connectedComponents remainder
  where component = connectedComponentFromMin fs
        remainder = Friendships $ getFriendships fs `IM.withoutKeys` component
        
connectedComponentFromMin :: Friendships -> IntSet
connectedComponentFromMin fs = case IM.lookupMin $ getFriendships fs of
    Nothing -> mempty
    Just (minKey, minKeyFriends) -> IS.insert minKey minKeyFriends <> transitiveFriends minKeyFriends fs

transitiveFriends :: IntSet -> Friendships -> IntSet
transitiveFriends from (Friendships fs) = fst $ snd $ runState go (from, from)
  where
    go :: State (IntSet, IntSet) ()
    go = do
        (acc, frontier) <- get
        case () of
          () | frontier == mempty -> pure ()
             | otherwise -> do
                 let next = fold $ IM.restrictKeys fs frontier
                 put (acc <> next, next `IS.difference` acc)
                 go

canSettle :: Friends -> Bool
canSettle (Friends debts friendships) = settle `all` connectedComponents friendships
  where
      settle :: IntSet -> Bool
      settle keys = 0 == IM.foldr' (+) 0 (IM.restrictKeys debts keys)

----------
-- parsing

type Pars a = forall s. ( P.Stream s Identity Char ) => P.Parsec s () a

p_friends :: Pars Friends
p_friends = do
    n <- p_int
    void P.space
    m <- p_int
    void P.newline
    debts <- IM.fromAscList . zip [0..] <$> P.count n (p_int <* P.newline)
    friendEdges <- P.count m $ (,) <$> p_int <* P.space <*> p_int <* P.newline
    pure $ Friends debts $ flip foldMap friendEdges $ \(x, y) ->
        Friendships (IM.singleton x $ IS.singleton y) <> Friendships (IM.singleton y $ IS.singleton x)

p_int :: Pars Int
p_int = p_read $ P.many1 $ P.oneOf ('-' : ['0'..'9'])

-- Beware: consumes more input than necessary to find error
p_read :: Read a => Pars String -> Pars a
p_read ps = either fail pure . readEither =<< ps

----------

main :: IO ()
main = do
    Right friends <- P.parse p_friends "input" <$> T.getContents
    T.putStrLn $ if canSettle friends then "POSSIBLE" else "IMPOSSIBLE"
