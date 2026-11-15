{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

#include "containers.h"

#if !(WORD_SIZE_IN_BITS >= 61)
#define DEFINE_ALTERF_FALLBACK 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
--
-- = Finite Maps (lazy interface internals)
--
-- The @'Map' k v@ type represents a finite map (sometimes called a dictionary)
-- from keys of type @k@ to values of type @v@. A 'Map' is strict in its keys
-- but lazy in its values.
--
--
-- == Implementation
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets—a balancing act/\",
--      Journal of Functional Programming 3(4):553-562, October 1993,
--      <https://doi.org/10.1017/S0956796800000885>,
--      <https://groups.csail.mit.edu/mac/users/adams/BB/index.html>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--      <https://doi.org/10.1137/0202005>.
--    * Yoichi Hirai and Kazuhiko Yamamoto,
--      \"/Balancing weight-balanced trees/\",
--      Journal of Functional Programming 21(3):287-307, 2011,
--      <https://doi.org/10.1017/S0956796811000104>
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Parallel Ordered Sets Using Join/\",
--      <https://arxiv.org/abs/1602.02120v4>.
--
--
-- @since 0.5.9
-----------------------------------------------------------------------------

-- [Note: Using INLINABLE]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- It is crucial to the performance that the functions specialize on the Ord
-- type when possible. GHC 7.0 and higher does this by itself when it sees th
-- unfolding of a function -- that is why all public functions are marked
-- INLINABLE (that exposes the unfolding).


-- Note [Using INLINE]
-- ~~~~~~~~~~~~~~~~~~~~
-- We mark some functions INLINE where it is more beneficial than INLINABLE.
-- There is a price to pay -- code growth. The code INLINED is therefore usually
-- only the tree navigation, other work such as rebalancing is not INLINED.
--
-- All methods marked INLINE have to be nonrecursive -- a 'go' function doing
-- the real work is provided.


-- [Note: Type of local 'go' function]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If the local 'go' function uses an Ord class, it sometimes heap-allocates
-- the Ord dictionary when the 'go' function does not have explicit type.
-- In that case we give 'go' explicit type. But this slightly decrease
-- performance, as the resulting 'go' function can float out to top level.


-- [Note: Local 'go' functions and capturing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As opposed to Map, when 'go' function captures an argument, increased
-- heap-allocation can occur: sometimes in a polymorphic function, the 'go'
-- floats out of its enclosing function and then it heap-allocates the
-- dictionary and the argument. Maybe it floats out too late and strictness
-- analyzer cannot see that these could be passed on stack.
--

-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of Map matters when considering performance.
-- Currently in GHC 7.0, when type has 2 constructors, a forward conditional
-- jump is made when successfully matching second constructor. Successful match
-- of first constructor results in the forward jump not taken.
-- On GHC 7.0, reordering constructors from Tip | Bin to Bin | Tip
-- improves the benchmark by up to 10% on x86.

module Data.Map.Internal (
    -- * Map type
      Map(..)          -- instance Eq,Show,Read
    , Size

    -- * Operators
    , (!), (!?), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , pop
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , upsert
    , updateLookupWithKey
    , alter
    , alterF

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Symmetric difference
    , symmetricDifference

    -- ** Disjoint
    , disjoint

    -- ** Compose
    , compose

    -- ** General combining function
    , SimpleWhenMissing
    , SimpleWhenMatched
    , runWhenMatched
    , runWhenMissing
    , merge
    -- *** @WhenMatched@ tactics
    , dropMatched
    , zipWithMaybeMatched
    , zipWithMatched
    -- *** @WhenMissing@ tactics
    , mapMaybeMissing
    , dropMissing
    , preserveMissing
    , preserveMissing'
    , mapMissing
    , filterMissing

    -- ** Applicative general combining function
    , WhenMissing (..)
    , WhenMatched (..)
    , mergeA

    -- *** @WhenMatched@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , zipWithMaybeAMatched
    , zipWithAMatched

    -- *** @WhenMissing@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , traverseMaybeMissing
    , traverseMissing
    , filterAMissing
    , whenMissing

    -- ** Deprecated general combining function

    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , traverseMaybeWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic
    , mapAssocsMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet
    , argSet
    , fromSet
    , fromSetA
    , fromSetMaybe
    , fromSetMaybeA
    , fromArgSet

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey
    , fromListUpsert

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromAscListUpsert
    , fromDistinctAscList
    , fromDescList
    , fromDescListWith
    , fromDescListWithKey
    , fromDescListUpsert
    , fromDistinctDescList

    -- * Filter
    , filter
    , filterKeys
    , filterWithKey

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt
    , take
    , drop
    , splitAt

    -- * Min\/Max
    , lookupMin
    , lookupMax
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- Used by the strict version
    , AreWeStrict (..)
    , atKeyImpl
    , bin
    , balance
    , balanceL
    , balanceR
    , delta
    , insertMax
    , link
    , link2
    , glue
    , ascLinkTop
    , ascLinkAll
    , descLinkTop
    , descLinkAll
    , Identity(..)
    , Stack(..)
    , foldl'Stack
    , MapBuilder(..)
    , emptyB
    , insertB
    , finishB

    -- Used by Map.Merge.Lazy
    , mapWhenMissing
    , mapWhenMatched
    , lmapWhenMissing
    , contramapFirstWhenMatched
    , contramapSecondWhenMatched
    , mapGentlyWhenMissing
    , mapGentlyWhenMatched
    ) where

import Data.Functor.Identity (Identity (..))
import Control.Applicative (liftA3)
import Data.Functor.Classes
import Data.Semigroup (stimesIdempotentMonoid)
import Data.Semigroup (Arg(..), Semigroup(stimes))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import Control.Applicative (Const (..))
import Control.DeepSeq (NFData(rnf),NFData1(liftRnf),NFData2(liftRnf2))
import qualified Data.Foldable as Foldable
import Data.Bifoldable
import Utils.Containers.Internal.Prelude hiding
  (lookup, map, filter, foldr, foldl, foldl', null, splitAt, take, drop)
import Prelude ()

import qualified Data.Set.Internal as Set
import Data.Set.Internal (Set)
import Utils.Containers.Internal.PtrEquality (ptrEq)
import Utils.Containers.Internal.Strict
  (StrictPair(..), StrictTriple(..), toPair)
import Utils.Containers.Internal.BitQueue
import Utils.Containers.Internal.EqOrdUtil (EqM(..), OrdM(..))
#ifdef DEFINE_ALTERF_FALLBACK
import Utils.Containers.Internal.BitUtil (wordSize)
#endif

#if __GLASGOW_HASKELL__
import GHC.Exts (build, lazy)
#  if __GLASGOW_HASKELL__ >= 914
import Language.Haskell.TH.Lift (Lift)
#  else
import Language.Haskell.TH.Syntax (Lift)
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#  endif
import qualified GHC.Exts as GHCExts
import Data.Data
import Data.Coerce
#endif
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
import Text.Read hiding (lift)
#endif
import qualified Control.Category as Category

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,!?,\\ --

-- | \(O(\log n)\). Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- __Note__: This function is partial. Prefer '!?'.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: Ord k => Map k a -> k -> a
(!) m k = find k m
{-# INLINE (!) #-}

-- | \(O(\log n)\). Find the value at a key.
-- Returns 'Nothing' when the element can not be found.
--
-- prop> fromList [(5, 'a'), (3, 'b')] !? 1 == Nothing
-- prop> fromList [(5, 'a'), (3, 'b')] !? 5 == Just 'a'
--
-- @since 0.5.9

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) m k = lookup k m
{-# INLINE (!?) #-}

-- | Same as 'difference'.
(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2
{-# INLINE (\\) #-}

{--------------------------------------------------------------------
  Size balanced trees.
--------------------------------------------------------------------}
-- | A Map from keys @k@ to values @a@.

-- See Note: Order of constructors
data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
              | Tip

type Size     = Int

#ifdef __GLASGOW_HASKELL__
type role Map nominal representational
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.6
deriving instance (Lift k, Lift a) => Lift (Map k a)
#endif

-- | @mempty@ = 'empty'
instance (Ord k) => Monoid (Map k v) where
    mempty  = empty
    mconcat = unions
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

-- | @(<>)@ = 'union'
--
-- @since 0.5.7
instance (Ord k) => Semigroup (Map k v) where
    (<>)    = union
    stimes  = stimesIdempotentMonoid

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance (Data k, Data a, Ord k) => Data (Map k a) where
  gfoldl f z m   = z fromList `f` toList m
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = mapDataType
  dataCast2 f    = gcast2 f

fromListConstr :: Constr
fromListConstr = mkConstr mapDataType "fromList" [] Prefix

mapDataType :: DataType
mapDataType = mkDataType "Data.Map.Internal.Map" [fromListConstr]

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | \(O(1)\). Is the map empty?
--
-- > Data.Map.null (empty)           == True
-- > Data.Map.null (singleton 1 'a') == False

null :: Map k a -> Bool
null Tip      = True
null (Bin {}) = False
{-# INLINE null #-}

-- | \(O(1)\). The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz
{-# INLINE size #-}


-- | \(O(\log n)\). Look up the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.Map
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup :: Ord k => k -> Map k a -> Maybe a
lookup = go
  where
    go !_ Tip = Nothing
    go k (Bin _ kx x l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> Just x
{-# INLINABLE lookup #-}

-- | \(O(\log n)\). Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: Ord k => k -> Map k a -> Bool
member = go
  where
    go !_ Tip = False
    go k (Bin _ kx _ l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> True
{-# INLINABLE member #-}

-- | \(O(\log n)\). Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Ord k => k -> Map k a -> Bool
notMember k m = not $ member k m
{-# INLINABLE notMember #-}

find :: Ord k => k -> Map k a -> a
find = go
  where
    go !_ Tip = error "Map.!: given key is not an element in the map"
    go k (Bin _ kx x l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> x
{-# INLINABLE find #-}

-- | \(O(\log n)\). The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault = go
  where
    go def !_ Tip = def
    go def k (Bin _ kx x l r) = case compare k kx of
      LT -> go def k l
      GT -> go def k r
      EQ -> x
{-# INLINABLE findWithDefault #-}

-- | \(O(\log n)\). Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLT = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) | k <= kx = goNothing k l
                                 | otherwise = goJust k kx x r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) | k <= kx = goJust k kx' x' l
                                     | otherwise = goJust k kx x r
{-# INLINABLE lookupLT #-}

-- | \(O(\log n)\). Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGT = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) | k < kx = goJust k kx x l
                                 | otherwise = goNothing k r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) | k < kx = goJust k kx x l
                                     | otherwise = goJust k kx' x' r
{-# INLINABLE lookupGT #-}

-- | \(O(\log n)\). Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLE = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) = case compare k kx of LT -> goNothing k l
                                                        EQ -> Just (kx, x)
                                                        GT -> goJust k kx x r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) = case compare k kx of LT -> goJust k kx' x' l
                                                            EQ -> Just (kx, x)
                                                            GT -> goJust k kx x r
{-# INLINABLE lookupLE #-}

-- | \(O(\log n)\). Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGE = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) = case compare k kx of LT -> goJust k kx x l
                                                        EQ -> Just (kx, x)
                                                        GT -> goNothing k r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) = case compare k kx of LT -> goJust k kx x l
                                                            EQ -> Just (kx, x)
                                                            GT -> goJust k kx' x' r
{-# INLINABLE lookupGE #-}

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | \(O(1)\). The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: Map k a
empty = Tip
{-# INLINE empty #-}

-- | \(O(1)\). A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | \(O(\log n)\). Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper
insert :: Ord k => k -> a -> Map k a -> Map k a
insert kx0 = go kx0 kx0
  where
    -- Unlike insertR, we only get sharing here
    -- when the inserted value is at the same address
    -- as the present value. We try anyway; this condition
    -- seems particularly likely to occur in 'union'.
    go :: Ord k => k -> k -> a -> Map k a -> Map k a
    go orig !_  x Tip = singleton (lazy orig) x
    go orig !kx x t@(Bin sz ky y l r) =
        case compare kx ky of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL ky y l' r
               where !l' = go orig kx x l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR ky y l r'
               where !r' = go orig kx x r
            EQ | x `ptrEq` y && (lazy orig `seq` (orig `ptrEq` ky)) -> t
               | otherwise -> Bin sz (lazy orig) x l r
{-# INLINABLE insert #-}

#ifndef __GLASGOW_HASKELL__
lazy :: a -> a
lazy a = a
#endif

-- [Note: Avoiding worker/wrapper]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'insert' has to go to great lengths to get pointer equality right and
-- to prevent unnecessary allocation. The trouble is that GHC *really* wants
-- to unbox the key and throw away the boxed one. This is bad for us, because
-- we want to compare the pointer of the box we are given to the one already
-- present if they compare EQ. It's also bad for us because it leads to the
-- key being *reboxed* if it's actually stored in the map. Ugh! So we pass the
-- 'go' function *two copies* of the key we're given. One of them we use for
-- comparisons; the other we keep in our pocket. To prevent worker/wrapper from
-- messing with the copy in our pocket, we sprinkle about calls to the magical
-- function 'lazy'. This is all horrible, but it seems to work okay.


-- Insert a new key and value in the map if it is not already present.
-- Used by `union`.

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper
insertR :: Ord k => k -> a -> Map k a -> Map k a
insertR kx0 = go kx0 kx0
  where
    go :: Ord k => k -> k -> a -> Map k a -> Map k a
    go orig !_  x Tip = singleton (lazy orig) x
    go orig !kx x t@(Bin _ ky y l r) =
        case compare kx ky of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL ky y l' r
               where !l' = go orig kx x l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR ky y l r'
               where !r' = go orig kx x r
            EQ -> t
{-# INLINABLE insertR #-}

-- | \(O(\log n)\). Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
--
-- Also see the performance note on 'fromListWith'.

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = go
  where
    -- We have no hope of making pointer equality tricks work
    -- here, because lazy insertWith *always* changes the tree,
    -- either adding a new entry or replacing an element with a
    -- thunk.
    go :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy kx (f x y) l r

{-# INLINABLE insertWith #-}

-- | A helper function for 'unionWith'. When the key is already in
-- the map, the key is left alone, not replaced. The combining
-- function is flipped--it is applied to the old value and then the
-- new value.
--
-- Also see the performance note on 'fromListWith'.

insertWithR :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithR = go
  where
    go :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy ky (f y x) l r
{-# INLINABLE insertWithR #-}

-- | \(O(\log n)\). Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
--
-- Also see the performance note on 'fromListWith'.

-- See Note: Type of local 'go' function
insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey = go
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy kx (f kx x y) l r
{-# INLINABLE insertWithKey #-}

-- | A helper function for 'unionWithKey'. When the key is already in
-- the map, the key is left alone, not replaced. The combining
-- function is flipped--it is applied to the old value and then the
-- new value.
--
-- Also see the performance note on 'fromListWith'.

insertWithKeyR :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKeyR = go
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy ky (f ky y x) l r
{-# INLINABLE insertWithKeyR #-}

-- | \(O(\log n)\). Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])
--
-- Also see the performance note on 'fromListWith'.

-- See Note: Type of local 'go' function
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                    -> (Maybe a, Map k a)
insertLookupWithKey f0 k0 x0 = toPair . go f0 k0 x0
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> StrictPair (Maybe a) (Map k a)
    go _ !kx x Tip = (Nothing :*: singleton kx x)
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> let !(found :*: l') = go f kx x l
                      !t' = balanceL ky y l' r
                  in (found :*: t')
            GT -> let !(found :*: r') = go f kx x r
                      !t' = balanceR ky y l r'
                  in (found :*: t')
            EQ -> (Just y :*: Bin sy kx (f kx x y) l r)
{-# INLINABLE insertLookupWithKey #-}

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | \(O(\log n)\). Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

-- See Note: Type of local 'go' function
delete :: Ord k => k -> Map k a -> Map k a
delete = go
  where
    go :: Ord k => k -> Map k a -> Map k a
    go !_ Tip = Tip
    go k t@(Bin _ kx x l r) =
        case compare k kx of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceR kx x l' r
               where !l' = go k l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceL kx x l r'
               where !r' = go k r
            EQ -> glue l r
{-# INLINABLE delete #-}

-- | \(O(\log n)\). Pop an entry from the map.
--
-- Returns @Nothing@ if the key is not in the map. Otherwise returns @Just@ the
-- value at the key and a map with the entry removed.
--
-- @
-- pop 1 (fromList [(0,"a"),(2,"b"),(4,"c")]) == Nothing
-- pop 2 (fromList [(0,"a"),(2,"b"),(4,"c")]) == Just ("b",fromList [(0,"a"),(4,"c")])
-- @
--
-- @since 0.8.1
pop :: Ord k => k -> Map k a -> Maybe (a, Map k a)
pop k0 t0 = case go k0 t0 of
  Popped (Just y) t -> Just (y, t)
  _ -> Nothing
  where
    -- See Note [Popped impl]
    go !k (Bin _ kx x l r) = case compare k kx of
      LT -> case go k l of
        Popped y@(Just _) l' -> Popped y (balanceR kx x l' r)
        q -> q
      EQ -> Popped (Just x) (glue l r)
      GT -> case go k r of
        Popped y@(Just _) r' -> Popped y (balanceL kx x l r')
        q -> q
    go !_ Tip = Popped Nothing Tip
{-# INLINABLE pop #-}

-- Note [Popped impl]
-- ~~~~~~~~~~~~~~~~~~
-- Popped is implemented as a pair, though a sum makes more sense:
--   data Popped k a = NotPopped | Popped a !(Map k a)
-- This is because GHC optimizes a return value of `Popped k a` to
-- `(# Maybe a, Map k a #)`, avoiding all Popped allocations in `pop`.
-- GHC cannot do this with a sum type yet, see GHC #14259. Manually using
-- unboxed sums avoids the allocations but GHC loses strictness information,
-- see #25988.
--
-- On GHC>=9.6 we unbox the Maybe and avoid that allocation too, so `pop`'s `go`
-- returns `(# (# (# #) | a #), Map k a #)`.

data Popped k a = Popped
#if __GLASGOW_HASKELL__ >= 906
  {-# UNPACK #-}
#endif
  !(Maybe a)
  !(Map k a)

-- | \(O(\log n)\). Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)
{-# INLINABLE adjust #-}

-- | \(O(\log n)\). Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey = go
  where
    go :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
    go _ !_ Tip = Tip
    go f k (Bin sx kx x l r) =
        case compare k kx of
           LT -> Bin sx kx x (go f k l) r
           GT -> Bin sx kx x l (go f k r)
           EQ -> Bin sx kx (f kx x) l r
{-# INLINABLE adjustWithKey #-}

-- | \(O(\log n)\). The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)
{-# INLINABLE update #-}

-- | \(O(\log n)\). The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

-- See Note: Type of local 'go' function
updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey = go
  where
    go :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
    go _ !_ Tip = Tip
    go f k(Bin sx kx x l r) =
        case compare k kx of
           LT -> balanceR kx x (go f k l) r
           GT -> balanceL kx x l (go f k r)
           EQ -> case f kx x of
                   Just x' -> Bin sx kx x' l r
                   Nothing -> glue l r
{-# INLINABLE updateWithKey #-}

-- | \(O(\log n)\). Update the value at a key or insert a value if the key is
-- not in the map.
--
-- @
-- let inc = maybe 1 (+1)
-- upsert inc \'a\' (fromList [(\'a\',1),(\'c\',2)]) == fromList [(\'a\',2),(\'c\',2)]
-- upsert inc \'b\' (fromList [(\'a\',1),(\'c\',2)]) == fromList [(\'a\',1),(\'b\',1),(\'c\',2)]
-- @
--
-- @since 0.8.1
upsert :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f !k (Bin sz kx x l r) =
  case compare k kx of
    LT -> balanceL kx x (upsert f k l) r
    EQ -> Bin sz kx (f (Just x)) l r
    GT -> balanceR kx x l (upsert f k r)
upsert f !k Tip = singleton k (f Nothing)
{-# INLINABLE upsert #-}

-- | \(O(\log n)\). Look up and update. See also 'updateWithKey'.
-- This function returns the changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
--
-- See also: 'pop'

-- See Note: Type of local 'go' function
updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a,Map k a)
updateLookupWithKey f0 k0 = toPair . go f0 k0
 where
   go :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> StrictPair (Maybe a) (Map k a)
   go _ !_ Tip = (Nothing :*: Tip)
   go f k (Bin sx kx x l r) =
          case compare k kx of
               LT -> let !(found :*: l') = go f k l
                         !t' = balanceR kx x l' r
                     in (found :*: t')
               GT -> let !(found :*: r') = go f k r
                         !t' = balanceL kx x l r'
                     in (found :*: t')
               EQ -> case f kx x of
                       Just x' -> (Just x' :*: Bin sx kx x' l r)
                       Nothing -> let !glued = glue l r
                                  in (Just x :*: glued)
{-# INLINABLE updateLookupWithKey #-}

-- | \(O(\log n)\). The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]
--
-- Note that @'adjust' = alter . fmap@.

-- See Note: Type of local 'go' function
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = go
  where
    go :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
    go f !k Tip = case f Nothing of
               Nothing -> Tip
               Just x  -> singleton k x

    go f k (Bin sx kx x l r) = case compare k kx of
               LT -> balance kx x (go f k l) r
               GT -> balance kx x l (go f k r)
               EQ -> case f (Just x) of
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r
{-# INLINABLE alter #-}

-- Used to choose the appropriate alterF implementation.
data AreWeStrict = Strict | Lazy

-- | \(O(\log n)\). The expression (@'alterF' f k map@) alters the value @x@ at
-- @k@, or absence thereof.  'alterF' can be used to inspect, insert, delete,
-- or update a value in a 'Map'.  In short: @'lookup' k \<$\> 'alterF' f k m = f
-- ('lookup' k m)@.
--
-- 'alterF' is the most general operation for working with an individual
-- key that may or may not be in a given map. When used with trivial
-- functors like 'Identity' and 'Const', it is often slightly slower than
-- more specialized combinators like 'lookup' and 'insert'. However, when
-- the functor is non-trivial and key comparison is not particularly cheap,
-- it is the fastest way.
--
-- Note on rewrite rules:
--
-- This module includes GHC rewrite rules to optimize 'alterF' for
-- the 'Const' and 'Identity' functors. In general, these rules
-- improve performance. The sole exception is that when using
-- 'Identity', deleting a key that is already absent takes longer
-- than it would without the rules. If you expect this to occur
-- a very large fraction of the time, you might consider using a
-- private copy of the 'Identity' type.
--
-- Note: 'alterF' is a flipped version of the @at@ combinator from
-- @Control.Lens.At@.
--
-- === Examples
--
-- @
-- -- Lookup the value at the key, and also remove the existing value or set a new value.
-- lookupAndSet :: Ord k => k -> Maybe a -> Map k a -> (Maybe a, Map k a)
-- lookupAndSet k new = alterF (\\old -> (old, new)) k
-- @
--
-- @
-- -- Delete the value at the key. If it is absent the result is Nothing.
-- mustDelete :: Ord k => k -> Map k a -> Maybe (Map k a)
-- mustDelete = alterF (Nothing <$)
-- @
--
-- @since 0.5.8
alterF :: (Functor f, Ord k)
       => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF f k m = atKeyImpl Lazy k f m

#ifdef __GLASGOW_HASKELL__
{-# INLINE [2] alterF #-}

-- We can save a little time by recognizing the special case of
-- `Control.Applicative.Const` and just doing a lookup.
{-# RULES
"alterF/Const" forall k (f :: Maybe a -> Const b (Maybe a)) . alterF f k = \m -> Const . getConst . f $ lookup k m
 #-}

-- base 4.8 and above include Data.Functor.Identity, so we can
-- save a pretty decent amount of time by handling it specially.
{-# RULES
"alterF/Identity" forall k f . alterF f k = atKeyIdentity k f
 #-}
#endif

atKeyImpl :: (Functor f, Ord k) =>
      AreWeStrict -> k -> (Maybe a -> f (Maybe a)) -> Map k a -> f (Map k a)
#ifdef DEFINE_ALTERF_FALLBACK
atKeyImpl strict !k f m
-- It doesn't seem sensible to worry about overflowing the queue
-- if the word size is 61 or more. If I calculate it correctly,
-- that would take a map with nearly a quadrillion entries.
  | wordSize < 61 && size m >= alterFCutoff = alterFFallback strict k f m
#endif
atKeyImpl strict !k f m = case lookupTrace k m of
  TraceResult mv q -> (<$> f mv) $ \ fres ->
    case fres of
      Nothing -> case mv of
                   Nothing -> m
                   Just _ -> deleteAlong q m
      Just new -> case strict of
         Strict -> new `seq` case mv of
                      Nothing -> insertAlong q k new m
                      Just _ -> replaceAlong q new m
         Lazy -> case mv of
                      Nothing -> insertAlong q k new m
                      Just _ -> replaceAlong q new m

{-# INLINE atKeyImpl #-}

#ifdef DEFINE_ALTERF_FALLBACK
alterFCutoff :: Int
#if WORD_SIZE_IN_BITS == 32
alterFCutoff = 55744454
#else
alterFCutoff = case wordSize of
      30 -> 17637893
      31 -> 31356255
      32 -> 55744454
      x -> (4^(x*2-2)) `quot` (3^(x*2-2))  -- Unlikely
#endif
#endif

-- On GHC >=9.6 we can unpack sum types, so we unbox the Maybe to avoid the
-- allocation.
data TraceResult a = TraceResult
#if __GLASGOW_HASKELL__ >= 906
  {-# UNPACK #-}
#endif
  !(Maybe a)
  {-# UNPACK #-} !BitQueue

-- Look up a key and return a result indicating whether it was found
-- and what path was taken.
lookupTrace :: Ord k => k -> Map k a -> TraceResult a
lookupTrace = go emptyQB
  where
    go :: Ord k => BitQueueB -> k -> Map k a -> TraceResult a
    go !q !_ Tip = TraceResult Nothing (buildQ q)
    go q k (Bin _ kx x l r) = case compare k kx of
      LT -> (go $! q `snocQB` False) k l
      GT -> (go $! q `snocQB` True) k r
      EQ -> TraceResult (Just x) (buildQ q)
{-# INLINABLE lookupTrace #-}

-- Insert at a location (which will always be a leaf)
-- described by the path passed in.
insertAlong :: BitQueue -> k -> a -> Map k a -> Map k a
insertAlong !_ kx x Tip = singleton kx x
insertAlong q kx x (Bin sz ky y l r) =
  case unconsQ q of
        Just (False, tl) -> balanceL ky y (insertAlong tl kx x l) r
        Just (True,tl) -> balanceR ky y l (insertAlong tl kx x r)
        Nothing -> Bin sz kx x l r  -- Shouldn't happen

-- Delete from a location (which will always be a node)
-- described by the path passed in.
deleteAlong :: BitQueue -> Map k a -> Map k a
deleteAlong !_ Tip = Tip
deleteAlong !q (Bin _ ky y l r) =
  case unconsQ q of
    Just (False, tl) -> balanceR ky y (deleteAlong tl l) r
    Just (True, tl) -> balanceL ky y l (deleteAlong tl r)
    Nothing -> glue l r

-- Replace the value found in the node described
-- by the given path with a new one.
replaceAlong :: BitQueue -> a -> Map k a -> Map k a
replaceAlong !_ _ Tip = Tip -- Should not happen
replaceAlong q  x (Bin sz ky y l r) =
      case unconsQ q of
        Just (False, tl) -> Bin sz ky y (replaceAlong tl x l) r
        Just (True,tl) -> Bin sz ky y l (replaceAlong tl x r)
        Nothing -> Bin sz ky x l r

#ifdef __GLASGOW_HASKELL__
atKeyIdentity :: Ord k => k -> (Maybe a -> Identity (Maybe a)) -> Map k a -> Identity (Map k a)
atKeyIdentity k f t = Identity (alter (coerce f) k t)
{-# INLINABLE atKeyIdentity #-}
#endif

#ifdef DEFINE_ALTERF_FALLBACK
-- When the map is too large to use a bit queue, we fall back to
-- this much slower version which uses a more "natural" implementation
-- improved with Yoneda to avoid repeated fmaps. This works okayish for
-- some operations, but it's pretty lousy for lookups.
alterFFallback :: (Functor f, Ord k)
   => AreWeStrict -> k -> (Maybe a -> f (Maybe a)) -> Map k a -> f (Map k a)
alterFFallback Lazy k f t = alterFYoneda k (\m q -> q <$> f m) t id
alterFFallback Strict k f t = alterFYoneda k (\m q -> q . forceMaybe <$> f m) t id
  where
    forceMaybe Nothing = Nothing
    forceMaybe may@(Just !_) = may
{-# NOINLINE alterFFallback #-}

alterFYoneda :: Ord k =>
      k -> (Maybe a -> (Maybe a -> b) -> f b) -> Map k a -> (Map k a -> b) -> f b
alterFYoneda = go
  where
    go :: Ord k =>
      k -> (Maybe a -> (Maybe a -> b) -> f b) -> Map k a -> (Map k a -> b) -> f b
    go !k f Tip g = f Nothing $ \ mx -> case mx of
      Nothing -> g Tip
      Just x -> g (singleton k x)
    go k f (Bin sx kx x l r) g = case compare k kx of
               LT -> go k f l (\m -> g (balance kx x m r))
               GT -> go k f r (\m -> g (balance kx x l m))
               EQ -> f (Just x) $ \ mx' -> case mx' of
                       Just x' -> g (Bin sx kx x' l r)
                       Nothing -> g (glue l r)
{-# INLINE alterFYoneda #-}
#endif

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | \(O(\log n)\). Return the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map. Calls 'error' when the key is not
-- a 'member' of the map.
--
-- __Note__: This function is partial. Prefer 'lookupIndex'.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map

-- See Note: Type of local 'go' function
findIndex :: Ord k => k -> Map k a -> Int
findIndex = go 0
  where
    go :: Ord k => Int -> k -> Map k a -> Int
    go !_   !_ Tip  = error "Map.findIndex: element is not in the map"
    go idx k (Bin _ kx _ l r) = case compare k kx of
      LT -> go idx k l
      GT -> go (idx + size l + 1) k r
      EQ -> idx + size l
{-# INLINABLE findIndex #-}

-- | \(O(\log n)\). Look up the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
-- > fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
-- > fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
-- > isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

-- See Note: Type of local 'go' function
lookupIndex :: Ord k => k -> Map k a -> Maybe Int
lookupIndex = go 0
  where
    go :: Ord k => Int -> k -> Map k a -> Maybe Int
    go !_  !_ Tip  = Nothing
    go idx k (Bin _ kx _ l r) = case compare k kx of
      LT -> go idx k l
      GT -> go (idx + size l + 1) k r
      EQ -> Just $! idx + size l
{-# INLINABLE lookupIndex #-}

-- | \(O(\log n)\). Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys. If the /index/ is out of range (less
-- than zero, greater or equal to 'size' of the map), 'error' is called.
--
-- __Note__: This function is partial.
--
-- > elemAt 0 (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > elemAt 1 (fromList [(5,"a"), (3,"b")]) == (5, "a")
-- > elemAt 2 (fromList [(5,"a"), (3,"b")])    Error: index out of range

elemAt :: Int -> Map k a -> (k,a)
elemAt !_ Tip = error "Map.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> (kx,x)
  where
    sizeL = size l

-- | \(O(\log n)\). Take a given number of entries in key order, beginning
-- with the smallest keys.
--
-- @
-- take n = 'fromDistinctAscList' . 'Prelude.take' n . 'toAscList'
-- @
--
-- @since 0.5.8

take :: Int -> Map k a -> Map k a
take i m | i >= size m = m
take i0 m0 = go i0 m0
  where
    go i !_ | i <= 0 = Tip
    go !_ Tip = Tip
    go i (Bin _ kx x l r) =
      case compare i sizeL of
        LT -> go i l
        GT -> linkL kx x l (go (i - sizeL - 1) r)
        EQ -> l
      where sizeL = size l

-- | \(O(\log n)\). Drop a given number of entries in key order, beginning
-- with the smallest keys.
--
-- @
-- drop n = 'fromDistinctAscList' . 'Prelude.drop' n . 'toAscList'
-- @
--
-- @since 0.5.8
drop :: Int -> Map k a -> Map k a
drop i m | i >= size m = Tip
drop i0 m0 = go i0 m0
  where
    go i m | i <= 0 = m
    go !_ Tip = Tip
    go i (Bin _ kx x l r) =
      case compare i sizeL of
        LT -> linkR kx x (go i l) r
        GT -> go (i - sizeL - 1) r
        EQ -> insertMin kx x r
      where sizeL = size l

-- | \(O(\log n)\). Split a map at a particular index.
--
-- @
-- splitAt !n !xs = ('take' n xs, 'drop' n xs)
-- @
--
-- @since 0.5.8
splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt i0 m0
  | i0 >= size m0 = (m0, Tip)
  | otherwise = toPair $ go i0 m0
  where
    go i m | i <= 0 = Tip :*: m
    go !_ Tip = Tip :*: Tip
    go i (Bin _ kx x l r)
      = case compare i sizeL of
          LT -> case go i l of
                  ll :*: lr -> ll :*: linkR kx x lr r
          GT -> case go (i - sizeL - 1) r of
                  rl :*: rr -> linkL kx x l rl :*: rr
          EQ -> l :*: insertMin kx x r
      where sizeL = size l

-- | \(O(\log n)\). Update the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- __Note__: This function is partial.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt f !i t =
  case t of
    Tip -> error "Map.updateAt: index out of range"
    Bin sx kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (updateAt f i l) r
      GT -> balanceL kx x l (updateAt f (i-sizeL-1) r)
      EQ -> case f kx x of
              Just x' -> Bin sx kx x' l r
              Nothing -> glue l r
      where
        sizeL = size l

-- | \(O(\log n)\). Delete the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- __Note__: This function is partial.
--
-- > deleteAt 0  (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > deleteAt 1  (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > deleteAt 2 (fromList [(5,"a"), (3,"b")])     Error: index out of range
-- > deleteAt (-1) (fromList [(5,"a"), (3,"b")])  Error: index out of range

deleteAt :: Int -> Map k a -> Map k a
deleteAt !i t =
  case t of
    Tip -> error "Map.deleteAt: index out of range"
    Bin _ kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (deleteAt i l) r
      GT -> balanceL kx x l (deleteAt (i-sizeL-1) r)
      EQ -> glue l r
      where
        sizeL = size l


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- The KeyValue type is used when returning a key-value pair and helps GHC keep
-- track of the fact that key is in WHNF.
--
-- As an example, for a use case like
--
-- fmap (\(k,_) -> <strict use of k>) (lookupMin m)
--
-- on a non-empty map, GHC can decide to evaluate the usage of k if it is cheap
-- and put the result in the Just, instead of making a thunk for it.
-- If GHC does not know that k is in WHNF, it could be bottom, and so GHC must
-- always return Just with a thunk inside.

data KeyValue k a = KeyValue !k a

kvToTuple :: KeyValue k a -> (k, a)
kvToTuple (KeyValue k a) = (k, a)
{-# INLINE kvToTuple #-}

lookupMinSure :: k -> a -> Map k a -> KeyValue k a
lookupMinSure !k a Tip = KeyValue k a
lookupMinSure _ _ (Bin _ k a l _) = lookupMinSure k a l

-- | \(O(\log n)\). The minimal key of the map. Returns 'Nothing' if the map is empty.
--
-- > lookupMin (fromList [(5,"a"), (3,"b")]) == Just (3,"b")
-- > lookupMin empty = Nothing
--
-- @since 0.5.9

lookupMin :: Map k a -> Maybe (k,a)
lookupMin Tip = Nothing
lookupMin (Bin _ k x l _) = Just $! kvToTuple (lookupMinSure k x l)
{-# INLINE lookupMin #-} -- See Note [Inline lookupMin] in Data.Set.Internal

-- | \(O(\log n)\). The minimal key of the map. Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'lookupMin'.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element

findMin :: Map k a -> (k,a)
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "Map.findMin: empty map has no minimal element"

lookupMaxSure :: k -> a -> Map k a -> KeyValue k a
lookupMaxSure !k a Tip = KeyValue k a
lookupMaxSure _ _ (Bin _ k a _ r) = lookupMaxSure k a r

-- | \(O(\log n)\). The maximal key of the map. Returns 'Nothing' if the map is empty.
--
-- > lookupMax (fromList [(5,"a"), (3,"b")]) == Just (5,"a")
-- > lookupMax empty = Nothing
--
-- @since 0.5.9

lookupMax :: Map k a -> Maybe (k, a)
lookupMax Tip = Nothing
lookupMax (Bin _ k x _ r) = Just $! kvToTuple (lookupMaxSure k x r)
{-# INLINE lookupMax #-} -- See Note [Inline lookupMin] in Data.Set.Internal

-- | \(O(\log n)\). The maximal key of the map. Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'lookupMax'.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element

findMax :: Map k a -> (k,a)
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "Map.findMax: empty map has no maximal element"

-- | \(O(\log n)\). Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty

deleteMin :: Map k a -> Map k a
deleteMin (Bin _ _  _ Tip r)  = r
deleteMin (Bin _ kx x l r)    = balanceR kx x (deleteMin l) r
deleteMin Tip                 = Tip

-- | \(O(\log n)\). Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty

deleteMax :: Map k a -> Map k a
deleteMax (Bin _ _  _ l Tip)  = l
deleteMax (Bin _ kx x l r)    = balanceL kx x l (deleteMax r)
deleteMax Tip                 = Tip

-- | \(O(\log n)\). Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m

-- | \(O(\log n)\). Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f m
  = updateMaxWithKey (\_ x -> f x) m


-- | \(O(\log n)\). Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey _ Tip                 = Tip
updateMinWithKey f (Bin sx kx x Tip r) = case f kx x of
                                           Nothing -> r
                                           Just x' -> Bin sx kx x' Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r

-- | \(O(\log n)\). Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x' -> Bin sx kx x' l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)

-- | \(O(\log n)\). Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: Map k a -> Maybe ((k,a), Map k a)
minViewWithKey Tip = Nothing
minViewWithKey (Bin _ k x l r) = Just $
  case minViewSure k x l r of
    MinView km xm t -> ((km, xm), t)
-- We inline this to give GHC the best possible chance of getting
-- rid of the Maybe and pair constructors, as well as the thunk under
-- the Just.
{-# INLINE minViewWithKey #-}

-- | \(O(\log n)\). Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: Map k a -> Maybe ((k,a), Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey (Bin _ k x l r) = Just $
  case maxViewSure k x l r of
    MaxView km xm t -> ((km, xm), t)
-- See note on inlining at minViewWithKey
{-# INLINE maxViewWithKey #-}

-- | \(O(\log n)\). Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
-- > minView empty == Nothing

minView :: Map k a -> Maybe (a, Map k a)
minView t = case minViewWithKey t of
              Nothing -> Nothing
              Just ~((_, x), t') -> Just (x, t')

-- | \(O(\log n)\). Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
-- > maxView empty == Nothing

maxView :: Map k a -> Maybe (a, Map k a)
maxView t = case maxViewWithKey t of
              Nothing -> Nothing
              Just ~((_, x), t') -> Just (x, t')

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: (Foldable f, Ord k) => f (Map k a) -> Map k a
unions ts
  = Foldable.foldl' union empty ts
{-# INLINE unions #-} -- Inline for list fusion

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: (Foldable f, Ord k) => (a->a->a) -> f (Map k a) -> Map k a
unionsWith f ts
  = Foldable.foldl' (unionWith f) empty ts
{-# INLINABLE unionsWith #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: Ord k => Map k a -> Map k a -> Map k a
union t1 Tip  = t1
union t1 (Bin _ k x Tip Tip) = insertR k x t1
union (Bin _ k x Tip Tip) t2 = insert k x t2
union Tip t2 = t2
union t1@(Bin _ k1 x1 l1 r1) t2 = case split k1 t2 of
  (l2, r2) | l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1 -> t1
           | otherwise -> link k1 x1 l1l2 r1r2
           where !l1l2 = union l1 l2
                 !r1r2 = union r1 r2
{-# INLINABLE union #-}

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
--
-- Also see the performance note on 'fromListWith'.

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
-- QuickCheck says pointer equality never happens here.
unionWith _f t1 Tip = t1
unionWith f t1 (Bin _ k x Tip Tip) = insertWithR f k x t1
unionWith f (Bin _ k x Tip Tip) t2 = insertWith f k x t2
unionWith _f Tip t2 = t2
unionWith f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
      Nothing -> link k1 x1 l1l2 r1r2
      Just x2 -> link k1 (f x1 x2) l1l2 r1r2
    where !l1l2 = unionWith f l1 l2
          !r1r2 = unionWith f r1 r2
{-# INLINABLE unionWith #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
-- Union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
--
-- Also see the performance note on 'fromListWith'.

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _f t1 Tip = t1
unionWithKey f t1 (Bin _ k x Tip Tip) = insertWithKeyR f k x t1
unionWithKey f (Bin _ k x Tip Tip) t2 = insertWithKey f k x t2
unionWithKey _f Tip t2 = t2
unionWithKey f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
      Nothing -> link k1 x1 l1l2 r1r2
      Just x2 -> link k1 (f k1 x1 x2) l1l2 r1r2
    where !l1l2 = unionWithKey f l1 l2
          !r1r2 = unionWithKey f r1 r2
{-# INLINABLE unionWithKey #-}

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- We don't currently attempt to use any pointer equality tricks for
-- 'difference'. To do so, we'd have to match on the first argument
-- and split the second. Unfortunately, the proof of the time bound
-- relies on doing it the way we do, and it's not clear whether that
-- bound holds the other way.

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Difference of two maps.
-- Return elements of the first map not existing in the second map.
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 (Bin _ k _ l2 r2) = case split k t1 of
  (l1, r1)
    | size l1l2 + size r1r2 == size t1 -> t1
    | otherwise -> link2 l1l2 r1r2
    where
      !l1l2 = difference l1 l2
      !r1r2 = difference r1 r2
{-# INLINABLE difference #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Remove all keys in a 'Set' from a 'Map'.
--
-- @
-- m \`withoutKeys\` s = 'filterWithKey' (\\k _ -> k ``Set.notMember`` s) m
-- m \`withoutKeys\` s = m ``difference`` 'fromSet' (const ()) s
-- @
--
-- @since 0.5.8

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
withoutKeys Tip _ = Tip
withoutKeys m Set.Tip = m
withoutKeys m (Set.Bin _ k ls rs) = case splitMember k m of
  (lm, b, rm)
     | not b && lm' `ptrEq` lm && rm' `ptrEq` rm -> m
     | otherwise -> link2 lm' rm'
     where
       !lm' = withoutKeys lm ls
       !rm' = withoutKeys rm rs
{-# INLINABLE withoutKeys #-}

-- | \(O(n+m)\). Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f = merge preserveMissing dropMissing $
       zipWithMaybeMatched (\_ x y -> f x y)
{-# INLINABLE differenceWith #-}

-- | \(O(n+m)\). Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f =
  merge preserveMissing dropMissing (zipWithMaybeMatched f)
{-# INLINABLE differenceWithKey #-}


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1@(Bin _ k x l1 r1) t2
  | mb = if l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1
         then t1
         else link k x l1l2 r1r2
  | otherwise = link2 l1l2 r1r2
  where
    !(l2, mb, r2) = splitMember k t2
    !l1l2 = intersection l1 l2
    !r1r2 = intersection r1 r2
{-# INLINABLE intersection #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Restrict a 'Map' to only those keys
-- found in a 'Set'.
--
-- @
-- m \`restrictKeys\` s = 'filterWithKey' (\\k _ -> k ``Set.member`` s) m
-- m \`restrictKeys\` s = m ``intersection`` 'fromSet' (const ()) s
-- @
--
-- @since 0.5.8
restrictKeys :: Ord k => Map k a -> Set k -> Map k a
restrictKeys Tip _ = Tip
restrictKeys _ Set.Tip = Tip
restrictKeys m@(Bin _ k x l1 r1) s
  | b = if l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1
        then m
        else link k x l1l2 r1r2
  | otherwise = link2 l1l2 r1r2
  where
    !(l2, b, r2) = Set.splitMember k s
    !l1l2 = restrictKeys l1 l2
    !r1r2 = restrictKeys r1 r2
{-# INLINABLE restrictKeys #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
-- We have no hope of pointer equality tricks here because every single
-- element in the result will be a thunk.
intersectionWith _f Tip _ = Tip
intersectionWith _f _ Tip = Tip
intersectionWith f (Bin _ k x1 l1 r1) t2 = case mb of
    Just x2 -> link k (f x1 x2) l1l2 r1r2
    Nothing -> link2 l1l2 r1r2
  where
    !(l2, mb, r2) = splitLookup k t2
    !l1l2 = intersectionWith f l1 l2
    !r1r2 = intersectionWith f r1 r2
{-# INLINABLE intersectionWith #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _f Tip _ = Tip
intersectionWithKey _f _ Tip = Tip
intersectionWithKey f (Bin _ k x1 l1 r1) t2 = case mb of
    Just x2 -> link k (f k x1 x2) l1l2 r1r2
    Nothing -> link2 l1l2 r1r2
  where
    !(l2, mb, r2) = splitLookup k t2
    !l1l2 = intersectionWithKey f l1 l2
    !r1r2 = intersectionWithKey f r1 r2
{-# INLINABLE intersectionWithKey #-}

{--------------------------------------------------------------------
  Symmetric difference
--------------------------------------------------------------------}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
-- The symmetric difference of two maps.
--
-- The result contains entries whose keys appear in exactly one of the two maps.
--
-- @
-- symmetricDifference
--   (fromList [(0,\'q\'),(2,\'b\'),(4,\'w\'),(6,\'o\')])
--   (fromList [(0,\'e\'),(3,\'r\'),(6,\'t\'),(9,\'s\')])
-- ==
-- fromList [(2,\'b\'),(3,\'r\'),(4,\'w\'),(9,\'s\')]
-- @
--
-- @since 0.8
symmetricDifference :: Ord k => Map k a -> Map k a -> Map k a
symmetricDifference Tip t2 = t2
symmetricDifference t1 Tip = t1
symmetricDifference (Bin _ k x l1 r1) t2
  | found = link2 l1l2 r1r2
  | otherwise = link k x l1l2 r1r2
  where
    !(l2, found, r2) = splitMember k t2
    !l1l2 = symmetricDifference l1 l2
    !r1r2 = symmetricDifference r1 r2
{-# INLINABLE symmetricDifference #-}

{--------------------------------------------------------------------
  Disjoint
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Check whether the key sets of two
-- maps are disjoint (i.e., their 'intersection' is empty).
--
-- > disjoint (fromList [(2,'a')]) (fromList [(1,()), (3,())])   == True
-- > disjoint (fromList [(2,'a')]) (fromList [(1,'a'), (2,'b')]) == False
-- > disjoint (fromList [])        (fromList [])                 == True
--
-- @
-- xs ``disjoint`` ys = null (xs ``intersection`` ys)
-- @
--
-- @since 0.6.2.1

-- See 'Data.Set.Internal.isSubsetOfX' for some background
-- on the implementation design.
disjoint :: Ord k => Map k a -> Map k b -> Bool
disjoint Tip _ = True
disjoint _ Tip = True
disjoint (Bin 1 k _ _ _) t = k `notMember` t
disjoint (Bin _ k _ l r) t
  = not found && disjoint l lt && disjoint r gt
  where
    (lt,found,gt) = splitMember k t

{--------------------------------------------------------------------
  Compose
--------------------------------------------------------------------}
-- | Given maps @bc@ and @ab@, relate the keys of @ab@ to the values of @bc@,
-- by using the values of @ab@ as keys for lookups in @bc@.
--
-- Complexity: \( O (n \log m) \), where \(m\) is the size of the first argument
--
-- > compose (fromList [('a', "A"), ('b', "B")]) (fromList [(1,'a'),(2,'b'),(3,'z')]) = fromList [(1,"A"),(2,"B")]
--
-- @
-- ('compose' bc ab '!?') = (bc '!?') <=< (ab '!?')
-- @
--
-- __Note:__ Prior to v0.6.4, "Data.Map.Strict" exposed a version of
-- 'compose' that forced the values of the output 'Map'. This version does not
-- force these values.
--
-- ==== __Note on complexity__
--
-- This function is asymptotically optimal. Given @n :: Map a b, m :: Map b c@,
-- the composition essentially maps each @a@ in @n@ to @Maybe c@, since the
-- composed lookup yields either one of the @c@ in @m@ or @Nothing@. The number
-- of possible such mappings is \((|m| + 1) ^ {|n|}\).
-- We now follow a similar reasoning to the one for
-- [sorting](https://en.wikipedia.org/wiki/Comparison_sort#Number_of_comparisons_required_to_sort_a_list).
-- To distinguish between \(x\) possible values, we need
-- \( \lceil \log_2 x \rceil \) bits. Thus, we have a lower bound of
-- \(\log_2 \left((|m| + 1) ^{|n|} \right) = |n| \cdot \log_2 (|m| + 1)\) bits.
-- @Map@ lookups are comparison-based, and each comparison gives us at most
-- one bit of information: in the worst case we'll always be left with at least
-- half of the remaining possible values, meaning we need at least as many
-- comparisons as we need bits.
--
-- @since 0.6.3.1
compose :: Ord b => Map b c -> Map a b -> Map a c
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab

-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMissing f k x z @ is an abstract representation
-- of a function of type @ k -> x -> f (Maybe z) @.
--
-- @since 0.5.9

data WhenMissing f k x y = WhenMissing
  { missingSubtree :: Map k x -> f (Map k y)
  , missingKey :: k -> x -> f (Maybe y)}

-- | @since 0.5.9
instance Monad f => Functor (WhenMissing f k x) where
  fmap = mapWhenMissing
  {-# INLINE fmap #-}

-- | @since 0.5.9
instance Monad f => Category.Category (WhenMissing f k) where
  id = preserveMissing
  f . g = traverseMaybeMissing $
    \ k x -> missingKey g k x >>= \y ->
         case y of
           Nothing -> pure Nothing
           Just q -> missingKey f k q
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (MaybeT f)) @.
--
-- @since 0.5.9
instance Monad f => Applicative (WhenMissing f k x) where
  pure x = mapMissing (\ _ _ -> x)
  f <*> g = traverseMaybeMissing $ \k x -> do
         res1 <- missingKey f k x
         case res1 of
           Nothing -> pure Nothing
           Just r -> (pure $!) . fmap r =<< missingKey g k x
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (MaybeT f)) @.
--
-- @since 0.5.9
instance Monad f => Monad (WhenMissing f k x) where
  m >>= f = traverseMaybeMissing $ \k x -> do
         res1 <- missingKey m k x
         case res1 of
           Nothing -> pure Nothing
           Just r -> missingKey (f r) k x
  {-# INLINE (>>=) #-}

-- | Create a @WhenMissing@ from two functions.
--
-- @whenMissing@ must be called with two functions @f@ and @g@ such that
-- @g = 'traverseMaybeWithKey' f@. @g@ may be a more efficient way of applying
-- @f@ to all key-value pairs in a @Map@.
--
-- __Warning__: It is the caller's responsibility to ensure the above property.
--
-- === __Examples__
--
-- @
-- preserveMissing :: Applicative f => WhenMissing f k x x
-- preserveMissing = whenMissing f g
--   where
--     f _k x = pure (Just x)
--     g m = pure m
--     -- Note that this satisfies g = traverseMaybeWithKey f
-- @
--
-- @
-- import Data.Functor.Const (Const(..))
-- import Data.Monoid (All(..))
--
-- -- For a usage of this, see examples on mergeA
-- isEmpty :: WhenMissing (Const All) k x y
-- isEmpty = whenMissing f g
--   where
--     f _k _x = Const (All False)
--     g m = Const (All (null m))
--     -- Note that this satisfies g = traverseMaybeWithKey f
-- @
--
-- @since 0.8.1
whenMissing
  :: (k -> x -> f (Maybe y)) -> (Map k x -> f (Map k y)) -> WhenMissing f k x y
whenMissing = flip WhenMissing

-- | Map covariantly over a @'WhenMissing' f k x@.
--
-- @since 0.5.9
mapWhenMissing :: Monad f
               => (a -> b)
               -> WhenMissing f k x a -> WhenMissing f k x b
mapWhenMissing f t = WhenMissing
    { missingSubtree = \m -> missingSubtree t m >>= \m' -> pure $! fmap f m'
    , missingKey = \k x -> missingKey t k x >>= \q -> (pure $! fmap f q) }
{-# INLINE mapWhenMissing #-}

-- | Map covariantly over a @'WhenMissing' f k x@, using only a 'Functor f'
-- constraint.
mapGentlyWhenMissing :: Functor f
               => (a -> b)
               -> WhenMissing f k x a -> WhenMissing f k x b
mapGentlyWhenMissing f t = WhenMissing
    { missingSubtree = \m -> fmap f <$> missingSubtree t m
    , missingKey = \k x -> fmap f <$> missingKey t k x }
{-# INLINE mapGentlyWhenMissing #-}

-- | Map covariantly over a @'WhenMatched' f k x@, using only a 'Functor f'
-- constraint.
mapGentlyWhenMatched :: Functor f
               => (a -> b)
               -> WhenMatched f k x y a -> WhenMatched f k x y b
mapGentlyWhenMatched f t = zipWithMaybeAMatched $
  \k x y -> fmap f <$> runWhenMatched t k x y
{-# INLINE mapGentlyWhenMatched #-}

-- | Map contravariantly over a @'WhenMissing' f k _ x@.
--
-- @since 0.5.9
lmapWhenMissing :: (b -> a) -> WhenMissing f k a x -> WhenMissing f k b x
lmapWhenMissing f t = WhenMissing
  { missingSubtree = \m -> missingSubtree t (fmap f m)
  , missingKey = \k x -> missingKey t k (f x) }
{-# INLINE lmapWhenMissing #-}

-- | Map contravariantly over a @'WhenMatched' f k _ y z@.
--
-- @since 0.5.9
contramapFirstWhenMatched :: (b -> a)
                          -> WhenMatched f k a y z
                          -> WhenMatched f k b y z
contramapFirstWhenMatched f t = WhenMatched $
  \k x y -> runWhenMatched t k (f x) y
{-# INLINE contramapFirstWhenMatched #-}

-- | Map contravariantly over a @'WhenMatched' f k x _ z@.
--
-- @since 0.5.9
contramapSecondWhenMatched :: (b -> a)
                           -> WhenMatched f k x a z
                           -> WhenMatched f k x b z
contramapSecondWhenMatched f t = WhenMatched $
  \k x y -> runWhenMatched t k x (f y)
{-# INLINE contramapSecondWhenMatched #-}

-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge'.
--
-- A tactic of type @ SimpleWhenMissing k x z @ is an abstract representation
-- of a function of type @ k -> x -> Maybe z @.
--
-- @since 0.5.9
type SimpleWhenMissing = WhenMissing Identity

-- | A tactic for dealing with keys present in both
-- maps in 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMatched f k x y z @ is an abstract representation
-- of a function of type @ k -> x -> y -> f (Maybe z) @.
--
-- @since 0.5.9
newtype WhenMatched f k x y z = WhenMatched
  { matchedKey :: k -> x -> y -> f (Maybe z) }

-- | Along with zipWithMaybeAMatched, witnesses the isomorphism between
-- @WhenMatched f k x y z@ and @k -> x -> y -> f (Maybe z)@.
--
-- @since 0.5.9
runWhenMatched :: WhenMatched f k x y z -> k -> x -> y -> f (Maybe z)
runWhenMatched = matchedKey
{-# INLINE runWhenMatched #-}

-- | Along with traverseMaybeMissing, witnesses the isomorphism between
-- @WhenMissing f k x y@ and @k -> x -> f (Maybe y)@.
--
-- @since 0.5.9
runWhenMissing :: WhenMissing f k x y -> k -> x -> f (Maybe y)
runWhenMissing = missingKey
{-# INLINE runWhenMissing #-}

-- | @since 0.5.9
instance Functor f => Functor (WhenMatched f k x y) where
  fmap = mapWhenMatched
  {-# INLINE fmap #-}

-- | @since 0.5.9
instance Monad f => Category.Category (WhenMatched f k x) where
  id = zipWithMatched (\_ _ y -> y)
  f . g = zipWithMaybeAMatched $
            \k x y -> do
              res <- runWhenMatched g k x y
              case res of
                Nothing -> pure Nothing
                Just r -> runWhenMatched f k x r
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (ReaderT y (MaybeT f))) @
--
-- @since 0.5.9
instance Monad f => Applicative (WhenMatched f k x y) where
  pure x = zipWithMatched (\_ _ _ -> x)
  fs <*> xs = zipWithMaybeAMatched $ \k x y -> do
    res <- runWhenMatched fs k x y
    case res of
      Nothing -> pure Nothing
      Just r -> (pure $!) . fmap r =<< runWhenMatched xs k x y
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (ReaderT y (MaybeT f))) @
--
-- @since 0.5.9
instance Monad f => Monad (WhenMatched f k x y) where
  m >>= f = zipWithMaybeAMatched $ \k x y -> do
    res <- runWhenMatched m k x y
    case res of
      Nothing -> pure Nothing
      Just r -> runWhenMatched (f r) k x y
  {-# INLINE (>>=) #-}

-- | Map covariantly over a @'WhenMatched' f k x y@.
--
-- @since 0.5.9
mapWhenMatched :: Functor f
               => (a -> b)
               -> WhenMatched f k x y a
               -> WhenMatched f k x y b
mapWhenMatched f (WhenMatched g) = WhenMatched $ \k x y -> fmap (fmap f) (g k x y)
{-# INLINE mapWhenMatched #-}

-- | A tactic for dealing with keys present in both maps in 'merge'.
--
-- A tactic of type @ SimpleWhenMatched k x y z @ is an abstract representation
-- of a function of type @ k -> x -> y -> Maybe z @.
--
-- @since 0.5.9
type SimpleWhenMatched = WhenMatched Identity

-- | When a key is found in both maps, drop the key and values.
--
-- @since 0.8.1
dropMatched :: Applicative f => WhenMatched f k x y z
dropMatched = WhenMatched (\_ _ _ -> pure Nothing)
{-# INLINE dropMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values and use the result in the merged map.
--
-- @
-- zipWithMatched :: (k -> x -> y -> z)
--                -> SimpleWhenMatched k x y z
-- @
--
-- @since 0.5.9
zipWithMatched :: Applicative f
               => (k -> x -> y -> z)
               -> WhenMatched f k x y z
zipWithMatched f = WhenMatched $ \ k x y -> pure . Just $ f k x y
{-# INLINE zipWithMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values to produce an action and use its result in the merged map.
--
-- @since 0.5.9
zipWithAMatched :: Applicative f
                => (k -> x -> y -> f z)
                -> WhenMatched f k x y z
zipWithAMatched f = WhenMatched $ \ k x y -> Just <$> f k x y
{-# INLINE zipWithAMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values and maybe use the result in the merged map.
--
-- @
-- zipWithMaybeMatched :: (k -> x -> y -> Maybe z)
--                     -> SimpleWhenMatched k x y z
-- @
--
-- @since 0.5.9
zipWithMaybeMatched :: Applicative f
                    => (k -> x -> y -> Maybe z)
                    -> WhenMatched f k x y z
zipWithMaybeMatched f = WhenMatched $ \ k x y -> pure $ f k x y
{-# INLINE zipWithMaybeMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values, perform the resulting action, and maybe use
-- the result in the merged map.
--
-- This is the fundamental 'WhenMatched' tactic.
--
-- @since 0.5.9
zipWithMaybeAMatched :: (k -> x -> y -> f (Maybe z))
                     -> WhenMatched f k x y z
zipWithMaybeAMatched f = WhenMatched $ \ k x y -> f k x y
{-# INLINE zipWithMaybeAMatched #-}

-- | Drop all the entries whose keys are missing from the other
-- map.
--
-- @
-- dropMissing :: SimpleWhenMissing k x y
-- @
--
-- prop> dropMissing = mapMaybeMissing (\_ _ -> Nothing)
--
-- but @dropMissing@ is much faster.
--
-- @since 0.5.9
dropMissing :: Applicative f => WhenMissing f k x y
dropMissing = WhenMissing
  { missingSubtree = const (pure Tip)
  , missingKey = \_ _ -> pure Nothing }
{-# INLINE dropMissing #-}

-- | Preserve, unchanged, the entries whose keys are missing from
-- the other map.
--
-- @
-- preserveMissing :: SimpleWhenMissing k x x
-- @
--
-- prop> preserveMissing = Merge.Lazy.mapMaybeMissing (\_ x -> Just x)
--
-- but @preserveMissing@ is much faster.
--
-- @since 0.5.9
preserveMissing :: Applicative f => WhenMissing f k x x
preserveMissing = WhenMissing
  { missingSubtree = pure
  , missingKey = \_ v -> pure (Just v) }
{-# INLINE preserveMissing #-}

-- | Force the entries whose keys are missing from
-- the other map and otherwise preserve them unchanged.
--
-- @
-- preserveMissing' :: SimpleWhenMissing k x x
-- @
--
-- prop> preserveMissing' = Merge.Lazy.mapMaybeMissing (\_ x -> Just $! x)
--
-- but @preserveMissing'@ is quite a bit faster.
--
-- @since 0.5.9
preserveMissing' :: Applicative f => WhenMissing f k x x
preserveMissing' = WhenMissing
  { missingSubtree = \t -> pure $! forceTree t `seq` t
  , missingKey = \_ v -> pure $! Just $! v }
{-# INLINE preserveMissing' #-}

-- Force all the values in a tree.
forceTree :: Map k a -> ()
forceTree (Bin _ _ v l r) = v `seq` forceTree l `seq` forceTree r `seq` ()
forceTree Tip = ()

-- | Map over the entries whose keys are missing from the other map.
--
-- @
-- mapMissing :: (k -> x -> y) -> SimpleWhenMissing k x y
-- @
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
--
-- @since 0.5.9
mapMissing :: Applicative f => (k -> x -> y) -> WhenMissing f k x y
mapMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapWithKey f m
  , missingKey = \ k x -> pure $ Just (f k x) }
{-# INLINE mapMissing #-}

-- | Map over the entries whose keys are missing from the other map,
-- optionally removing some. This is the most powerful 'SimpleWhenMissing'
-- tactic, but others are usually more efficient.
--
-- @
-- mapMaybeMissing :: (k -> x -> Maybe y) -> SimpleWhenMissing k x y
-- @
--
-- prop> mapMaybeMissing f = traverseMaybeMissing (\k x -> pure (f k x))
--
-- but @mapMaybeMissing@ uses fewer unnecessary 'Applicative' operations.
--
-- @since 0.5.9
mapMaybeMissing :: Applicative f => (k -> x -> Maybe y) -> WhenMissing f k x y
mapMaybeMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapMaybeWithKey f m
  , missingKey = \k x -> pure $! f k x }
{-# INLINE mapMaybeMissing #-}

-- | Filter the entries whose keys are missing from the other map.
--
-- @
-- filterMissing :: (k -> x -> Bool) -> SimpleWhenMissing k x x
-- @
--
-- prop> filterMissing f = Merge.Lazy.mapMaybeMissing $ \k x -> guard (f k x) *> Just x
--
-- but this should be a little faster.
--
-- @since 0.5.9
filterMissing :: Applicative f
              => (k -> x -> Bool) -> WhenMissing f k x x
filterMissing f = WhenMissing
  { missingSubtree = \m -> pure $! filterWithKey f m
  , missingKey = \k x -> pure $! if f k x then Just x else Nothing }
{-# INLINE filterMissing #-}

-- | Filter the entries whose keys are missing from the other map
-- using some 'Applicative' action.
--
-- > filterAMissing f = Merge.Lazy.traverseMaybeMissing $
-- >   \k x -> (\b -> guard b *> Just x) <$> f k x
--
-- but this should be a little faster.
--
-- @since 0.5.9
filterAMissing :: Applicative f
              => (k -> x -> f Bool) -> WhenMissing f k x x
filterAMissing f = WhenMissing
  { missingSubtree = \m -> filterWithKeyA f m
  , missingKey = \k x -> bool Nothing (Just x) <$> f k x }
{-# INLINE filterAMissing #-}

-- | This wasn't in Data.Bool until 4.7.0, so we define it here
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t

-- | Traverse over the entries whose keys are missing from the other map.
--
-- @since 0.5.9
traverseMissing :: Applicative f
                    => (k -> x -> f y) -> WhenMissing f k x y
traverseMissing f = WhenMissing
  { missingSubtree = traverseWithKey f
  , missingKey = \k x -> Just <$> f k x }
{-# INLINE traverseMissing #-}

-- | Traverse over the entries whose keys are missing from the other map,
-- optionally producing values to put in the result.
-- This is the most powerful 'WhenMissing' tactic, but others are usually
-- more efficient.
--
-- @since 0.5.9
traverseMaybeMissing :: Applicative f
                      => (k -> x -> f (Maybe y)) -> WhenMissing f k x y
traverseMaybeMissing f = WhenMissing
  { missingSubtree = traverseMaybeWithKey f
  , missingKey = f }
{-# INLINE traverseMaybeMissing #-}

-- | Merge two maps.
--
-- 'merge' takes two 'SimpleWhenMissing' tactics, a 'SimpleWhenMatched'
-- tactic and two maps. It uses the tactics to merge the maps.
-- Its behavior is best understood via its fundamental tactics,
-- 'mapMaybeMissing' and 'zipWithMaybeMatched'.
--
-- Consider
--
-- @
-- merge (mapMaybeMissing g1) (mapMaybeMissing g2) (zipWithMaybeMatched f) m1 m2
-- @
--
-- @
-- g1 k x = if k == 2 then Just ("1" ++ x) else Nothing
-- g2 k x = if k == 3 then Just ("2" ++ x) else Nothing
-- f k x y = if k == 6 then Just ("3" ++ x ++ y) else Nothing
-- m1 = fromList [(2,"a"), (4,"b"), (6,"c"), (8,"d"), (10,"e"), (12,"f")]
-- m2 = fromList [(3,"g"), (6,"h"), (9,"i"), (12,"j")]
-- @
--
-- 'merge' will pass the keys and values to @g1@, @g2@, or @f@ as appropriate,
-- producing a @Maybe@ for each element.
--
-- @
-- m1:      [ (2, "a"),            (4, "b"),    (6, "c"), (8, "d"),           (10, "e"),    (12, "f")]
-- m2:      [            (3, "g"),              (6, "h"),           (9, "i"),               (12, "j")]
-- result:  [ g1 2 "a",  g2 3 "g", g1 4 "b", f 6 "c" "h", g1 8 "d", g2 9 "i", g1 10 "e", f 12 "f" "j"]
--        = [Just "1a", Just "2g",  Nothing,  Just "3ch",  Nothing,  Nothing,   Nothing,      Nothing]
-- @
--
-- The result map contains the @Just@ values.
--
-- >>> merge (mapMaybeMissing g1) (mapMaybeMissing g2) (zipWithMaybeMatched f) m1 m2
-- fromList [(2,"1a"), (3,"2g"), (6,"3ch")]
--
-- The other tactics below are optimizations or simplifications of
-- 'mapMaybeMissing' for special cases. Most importantly,
--
-- * 'dropMissing' drops all the keys.
-- * 'preserveMissing' leaves all the entries alone.
--
-- When 'merge' is given three arguments, it is inlined at the call
-- site. To prevent excessive inlining, you should typically use 'merge'
-- to define your custom combining functions.
--
--
-- Examples:
--
-- prop> unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)
-- prop> intersectionWithKey f = merge dropMissing dropMissing (zipWithMatched f)
-- prop> differenceWith f = merge preserveMissing dropMissing (zipWithMatched f)
-- prop> symmetricDifference = merge preserveMissing preserveMissing (zipWithMaybeMatched $ \ _ _ _ -> Nothing)
-- prop> mapEachPiece f g h = merge (mapMissing f) (mapMissing g) (zipWithMatched h)
--
-- @since 0.5.9
merge :: Ord k
             => SimpleWhenMissing k a c -- ^ What to do with keys in @m1@ but not @m2@
             -> SimpleWhenMissing k b c -- ^ What to do with keys in @m2@ but not @m1@
             -> SimpleWhenMatched k a b c -- ^ What to do with keys in both @m1@ and @m2@
             -> Map k a -- ^ Map @m1@
             -> Map k b -- ^ Map @m2@
             -> Map k c
merge g1 g2 f = \m1 m2 -> runIdentity $
  mergeA g1 g2 f m1 m2
{-# INLINE merge #-}

-- | An applicative version of 'merge'.
--
-- 'mergeA' takes two 'WhenMissing' tactics, a 'WhenMatched'
-- tactic and two maps. It uses the tactics to merge the maps.
-- Its behavior is best understood via its fundamental tactics,
-- 'traverseMaybeMissing' and 'zipWithMaybeAMatched'.
--
-- Behaves just like 'merge' while allowing @Applicative@ effects. Effects are
-- performed in increasing order of keys.
--
-- Consider
--
-- @
-- mergeA (traverseMaybeMissing g1)
--        (traverseMaybeMissing g2)
--        (zipWithMaybeAMatched f)
--        m1
--        m2
-- @
--
-- @
-- g1 k x = let z = if k == 2 then Just ("1" ++ x) else Nothing
--          in z <$ putStrLn ("g1 " ++ show (k, x))
-- g2 k x = let z = if k == 3 then Just ("2" ++ x) else Nothing
--          in z <$ putStrLn ("g2 " ++ show (k, x))
-- f k x y = let z = if k == 6 then Just ("3" ++ x ++ y) else Nothing
--           in z <$ putStrLn ("f " ++ show (k, x, y))
-- m1 = fromList [(2,"a"), (4,"b"), (6,"c"), (8,"d"), (10,"e"), (12,"f")]
-- m2 = fromList [(3,"g"), (6,"h"), (9,"i"), (12,"j")]
-- @
--
-- As with 'merge', the result map is @[(2,"1a"), (3,"2g"), (6,"3ch")]@.
-- Additionally, @g1@, @g2@, and @f@ perform @IO@ effects, printing in
-- increasing order of key.
--
-- >>> mergeA (traverseMaybeMissing g1) (traverseMaybeMissing g2) (zipWithMaybeAMatched f) m1 m2
-- g1 (2,"a")
-- g2 (3,"g")
-- g1 (4,"b")
-- f (6,"c","h")
-- g1 (8,"d")
-- g2 (9,"i")
-- g1 (10,"e")
-- f (12,"f","j")
-- fromList [(2,"1a"),(3,"2g"),(6,"3ch")]
--
-- The other tactics below are optimizations or simplifications of
-- 'traverseMaybeMissing' for special cases. Most importantly,
--
-- * 'dropMissing' drops all the keys.
-- * 'preserveMissing' leaves all the entries alone.
-- * 'mapMaybeMissing' does not use the 'Applicative' context.
--
-- When 'mergeA' is given three arguments, it is inlined at the call
-- site. To prevent excessive inlining, you should generally only use
-- 'mergeA' to define custom combining functions.
--
-- === __Examples__
--
-- @
-- data Pair a = Pair !a !a deriving Functor
--
-- instance Applicative Pair where
--    pure x = Pair x x
--    liftA2 f (Pair x1 y1) (Pair x2 y2) = Pair (f x1 x2) (f y1 y2)
--
-- -- | Calculate the left-biased union and intersection of two maps.
-- unionIntersection :: Ord k => Map k a -> Map k a -> (Map k a, Map k a)
-- unionIntersection m1 m2 =
--   case mergeA preserveAndDropMissing preserveAndDropMissing preserveLeftMatched m1 m2 of
--     Pair mu mi -> (mu, mi)
--   where
--     -- use Pair to build the union and intersection together
--     preserveAndDropMissing = 'whenMissing' (\\_k x -> Pair (Just x) Nothing) (\\m -> Pair m empty)
--     preserveLeftMatched = 'zipWithMaybeAMatched' (\\_k x1 _x2 -> Pair (Just x1) (Just x1))
-- @
--
-- @
-- import Data.Functor.Const (Const(..))
-- import Data.Monoid (All(..))
--
-- -- | Whether the keys of the first map are a subset of the keys of the second map.
-- keysAreSubsetOf :: Ord k => Map k a -> Map k b -> Bool
-- keysAreSubsetOf m1 m2 =
--   getAll (getConst (mergeA isEmpty 'dropMissing' 'dropMatched' m1 m2))
--   where
--     isEmpty = 'whenMissing' (\\_k _x -> Const (All False)) (\\m -> Const (All (null m)))
-- @
--
-- @since 0.5.9
mergeA
  :: (Applicative f, Ord k)
  => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Map k a -- ^ Map @m1@
  -> Map k b -- ^ Map @m2@
  -> f (Map k c)
mergeA
    WhenMissing{missingSubtree = g1t, missingKey = g1k}
    WhenMissing{missingSubtree = g2t}
    (WhenMatched f) = go
  where
    go t1 Tip = g1t t1
    go Tip t2 = g2t t2
    go (Bin _ kx x1 l1 r1) t2 = case splitLookup kx t2 of
      (l2, mx2, r2) -> case mx2 of
          Nothing -> liftA3 (\l' mx' r' -> maybe link2 (link kx) mx' l' r')
                        l1l2 (g1k kx x1) r1r2
          Just x2 -> liftA3 (\l' mx' r' -> maybe link2 (link kx) mx' l' r')
                        l1l2 (f kx x1 x2) r1r2
        where
          !l1l2 = go l1 l2
          !r1r2 = go r1 r2
{-# INLINE mergeA #-}


{--------------------------------------------------------------------
  MergeWithKey
--------------------------------------------------------------------}

-- | \(O(n+m)\). An unsafe general combining function.
--
-- __Warning__: This function can produce corrupt maps and its results
-- may depend on the internal structures of its inputs. Users should
-- prefer 'merge' or 'mergeA'.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'Map's is created, such that
--
-- * if a key is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the key is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily. Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@,
-- @'filterWithKey' f@, or @'mapMaybeWithKey' f@ could be used for any @f@.

mergeWithKey :: Ord k
             => (k -> a -> b -> Maybe c)
             -> (Map k a -> Map k c)
             -> (Map k b -> Map k c)
             -> Map k a -> Map k b -> Map k c
mergeWithKey f g1 g2 = go
  where
    go Tip Tip = Tip
    go Tip t2 = g2 t2
    go t1 Tip = g1 t1
    go (Bin _ kx x l1 r1) t2 =
      case found of
        Nothing -> case g1 (singleton kx x) of
                     Tip -> link2 l' r'
                     (Bin _ _ x' Tip Tip) -> link kx x' l' r'
                     _ -> error "mergeWithKey: Given function only1 does not fulfill required conditions (see documentation)"
        Just x2 -> case f kx x x2 of
                     Nothing -> link2 l' r'
                     Just x' -> link kx x' l' r'
      where
        (l2, found, r2) = splitLookup kx t2
        l' = go l1 l2
        r' = go r1 r2
{-# INLINE mergeWithKey #-}

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
--
isSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2
{-# INLINABLE isSubmapOf #-}

{- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

 > isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)])

 But the following are all 'False':

 > isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)])

 Note that @isSubmapOfBy (\_ _ -> True) m1 m2@ tests whether all the keys
 in @m1@ are also keys in @m2@.

-}
isSubmapOfBy :: Ord k => (a->b->Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f t1 t2
  = size t1 <= size t2 && submap' f t1 t2
{-# INLINABLE isSubmapOfBy #-}

-- Test whether a map is a submap of another without the *initial*
-- size test. See Data.Set.Internal.isSubsetOfX for notes on
-- implementation and analysis.
submap' :: Ord a => (b -> c -> Bool) -> Map a b -> Map a c -> Bool
submap' _ Tip _ = True
submap' _ _ Tip = False
submap' f (Bin 1 kx x _ _) t
  = case lookup kx t of
      Just y -> f x y
      Nothing -> False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> f x y
                 && size l <= size lt && size r <= size gt
                 && submap' f l lt && submap' f r gt
  where
    (lt,found,gt) = splitLookup kx t
{-# INLINABLE submap' #-}

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2
{-# INLINABLE isProperSubmapOf #-}

{- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @keys m1@ and @keys m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])


-}
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f t1 t2
  = size t1 < size t2 && submap' f t1 t2
{-# INLINABLE isProperSubmapOfBy #-}

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | \(O(n)\). Keep all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (a -> Bool) -> Map k a -> Map k a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | \(O(n)\). Keep all keys that satisfy the predicate.
--
-- @
-- filterKeys p = 'filterWithKey' (\\k _ -> p k)
-- @
--
-- > filterKeys (> 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
--
-- @since 0.8

filterKeys :: (k -> Bool) -> Map k a -> Map k a
filterKeys p m = filterWithKey (\k _ -> p k) m

-- | \(O(n)\). Keep all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p t@(Bin _ kx x l r)
  | p kx x    = if pl `ptrEq` l && pr `ptrEq` r
                then t
                else link kx x pl pr
  | otherwise = link2 pl pr
  where !pl = filterWithKey p l
        !pr = filterWithKey p r

-- | \(O(n)\). Filter keys and values using an 'Applicative'
-- predicate.
filterWithKeyA :: Applicative f => (k -> a -> f Bool) -> Map k a -> f (Map k a)
filterWithKeyA _ Tip = pure Tip
filterWithKeyA p t@(Bin _ kx x l r) =
  liftA3 combine (filterWithKeyA p l) (p kx x) (filterWithKeyA p r)
  where
    combine pl True pr
      | pl `ptrEq` l && pr `ptrEq` r = t
      | otherwise = link kx x pl pr
    combine pl False pr = link2 pl pr

-- | \(O(\log n)\). Take while a predicate on the keys holds.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' (p . fst) . 'toList'
-- takeWhileAntitone p = 'filterWithKey' (\k _ -> p k)
-- @
--
-- @since 0.5.8

takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone _ Tip = Tip
takeWhileAntitone p (Bin _ kx x l r)
  | p kx = linkL kx x l (takeWhileAntitone p r)
  | otherwise = takeWhileAntitone p l

-- | \(O(\log n)\). Drop while a predicate on the keys holds.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' (p . fst) . 'toList'
-- dropWhileAntitone p = 'filterWithKey' (\\k _ -> not (p k))
-- @
--
-- @since 0.5.8

dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone _ Tip = Tip
dropWhileAntitone p (Bin _ kx x l r)
  | p kx = dropWhileAntitone p r
  | otherwise = linkR kx x (dropWhileAntitone p l) r

-- | \(O(\log n)\). Divide a map at the point where a predicate on the keys stops holding.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = partitionWithKey (\\k _ -> p k) xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the map
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first key and to fail
-- after the last key).
--
-- @since 0.5.8

spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
spanAntitone p0 m = toPair (go p0 m)
  where
    go _ Tip = Tip :*: Tip
    go p (Bin _ kx x l r)
      | p kx = let u :*: v = go p r in linkL kx x l u :*: v
      | otherwise = let u :*: v = go p l in u :*: linkR kx x v r

-- | \(O(n)\). Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (a -> Bool) -> Map k a -> (Map k a,Map k a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | \(O(n)\). Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a,Map k a)
partitionWithKey p0 t0 = toPair $ go p0 t0
  where
    go _ Tip = (Tip :*: Tip)
    go p t@(Bin _ kx x l r)
      | p kx x    = (if l1 `ptrEq` l && r1 `ptrEq` r
                     then t
                     else link kx x l1 r1) :*: link2 l2 r2
      | otherwise = link2 l1 r1 :*:
                    (if l2 `ptrEq` l && r2 `ptrEq` r
                     then t
                     else link kx x l2 r2)
      where
        (l1 :*: l2) = go p l
        (r1 :*: r2) = go p r

-- | \(O(n)\). Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | \(O(n)\). Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
  Just y  -> link kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> link2 (mapMaybeWithKey f l) (mapMaybeWithKey f r)

-- | \(O(n)\). Traverse keys\/values and collect the 'Just' results.
--
-- @since 0.5.8
traverseMaybeWithKey :: Applicative f
                     => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseMaybeWithKey = go
  where
    go _ Tip = pure Tip
    go f (Bin _ kx x Tip Tip) = maybe Tip (\x' -> Bin 1 kx x' Tip Tip) <$> f kx x
    go f (Bin _ kx x l r) = liftA3 combine (go f l) (f kx x) (go f r)
      where
        combine !l' mx !r' = case mx of
          Nothing -> link2 l' r'
          Just x' -> link kx x' l' r'
{-# INLINABLE traverseMaybeWithKey #-}

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | \(O(n)\). Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f0 t0 = toPair $ go f0 t0
  where
    go _ Tip = (Tip :*: Tip)
    go f (Bin _ kx x l r) = case f kx x of
      Left y  -> link kx y l1 r1 :*: link2 l2 r2
      Right z -> link2 l1 r1 :*: link kx z l2 r2
     where
        (l1 :*: l2) = go f l
        (r1 :*: r2) = go f r

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | \(O(n)\). Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> Map k a -> Map k b
map f = go where
  go Tip = Tip
  go (Bin sx kx x l r) = Bin sx kx (f x) (go l) (go r)
-- We use a `go` function to allow `map` to inline. This makes
-- a big difference if someone uses `map (const x) m` instead
-- of `x <$ m`; it doesn't seem to do any harm.

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
"map/coerce" map coerce = coerce
 #-}
#endif

-- | \(O(n)\). Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}
#endif

-- | \(O(n)\).
-- @'traverseWithKey' f m == 'fromList' \<$\> 'traverse' (\\(k, v) -> (,) k \<$\> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey f = go
  where
    go Tip = pure Tip
    go (Bin 1 k v _ _) = (\v' -> Bin 1 k v' Tip Tip) <$> f k v
    go (Bin s k v l r) = liftA3 (flip (Bin s k)) (go l) (f k v) (go r)
{-# INLINE traverseWithKey #-}

-- | \(O(n)\). The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | \(O(n)\). The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | \(O(n)\). The function 'mapAccumL' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumL _ a Tip               = (a,Tip)
mapAccumL f a (Bin sx kx x l r) =
  let (a1,l') = mapAccumL f a l
      (a2,x') = f a1 kx x
      (a3,r') = mapAccumL f a2 r
  in (a3,Bin sx kx x' l' r')

-- | \(O(n)\). The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r) =
  let (a1,r') = mapAccumRWithKey f a r
      (a2,x') = f a1 kx x
      (a3,l') = mapAccumRWithKey f a2 l
  in (a3,Bin sx kx x' l' r')

-- | \(O(n \log n)\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- If `f` is monotonically non-decreasing, this function takes \(O(n)\) time.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: Ord k2 => (k1->k2) -> Map k1 a -> Map k2 a
mapKeys f m = finishB (foldlWithKey' (\b kx x -> insertB (f kx) x b) emptyB m)
{-# INLINABLE mapKeys #-}

-- | \(O(n \log n)\).
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- If `f` is monotonically non-decreasing, this function takes \(O(n)\) time.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@. The value at the greater of the two original keys
-- is used as the first argument to @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
--
-- Also see the performance note on 'fromListWith'.

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1->k2) -> Map k1 a -> Map k2 a
mapKeysWith c f m =
  finishB (foldlWithKey' (\b kx x -> insertWithB c (f kx) x b) emptyB m)
{-# INLINABLE mapKeysWith #-}


-- | \(O(n)\).
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- __Warning__: This function should be used only if @f@ is monotonically
-- strictly increasing. This precondition is not checked. Use 'mapKeys' if the
-- precondition may not hold.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False

mapKeysMonotonic :: (k1->k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic f = mapAssocsMonotonic (\k x -> (f k, x))

-- | \(O(n)\). Map over keys and values with a function @f@ that is
-- monotonically strictly increasing in the keys. That is, for keys @kx@ and
-- @ky@ and values @x@ and @y@, if @kx@ < @ky@ then
-- @fst (f kx x)@ < @fst (f ky y)@.
--
-- __Warning__: This function should be used only if @f@ is monotonically
-- strictly increasing in the key. This precondition is not checked.
--
-- @since 0.8.1
mapAssocsMonotonic :: (k1 -> a1 -> (k2, a2)) -> Map k1 a1 -> Map k2 a2
mapAssocsMonotonic f = go
  where
    go Tip = Tip
    go (Bin sz k1 x1 l r) = case f k1 x1 of
      (k2, x2) -> Bin sz k2 x2 (go l) (go r)
-- See Note [INLINABLE to expose unfoldings] in Data.IntMap.Internal
{-# INLINABLE mapAssocsMonotonic #-}

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | \(O(n)\). Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr #-}

-- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' f z = go z
  where
    go !z' Tip            = z'
    go z' (Bin _ _ x l r) = go (f x $! go z' r) l
{-# INLINE foldr' #-}

-- | \(O(n)\). Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl #-}

-- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldl' f z = go z
  where
    go !z' Tip            = z'
    go z' (Bin _ _ x l r) =
      let !z'' = go z' l
      in go (f z'' x) r
{-# INLINE foldl' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l
{-# INLINE foldrWithKey #-}

-- | \(O(n)\). A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z = go z
  where
    go !z' Tip              = z'
    go z' (Bin _ kx x l r) = go (f kx x $! go z' r) l
{-# INLINE foldrWithKey' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey f z = go z
  where
    go z' Tip              = z'
    go z' (Bin _ kx x l r) = go (f (go z' l) kx x) r
{-# INLINE foldlWithKey #-}

-- | \(O(n)\). A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' f z = go z
  where
    go !z' Tip             = z'
    go z' (Bin _ kx x l r) =
      let !z'' = go z' l
      in go (f z'' kx x) r
{-# INLINE foldlWithKey' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
--
-- @since 0.5.4
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
foldMapWithKey f = go
  where
    go Tip             = mempty
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l `mappend` (f k v `mappend` go r)
{-# INLINE foldMapWithKey #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | \(O(n)\).
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: Map k a -> [a]
elems = foldr (:) []

-- | \(O(n)\). Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: Map k a -> [k]
keys = foldrWithKey (\k _ ks -> k : ks) []

-- | \(O(n)\). An alias for 'toAscList'. Return all key\/value pairs in the map
-- in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: Map k a -> [(k,a)]
assocs m
  = toAscList m

-- | \(O(n)\). The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty

keysSet :: Map k a -> Set.Set k
keysSet Tip = Set.Tip
keysSet (Bin sz kx _ l r) = Set.Bin sz kx (keysSet l) (keysSet r)

-- | \(O(n)\). The set of all elements of the map contained in 'Arg's.
--
-- > argSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [Arg 3 "b",Arg 5 "a"]
-- > argSet empty == Data.Set.empty
--
-- @since 0.6.6
argSet :: Map k a -> Set.Set (Arg k a)
argSet Tip = Set.Tip
argSet (Bin sz kx x l r) = Set.Bin sz (Arg kx x) (argSet l) (argSet r)

-- | \(O(n)\). Build a map from a 'Set' of keys and a function which for each
-- key computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.fromList [3,5]) == fromList [(3,"aaa"), (5,"aaaaa")]

fromSet :: (k -> a) -> Set k -> Map k a
#ifdef __GLASGOW_HASKELL__
fromSet =
  (coerce :: ((k -> Identity a) -> Set k -> Identity (Map k a))
          -> (k -> a) -> Set k -> Map k a)
    fromSetA
#else
fromSet f = runIdentity . fromSetA (pure . f)
#endif

-- | \(O(n)\). Build a map from a 'Set' of keys and a function which for each
-- key computes its value in an 'Applicative' context.
--
-- The @Applicative@ actions are sequenced in order of increasing key.
--
-- > let f k = if k == 0 then Nothing else Just (6 `div` k)
-- > fromSetA f (Data.Set.fromList [1,2,3,4]) == Just (fromList [(1,6),(2,3),(3,2),(4,1)])
-- > fromSetA f (Data.Set.fromList [0,1,2]) == Nothing
--
-- @since 0.8.1
fromSetA :: Applicative f => (k -> f a) -> Set k -> f (Map k a)
fromSetA _ Set.Tip = pure Tip
fromSetA f (Set.Bin sz x l r) =
  liftA3 (flip (Bin sz x)) (fromSetA f l) (f x) (fromSetA f r)
{-# INLINABLE fromSetA #-}

-- | \(O(n)\). Build a map from a 'Set' of keys and a function which for each
-- key optionally computes its value.
--
-- > let f k = if even k then Just (replicate k 'a') else Nothing
-- > fromSetMaybe f (Data.Set.fromList [1,2,3,4]) == fromList [(2,"aa"), (4,"aaaa")]
--
-- @since 0.8.1
fromSetMaybe :: (k -> Maybe a) -> Set k -> Map k a
#ifdef __GLASGOW_HASKELL__
fromSetMaybe =
  (coerce :: ((k -> Identity (Maybe a)) -> Set k -> Identity (Map k a))
          -> (k -> Maybe a) -> Set k -> Map k a)
     fromSetMaybeA
#else
fromSetMaybe f s = runIdentity (fromSetMaybeA (Identity . f) s)
#endif

-- | \(O(n)\). Build a map from a 'Set' of keys and a function which for each
-- key optionally computes its values in an 'Applicative' context.
--
-- The @Applicative@ actions are sequenced in order of increasing key.
--
-- @since 0.8.1
fromSetMaybeA :: Applicative f => (k -> f (Maybe a)) -> Set k -> f (Map k a)
fromSetMaybeA f = go
  where
    go Set.Tip = pure Tip
    go (Set.Bin _ k l r) =
      liftA3 (\l' mx r' -> maybe link2 (link k) mx l' r') (go l) (f k) (go r)
{-# INLINABLE fromSetMaybeA #-}

-- | \(O(n)\). Build a map from a set of elements contained inside 'Arg's.
--
-- > fromArgSet (Data.Set.fromList [Arg 3 "aaa", Arg 5 "aaaaa"]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromArgSet Data.Set.empty == empty
--
-- @since 0.6.6
fromArgSet :: Set.Set (Arg k a) -> Map k a
fromArgSet Set.Tip = Tip
fromArgSet (Set.Bin sz (Arg x v) l r) = Bin sz x v (fromArgSet l) (fromArgSet r)

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.6.2
instance (Ord k) => GHCExts.IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromList = fromList
  toList   = toList
#endif

-- | \(O(n \log n)\). Build a map from a list of key\/value pairs.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- If the keys are in non-decreasing order, this function takes \(O(n)\) time.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: Ord k => [(k,a)] -> Map k a
fromList xs = finishB (Foldable.foldl' (\b (kx, x) -> insertB kx x b) emptyB xs)
{-# INLINE fromList #-} -- INLINE for fusion

-- | \(O(n \log n)\). Build a map from a list of key\/value pairs with a combining function.
--
-- If the keys are in non-decreasing order, this function takes \(O(n)\) time.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"x"), (5,"c")] == fromList [(3, "x"), (5, "cba")]
-- > fromListWith (++) [] == empty
--
-- Note the reverse ordering of @"cba"@ in the example.
--
-- The symmetric combining function @f@ is applied in a left-fold over the list, as @f new old@.
--
-- See also: 'fromListUpsert'
--
-- === Performance
--
-- You should ensure that the given @f@ is fast with this order of arguments.
--
-- Symmetric functions may be slow in one order, and fast in another.
-- For the common case of collecting values of matching keys in a list, as above:
--
-- The complexity of @(++) a b@ is \(O(a)\), so it is fast when given a short list as its first argument.
-- Thus:
--
-- > fromListWith       (++)  (replicate 1000000 (3, "x"))   -- O(n),  fast
-- > fromListWith (flip (++)) (replicate 1000000 (3, "x"))   -- O(n²), extremely slow
--
-- because they evaluate as, respectively:
--
-- > fromList [(3, "x" ++ ("x" ++ "xxxxx..xxxxx"))]   -- O(n)
-- > fromList [(3, ("xxxxx..xxxxx" ++ "x") ++ "x")]   -- O(n²)
--
-- Thus, to get good performance with an operation like @(++)@ while also preserving
-- the same order as in the input list, reverse the input:
--
-- > fromListWith (++) (reverse [(5,"a"), (5,"b"), (5,"c")]) == fromList [(5, "abc")]
--
-- and it is always fast to combine singleton-list values @[v]@ with @fromListWith (++)@, as in:
--
-- > fromListWith (++) $ reverse $ map (\(k, v) -> (k, [v])) someListOfTuples

fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
fromListWith f xs =
  finishB (Foldable.foldl' (\b (kx, x) -> insertWithB f kx x b) emptyB xs)
{-# INLINE fromListWith #-}  -- INLINE for fusion

-- | \(O(n \log n)\). Build a map from a list of key\/value pairs with a combining function.
--
-- If the keys are in non-decreasing order, this function takes \(O(n)\) time.
--
-- > let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty
--
-- Also see the performance note on 'fromListWith'.
--
-- See also: 'fromListUpsert'

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromListWithKey f xs =
  finishB (Foldable.foldl' (\b (kx, x) -> insertWithB (f kx) kx x b) emptyB xs)
{-# INLINE fromListWithKey #-}  -- INLINE for fusion

-- | \(O(n \log n)\). Build a map from a list of key\/value pairs with a
-- combining function.
--
-- If the keys are in non-decreasing order, this function takes \(O(n)\) time.
--
-- The result is equivalent to performing an @upsert@ for every key\/value in
-- the list.
--
-- @
-- fromListUpsert f = foldl' (\\m (k, x) -> 'upsert' (f x) k m) 'empty'
-- @
--
-- > let f x = maybe [x] (x:)
-- > fromListUpsert f [(5,'a'), (5,'b'), (3,'c'), (3,'d'), (5,'e')] == fromList [(3,"dc"), (5,"eba")]
--
-- @since 0.8.1
fromListUpsert :: Ord k => (a -> Maybe b -> b) -> [(k, a)] -> Map k b
fromListUpsert f xs =
  finishB (Foldable.foldl' (\b (kx, x) -> upsertB (f x) kx b) emptyB xs)
{-# INLINE fromListUpsert #-}  -- INLINE for fusion

-- | \(O(n)\). Convert the map to a list of key\/value pairs. Subject to list fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: Map k a -> [(k,a)]
toList = toAscList

-- | \(O(n)\). Convert the map to a list of key\/value pairs where the keys are
-- in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: Map k a -> [(k,a)]
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []

-- | \(O(n)\). Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: Map k a -> [(k,a)]
toDescList = foldlWithKey (\xs k x -> (k,x):xs) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are fold{r,l}WithKey equivalents, used for list fusion.
-- They are important to convert unfused methods back, see mapFB in prelude.
foldrFB :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrFB = foldrWithKey
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlFB = foldlWithKey
{-# INLINE[0] foldlFB #-}

-- Inline assocs and toList, so that we need to fuse only toAscList.
{-# INLINE assocs #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded elems,keys,to{Asc,Desc}List calls back to
-- elems,keys,to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were
-- used in a list fusion, otherwise it would go away in phase 1), and let compiler
-- do whatever it wants with elems,keys,to{Asc,Desc}List -- it was forbidden to
-- inline it before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] elems #-}
{-# NOINLINE[0] keys #-}
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "Map.elems" [~1] forall m . elems m = build (\c n -> foldrFB (\_ x xs -> c x xs) n m) #-}
{-# RULES "Map.elemsBack" [1] foldrFB (\_ x xs -> x : xs) [] = elems #-}
{-# RULES "Map.keys" [~1] forall m . keys m = build (\c n -> foldrFB (\k _ xs -> c k xs) n m) #-}
{-# RULES "Map.keysBack" [1] foldrFB (\k _ xs -> k : xs) [] = keys #-}
{-# RULES "Map.toAscList" [~1] forall m . toAscList m = build (\c n -> foldrFB (\k x xs -> c (k,x) xs) n m) #-}
{-# RULES "Map.toAscListBack" [1] foldrFB (\k x xs -> (k, x) : xs) [] = toAscList #-}
{-# RULES "Map.toDescList" [~1] forall m . toDescList m = build (\c n -> foldlFB (\xs k x -> c (k,x) xs) n m) #-}
{-# RULES "Map.toDescListBack" [1] foldlFB (\xs k x -> (k, x) : xs) [] = toDescList #-}
#endif

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | \(O(n)\). Build a map from an ascending list in linear time.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromList' if the
-- precondition may not hold.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: Eq k => [(k,a)] -> Map k a
fromAscList xs = fromAscListWithKey (\_ x _ -> x) xs
{-# INLINE fromAscList #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from a descending list in linear time.
--
-- __Warning__: This function should be used only if the keys are in
-- non-increasing order. This precondition is not checked. Use 'fromList' if the
-- precondition may not hold.
--
-- > fromDescList [(5,"a"), (3,"b")]          == fromList [(3, "b"), (5, "a")]
-- > fromDescList [(5,"a"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromDescList [(5,"a"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescList [(5,"a"), (3,"b"), (5,"b")]) == False
--
-- @since 0.5.8

fromDescList :: Eq k => [(k,a)] -> Map k a
fromDescList xs = fromDescListWithKey (\_ x _ -> x) xs
{-# INLINE fromDescList #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from an ascending list in linear time with a combining function for equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromListWith' if
-- the precondition may not hold.
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False
--
-- See also: 'fromAscListUpsert'

fromAscListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> Map k a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs
{-# INLINE fromAscListWith #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from a descending list in linear time with a combining function for equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-increasing order. This precondition is not checked. Use 'fromListWith' if
-- the precondition may not hold.
--
-- > fromDescListWith (++) [(5,"a"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromDescListWith (++) [(5,"a"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False
--
-- Also see the performance note on 'fromListWith'.
--
-- See also: 'fromDescListUpsert'
--
-- @since 0.5.8

fromDescListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> Map k a
fromDescListWith f xs
  = fromDescListWithKey (\_ x y -> f x y) xs
{-# INLINE fromDescListWith #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from an ascending list in linear time with a
-- combining function for equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromListWithKey'
-- if the precondition may not hold.
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"c")] == fromList [(3, "b"), (5, "5:c5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"c")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"c")]) == False
--
-- Also see the performance note on 'fromListWith'.
--
-- See also: 'fromAscListUpsert'

fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromAscListWithKey f xs = ascLinkAll (Foldable.foldl' next Nada xs)
  where
    next stk (!ky, y) = case stk of
      Push kx x l stk'
        | ky == kx -> Push ky (f ky y x) l stk'
        | Tip <- l -> ascLinkTop stk' 1 (singleton kx x) ky y
        | otherwise -> Push ky y Tip stk
      Nada -> Push ky y Tip stk
{-# INLINE fromAscListWithKey #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from a descending list in linear time with a
-- combining function for equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-increasing order. This precondition is not checked. Use 'fromListWithKey'
-- if the precondition may not hold.
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromDescListWithKey f [(5,"a"), (5,"b"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromDescListWithKey f [(5,"a"), (5,"b"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False
--
-- Also see the performance note on 'fromListWith'.
--
-- See also: 'fromDescListUpsert'

fromDescListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromDescListWithKey f xs = descLinkAll (Foldable.foldl' next Nada xs)
  where
    next stk (!ky, y) = case stk of
      Push kx x r stk'
        | ky == kx -> Push ky (f ky y x) r stk'
        | Tip <- r -> descLinkTop ky y 1 (singleton kx x) stk'
        | otherwise -> Push ky y Tip stk
      Nada -> Push ky y Tip stk
{-# INLINE fromDescListWithKey #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from an ascending list in linear time with a
-- combining function for equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromListUpsert'
-- if the precondition may not hold.
--
-- > let f x = maybe [x] (x:)
-- > fromAscListUpsert f [(3,'a'), (3,'b'), (5,'c'), (5,'d'), (5,'e')] == fromList [(3,"ba"), (5,"edc")]
--
-- @since 0.8.1
fromAscListUpsert :: Eq k => (a -> Maybe b -> b) -> [(k, a)] -> Map k b
fromAscListUpsert f xs = ascLinkAll (Foldable.foldl' next Nada xs)
  where
    next stk (!ky, y) = case stk of
      Push kx x l stk'
        | ky == kx -> Push ky (f y (Just x)) l stk'
        | Tip <- l -> ascLinkTop stk' 1 (singleton kx x) ky (f y Nothing)
        | otherwise -> Push ky (f y Nothing) Tip stk
      Nada -> Push ky (f y Nothing) Tip stk
{-# INLINE fromAscListUpsert #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from a descending list in linear time with a
-- combining function for equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-increasing order. This precondition is not checked. Use 'fromListUpsert'
-- if the precondition may not hold.
--
-- > let f x = maybe [x] (x:)
-- > fromDescListUpsert f [(5,'a'), (5,'b'), (5,'c'), (3,'d'), (3,'e')] == fromList [(3,"ed"), (5,"cba")]
--
-- @since 0.8.1
fromDescListUpsert :: Eq k => (a -> Maybe b -> b) -> [(k, a)] -> Map k b
fromDescListUpsert f xs = descLinkAll (Foldable.foldl' next Nada xs)
  where
    next stk (!ky, y) = case stk of
      Push kx x r stk'
        | ky == kx -> Push ky (f y (Just x)) r stk'
        | Tip <- r -> descLinkTop ky (f y Nothing) 1 (singleton kx x) stk'
        | otherwise -> Push ky (f y Nothing) Tip stk
      Nada -> Push ky (f y Nothing) Tip stk
{-# INLINE fromDescListUpsert #-}  -- INLINE for fusion

-- | \(O(n)\). Build a map from an ascending list of distinct elements in linear time.
--
-- __Warning__: This function should be used only if the keys are in
-- strictly increasing order. This precondition is not checked. Use 'fromList'
-- if the precondition may not hold.
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

-- See Note [fromDistinctAscList implementation] in Data.Set.Internal.
fromDistinctAscList :: [(k,a)] -> Map k a
fromDistinctAscList xs = ascLinkAll (Foldable.foldl' next Nada xs)
  where
    next :: Stack k a -> (k, a) -> Stack k a
    next (Push kx x Tip stk) (!ky, y) = ascLinkTop stk 1 (singleton kx x) ky y
    next stk (!kx, x) = Push kx x Tip stk
{-# INLINE fromDistinctAscList #-}  -- INLINE for fusion

ascLinkTop :: Stack k a -> Int -> Map k a -> k -> a -> Stack k a
ascLinkTop (Push kx x l@(Bin lsz _ _ _ _) stk) !rsz r ky y
  | lsz == rsz = ascLinkTop stk sz (Bin sz kx x l r) ky y
  where
    sz = lsz + rsz + 1
ascLinkTop stk !_ l kx x = Push kx x l stk

ascLinkAll :: Stack k a -> Map k a
ascLinkAll stk = foldl'Stack (\r kx x l -> linkL kx x l r) Tip stk
{-# INLINABLE ascLinkAll #-}

-- | \(O(n)\). Build a map from a descending list of distinct elements in linear time.
--
-- __Warning__: This function should be used only if the keys are in
-- strictly decreasing order. This precondition is not checked. Use 'fromList'
-- if the precondition may not hold.
--
-- > fromDistinctDescList [(5,"a"), (3,"b")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctDescList [(5,"a"), (3,"b")])          == True
-- > valid (fromDistinctDescList [(5,"a"), (5,"b"), (3,"b")]) == False
--
-- @since 0.5.8

-- See Note [fromDistinctAscList implementation] in Data.Set.Internal.
fromDistinctDescList :: [(k,a)] -> Map k a
fromDistinctDescList xs = descLinkAll (Foldable.foldl' next Nada xs)
  where
    next :: Stack k a -> (k, a) -> Stack k a
    next (Push ky y Tip stk) (!kx, x) = descLinkTop kx x 1 (singleton ky y) stk
    next stk (!ky, y) = Push ky y Tip stk
{-# INLINE fromDistinctDescList #-}  -- INLINE for fusion

descLinkTop :: k -> a -> Int -> Map k a -> Stack k a -> Stack k a
descLinkTop kx x !lsz l (Push ky y r@(Bin rsz _ _ _ _) stk)
  | lsz == rsz = descLinkTop kx x sz (Bin sz ky y l r) stk
  where
    sz = lsz + rsz + 1
descLinkTop ky y !_ r stk = Push ky y r stk
{-# INLINABLE descLinkTop #-}

descLinkAll :: Stack k a -> Map k a
descLinkAll stk = foldl'Stack (\l kx x r -> linkR kx x l r) Tip stk
{-# INLINABLE descLinkAll #-}

data Stack k a = Push !k a !(Map k a) !(Stack k a) | Nada

foldl'Stack :: (b -> k -> a -> Map k a -> b) -> b -> Stack k a -> b
foldl'Stack f = go
  where
    go !z Nada = z
    go z (Push kx x t stk) = go (f z kx x t) stk
{-# INLINE foldl'Stack #-}

{-
-- Functions very similar to these were used to implement
-- hedge union, intersection, and difference algorithms that we no
-- longer use. These functions, however, seem likely to be useful
-- in their own right, so I'm leaving them here in case we end up
-- exporting them.

{--------------------------------------------------------------------
  [filterGt b t] filter all keys >[b] from tree [t]
  [filterLt b t] filter all keys <[b] from tree [t]
--------------------------------------------------------------------}
filterGt :: Ord k => k -> Map k v -> Map k v
filterGt !_ Tip = Tip
filterGt !b (Bin _ kx x l r) =
  case compare b kx of LT -> link kx x (filterGt b l) r
                       EQ -> r
                       GT -> filterGt b r
#if __GLASGOW_HASKELL__
{-# INLINABLE filterGt #-}
#endif

filterLt :: Ord k => k -> Map k v -> Map k v
filterLt !_ Tip = Tip
filterLt !b (Bin _ kx x l r) =
  case compare kx b of LT -> link kx x l (filterLt b r)
                       EQ -> l
                       GT -> filterLt b l
#if __GLASGOW_HASKELL__
{-# INLINABLE filterLt #-}
#endif
-}

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | \(O(\log n)\). The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Ord k => k -> Map k a -> (Map k a,Map k a)
split !k0 t0 = toPair $ go k0 t0
  where
    go k t =
      case t of
        Tip            -> Tip :*: Tip
        Bin _ kx x l r -> case compare k kx of
          LT -> let (lt :*: gt) = go k l in lt :*: linkR kx x gt r
          GT -> let (lt :*: gt) = go k r in linkL kx x l lt :*: gt
          EQ -> (l :*: r)
{-# INLINABLE split #-}

-- | \(O(\log n)\). The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Ord k => k -> Map k a -> (Map k a,Maybe a,Map k a)
splitLookup k0 m = case go k0 m of
     TripleS l mv r -> (l, mv, r)
  where
    go :: Ord k => k -> Map k a -> StrictTriple (Map k a) (Maybe a) (Map k a)
    go !k t =
      case t of
        Tip            -> TripleS Tip Nothing Tip
        Bin _ kx x l r -> case compare k kx of
          LT -> let TripleS lt z gt = go k l
                    !gt' = linkR kx x gt r
                in TripleS lt z gt'
          GT -> let TripleS lt z gt = go k r
                    !lt' = linkL kx x l lt
                in TripleS lt' z gt
          EQ -> TripleS l (Just x) r
{-# INLINABLE splitLookup #-}

-- | \(O(\log n)\). A variant of 'splitLookup' that indicates only whether the
-- key was present, rather than producing its value. This is used to
-- implement 'intersection' to avoid allocating unnecessary 'Just'
-- constructors.
splitMember :: Ord k => k -> Map k a -> (Map k a,Bool,Map k a)
splitMember k0 m = case go k0 m of
     TripleS l mv r -> (l, mv, r)
  where
    go :: Ord k => k -> Map k a -> StrictTriple (Map k a) Bool (Map k a)
    go !k t =
      case t of
        Tip            -> TripleS Tip False Tip
        Bin _ kx x l r -> case compare k kx of
          LT -> let TripleS lt z gt = go k l
                    !gt' = linkR kx x gt r
                in TripleS lt z gt'
          GT -> let TripleS lt z gt = go k r
                    !lt' = linkL kx x l lt
                in TripleS lt' z gt
          EQ -> TripleS l True r
{-# INLINABLE splitMember #-}

{--------------------------------------------------------------------
  MapBuilder
--------------------------------------------------------------------}

-- See Note [SetBuilder] in Data.Set.Internal

data MapBuilder k a
  = BAsc !(Stack k a)
  | BMap !(Map k a)

-- Empty builder.
emptyB :: MapBuilder k a
emptyB = BAsc Nada

-- Insert a key and value. Replaces the old value if one already exists for
-- the key.
insertB :: Ord k => k -> a -> MapBuilder k a -> MapBuilder k a
insertB !ky y b = case b of
  BAsc stk -> case stk of
    Push kx x l stk' -> case compare ky kx of
      LT -> BMap (insert ky y (ascLinkAll stk))
      EQ -> BAsc (Push ky y l stk')
      GT -> case l of
        Tip -> BAsc (ascLinkTop stk' 1 (singleton kx x) ky y)
        Bin{} -> BAsc (Push ky y Tip stk)
    Nada -> BAsc (Push ky y Tip Nada)
  BMap m -> BMap (insert ky y m)
{-# INLINE insertB #-}

-- Insert a key and value. The new value is combined with the old value if one
-- already exists for the key.
insertWithB
  :: Ord k => (a -> a -> a) -> k -> a -> MapBuilder k a -> MapBuilder k a
insertWithB f !ky y b = case b of
  BAsc stk -> case stk of
    Push kx x l stk' -> case compare ky kx of
      LT -> BMap (insertWith f ky y (ascLinkAll stk))
      EQ -> BAsc (Push ky (f y x) l stk')
      GT -> case l of
        Tip -> BAsc (ascLinkTop stk' 1 (singleton kx x) ky y)
        Bin{} -> BAsc (Push ky y Tip stk)
    Nada -> BAsc (Push ky y Tip Nada)
  BMap m -> BMap (insertWith f ky y m)
{-# INLINE insertWithB #-}

-- Upsert a key-value. The given function is used to generate the value based
-- on the existing value for the key.
upsertB :: Ord k => (Maybe a -> a) -> k -> MapBuilder k a -> MapBuilder k a
upsertB f !ky b = case b of
  BAsc stk -> case stk of
    Push kx x l stk' -> case compare ky kx of
      LT -> BMap (upsert f ky (ascLinkAll stk))
      EQ -> BAsc (Push ky (f (Just x)) l stk')
      GT -> case l of
        Tip -> BAsc (ascLinkTop stk' 1 (singleton kx x) ky (f Nothing))
        Bin{} -> BAsc (Push ky (f Nothing) Tip stk)
    Nada -> BAsc (Push ky (f Nothing) Tip Nada)
  BMap m -> BMap (upsert f ky m)
{-# INLINE upsertB #-}

-- Finalize the builder into a Map.
finishB :: MapBuilder k a -> Map k a
finishB (BAsc stk) = ascLinkAll stk
finishB (BMap m) = m
{-# INLINABLE finishB #-}

{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [k] and all values
  in [r] > [k], and that [l] and [r] are valid trees.

  In order of sophistication:
    [Bin sz k x l r]  The type constructor.
    [bin k x l r]     Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance k x l r] Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [link k x l r]    Restores balance and size.

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [link2 l r]       Merges two trees and restores balance.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: k -> a -> Map k a -> Map k a -> Map k a
link kx x Tip r  = insertMin kx x r
link kx x l Tip  = insertMax kx x l
link kx x l@(Bin lsz lkx lx ll lr) r@(Bin rsz rkx rx rl rr)
  | delta*lsz < rsz = balanceL rkx rx (linkR_ kx x lsz l rl) rr
  | delta*rsz < lsz = balanceR lkx lx ll (linkL_ kx x lr rsz r)
  | otherwise       = Bin (1+lsz+rsz) kx x l r

-- Variant of link. Restores balance when the left tree may be too large for the
-- right tree, but not the other way around.
linkL :: k -> a -> Map k a -> Map k a -> Map k a
linkL kx x l r = case r of
  Tip -> insertMax kx x l
  Bin rsz _ _ _ _ -> linkL_ kx x l rsz r

linkL_ :: k -> a -> Map k a -> Int -> Map k a -> Map k a
linkL_ kx x l !rsz r = case l of
  Bin lsz lkx lx ll lr
    | delta*rsz < lsz -> balanceR lkx lx ll (linkL_ kx x lr rsz r)
    | otherwise -> Bin (1+lsz+rsz) kx x l r
  Tip -> Bin (1+rsz) kx x Tip r

-- Variant of link. Restores balance when the right tree may be too large for
-- the left tree, but not the other way around.
linkR :: k -> a -> Map k a -> Map k a -> Map k a
linkR kx x l r = case l of
  Tip -> insertMin kx x r
  Bin lsz _ _ _ _ -> linkR_ kx x lsz l r

linkR_ :: k -> a -> Int -> Map k a -> Map k a -> Map k a
linkR_ kx x !lsz l r = case r of
  Bin rsz rkx rx rl rr
    | delta*lsz < rsz -> balanceL rkx rx (linkR_ kx x lsz l rl) rr
    | otherwise -> Bin (1+lsz+rsz) kx x l r
  Tip -> Bin (1+lsz) kx x l Tip

-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: k -> a -> Map k a -> Map k a
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y l (insertMax kx x r)

insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y (insertMin kx x l) r

{--------------------------------------------------------------------
  [link2 l r]: merges two trees.
--------------------------------------------------------------------}
link2 :: Map k a -> Map k a -> Map k a
link2 Tip r   = r
link2 l Tip   = l
link2 l@(Bin lsz lkx lx ll lr) r@(Bin rsz rkx rx rl rr)
  | delta*lsz < rsz = balanceL rkx rx (link2R_ lsz l rl) rr
  | delta*rsz < lsz = balanceR lkx lx ll (link2L_ lr rsz r)
  | otherwise = glue l r

link2L_ :: Map k a -> Int -> Map k a -> Map k a
link2L_ l !rsz r = case l of
  Bin lsz lkx lx ll lr
    | delta*rsz < lsz -> balanceR lkx lx ll (link2L_ lr rsz r)
    | otherwise -> glue l r
  Tip -> r

link2R_ :: Int -> Map k a -> Map k a -> Map k a
link2R_ !lsz l r = case r of
  Bin rsz rkx rx rl rr
    | delta*lsz < rsz -> balanceL rkx rx (link2R_ lsz l rl) rr
    | otherwise -> glue l r
  Tip -> l

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl kl xl ll lr) r@(Bin sr kr xr rl rr)
  | sl > sr = let !(MaxView km m l') = maxViewSure kl xl ll lr in Bin (sl+sr) km m l' r
  | otherwise = let !(MinView km m r') = minViewSure kr xr rl rr in Bin (sl+sr) km m l r'

data MinView k a = MinView !k a !(Map k a)
data MaxView k a = MaxView !k a !(Map k a)

minViewSure :: k -> a -> Map k a -> Map k a -> MinView k a
minViewSure !k x l !r = case l of
  Tip -> MinView k x r
  Bin _ lk lx ll lr -> case minViewSure lk lx ll lr of
    MinView km xm l' -> MinView km xm (balanceR k x l' r)

maxViewSure :: k -> a -> Map k a -> Map k a -> MaxView k a
maxViewSure !k x !l r = case r of
  Tip -> MaxView k x l
  Bin _ rk rx rl rr -> case maxViewSure rk rx rl rr of
    MaxView km xm r' -> MaxView km xm (balanceL k x l r')

-- | \(O(\log n)\). Delete and find the minimal element.
--
-- Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'minViewWithKey'.
deleteFindMin :: Map k a -> ((k,a),Map k a)
deleteFindMin t = case minViewWithKey t of
  Nothing -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)
  Just res -> res

-- | \(O(\log n)\). Delete and find the maximal element.
--
-- Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'maxViewWithKey'.
deleteFindMax :: Map k a -> ((k,a),Map k a)
deleteFindMax t = case maxViewWithKey t of
  Nothing -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)
  Just res -> res

{--------------------------------------------------------------------
  Iterator
--------------------------------------------------------------------}

-- See Note [Iterator] in Data.Set.Internal

iterDown :: Map k a -> Stack k a -> Stack k a
iterDown (Bin _ kx x l r) stk = iterDown l (Push kx x r stk)
iterDown Tip stk = stk

-- Create an iterator from a Map, starting at the smallest key.
iterator :: Map k a -> Stack k a
iterator m = iterDown m Nada

-- Get the next key-value and the remaining iterator.
iterNext :: Stack k a -> Maybe (StrictPair (KeyValue k a) (Stack k a))
iterNext (Push kx x r stk) = Just $! KeyValue kx x :*: iterDown r stk
iterNext Nada = Nothing
{-# INLINE iterNext #-}

-- Whether there are no more key-values in the iterator.
iterNull :: Stack k a -> Bool
iterNull (Push _ _ _ _) = False
iterNull Nada = True

{--------------------------------------------------------------------
  [balance l x r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is corresponds with the inverse
          of $\alpha$ in Adam's article.

  Note that according to the Adam's paper:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.

  But the Adam's paper is erroneous:
  - It can be proved that for delta=2 and delta>=5 there does
    not exist any ratio that would work.
  - Delta=4.5 and ratio=2 does not work.

  That leaves two reasonable variants, delta=3 and delta=4,
  both with ratio=2.

  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  In the benchmarks, delta=3 is faster on insert operations,
  and delta=4 has slightly better deletes. As the insert speedup
  is larger, we currently use delta=3.

--------------------------------------------------------------------}
delta,ratio :: Int
delta = 3
ratio = 2

-- The balance function is equivalent to the following:
--
--   balance :: k -> a -> Map k a -> Map k a -> Map k a
--   balance k x l r
--     | sizeL + sizeR <= 1    = Bin sizeX k x l r
--     | sizeR > delta*sizeL   = rotateL k x l r
--     | sizeL > delta*sizeR   = rotateR k x l r
--     | otherwise             = Bin sizeX k x l r
--     where
--       sizeL = size l
--       sizeR = size r
--       sizeX = sizeL + sizeR + 1
--
--   rotateL :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateL k x l r@(Bin _ _ _ ly ry) | size ly < ratio*size ry = singleL k x l r
--                                     | otherwise               = doubleL k x l r
--
--   rotateR :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateR k x l@(Bin _ _ _ ly ry) r | size ry < ratio*size ly = singleR k x l r
--                                     | otherwise               = doubleR k x l r
--
--   singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
--   singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
--   singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
--
--   doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
--   doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
--   doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
--
-- It is only written in such a way that every node is pattern-matched only once.

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r = case (l, r) of
  (Bin ls _ _ _ _, Bin rs _ _ _ _)
    | rs <= delta*ls && ls <= delta*rs -> Bin (1+ls+rs) k x l r
  _ -> balance_ k x l r
{-# INLINE balance #-} -- See Note [Inlining balance] in Data.Set.Internal

balance_ :: k -> a -> Map k a -> Map k a -> Map k a
balance_ k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls lk lx ll lr) -> case r of
           Tip -> case (ll, lr) of
                    (Tip, Tip) -> Bin 2 k x l Tip
                    (Tip, (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
                    ((Bin _ _ _ _ _), Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
                    ((Bin lls _ _ _ _), (Bin lrs lrk lrx lrl lrr))
                      | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
                      | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)
           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balance"
              | {- ls > delta*rs -} otherwise -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balance"
{-# NOINLINE balance_ #-}

-- Functions balanceL and balanceR are specialized versions of balance.
-- balanceL only checks whether the left subtree is too big,
-- balanceR only checks whether the right subtree is too big.

-- balanceL is called when left subtree might have been inserted to or when
-- right subtree might have been deleted from.
balanceL :: k -> a -> Map k a -> Map k a -> Map k a
balanceL k x l r = case (l, r) of
  (Bin ls _ _ _ _, Bin rs _ _ _ _)
    | ls <= delta*rs -> Bin (1+ls+rs) k x l r
  _ -> balanceL_ k x l r
{-# INLINE balanceL #-} -- See Note [Inlining balance] in Data.Set.Internal

balanceL_ :: k -> a -> Map k a -> Map k a -> Map k a
balanceL_ k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr) -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL_"
{-# NOINLINE balanceL_ #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r = case (l, r) of
  (Bin ls _ _ _ _, Bin rs _ _ _ _)
    | rs <= delta*ls -> Bin (1+ls+rs) k x l r
  _ -> balanceR_ k x l r
{-# INLINE balanceR #-} -- See Note [Inlining balance] in Data.Set.Internal

balanceR_ :: k -> a -> Map k a -> Map k a -> Map k a
balanceR_ k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr) -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR_"
{-# NOINLINE balanceR_ #-}


{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r
  = Bin (size l + size r + 1) k x l r
{-# INLINE bin #-}


{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}

instance (Eq k,Eq a) => Eq (Map k a) where
  m1 == m2 = liftEq2 (==) (==) m1 m2
  {-# INLINABLE (==) #-}

-- | @since 0.5.9
instance Eq k => Eq1 (Map k) where
  liftEq = liftEq2 (==)
  {-# INLINE liftEq #-}

-- | @since 0.5.9
instance Eq2 Map where
  liftEq2 keq eq m1 m2 = size m1 == size m2 && sameSizeLiftEq2 keq eq m1 m2
  {-# INLINE liftEq2 #-}

-- Assumes the maps are of equal size to skip the final check
sameSizeLiftEq2
  :: (ka -> kb -> Bool) -> (a -> b -> Bool) -> Map ka a -> Map kb b -> Bool
sameSizeLiftEq2 keq eq m1 m2 =
  case runEqM (foldMapWithKey f m1) (iterator m2) of e :*: _ -> e
  where
    f kx x = EqM $ \it -> case iterNext it of
      Nothing -> False :*: it
      Just (KeyValue ky y :*: it') -> (keq kx ky && eq x y) :*: it'
{-# INLINE sameSizeLiftEq2 #-}

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = liftCmp2 compare compare m1 m2
  {-# INLINABLE compare #-}

-- | @since 0.5.9
instance Ord k => Ord1 (Map k) where
  liftCompare = liftCmp2 compare
  {-# INLINE liftCompare #-}

-- | @since 0.5.9
instance Ord2 Map where
  liftCompare2 = liftCmp2
  {-# INLINE liftCompare2 #-}

liftCmp2
  :: (ka -> kb -> Ordering)
  -> (a -> b -> Ordering)
  -> Map ka a
  -> Map kb b
  -> Ordering
liftCmp2 kcmp cmp m1 m2 = case runOrdM (foldMapWithKey f m1) (iterator m2) of
  o :*: it -> o <> if iterNull it then EQ else LT
  where
    f kx x = OrdM $ \it -> case iterNext it of
      Nothing -> GT :*: it
      Just (KeyValue ky y :*: it') -> (kcmp kx ky <> cmp x y) :*: it'
{-# INLINE liftCmp2 #-}

{--------------------------------------------------------------------
  Lifted instances
--------------------------------------------------------------------}

-- | @since 0.5.9
instance Show2 Map where
    liftShowsPrec2 spk slk spv slv d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

-- | @since 0.5.9
instance Show k => Show1 (Map k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 0.5.9
instance (Ord k, Read k) => Read1 (Map k) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}
instance Functor (Map k) where
  fmap f m  = map f m
#ifdef __GLASGOW_HASKELL__
  _ <$ Tip = Tip
  a <$ (Bin sx kx _ l r) = Bin sx kx a (a <$ l) (a <$ r)
#endif

-- | Traverses in order of increasing key.
instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ -> f)
  {-# INLINE traverse #-}

-- | Folds in order of increasing key.
instance Foldable.Foldable (Map k) where
  fold = go
    where go Tip = mempty
          go (Bin 1 _ v _ _) = v
          go (Bin _ _ v l r) = go l `mappend` (v `mappend` go r)
  {-# INLINABLE fold #-}
  foldr = foldr
  {-# INLINE foldr #-}
  foldl = foldl
  {-# INLINE foldl #-}
  foldMap f t = go t
    where go Tip = mempty
          go (Bin 1 _ v _ _) = f v
          go (Bin _ _ v l r) = go l `mappend` (f v `mappend` go r)
  {-# INLINE foldMap #-}
  foldl' = foldl'
  {-# INLINE foldl' #-}
  foldr' = foldr'
  {-# INLINE foldr' #-}
  length = size
  {-# INLINE length #-}
  null   = null
  {-# INLINE null #-}
  toList = elems -- NB: Foldable.toList /= Map.toList
  {-# INLINE toList #-}
  elem = go
    where go !_ Tip = False
          go x (Bin _ _ v l r) = x == v || go x l || go x r
  {-# INLINABLE elem #-}
  maximum = start
    where start Tip = error "Data.Foldable.maximum (for Data.Map): empty map"
          start (Bin _ _ v l r) = go (go v l) r

          go !m Tip = m
          go m (Bin _ _ v l r) = go (go (max m v) l) r
  {-# INLINABLE maximum #-}
  minimum = start
    where start Tip = error "Data.Foldable.minimum (for Data.Map): empty map"
          start (Bin _ _ v l r) = go (go v l) r

          go !m Tip = m
          go m (Bin _ _ v l r) = go (go (min m v) l) r
  {-# INLINABLE minimum #-}
  sum = foldl' (+) 0
  {-# INLINABLE sum #-}
  product = foldl' (*) 1
  {-# INLINABLE product #-}

-- | @since 0.6.3.1
instance Bifoldable Map where
  bifold = go
    where go Tip = mempty
          go (Bin 1 k v _ _) = k `mappend` v
          go (Bin _ k v l r) = go l `mappend` (k `mappend` (v `mappend` go r))
  {-# INLINABLE bifold #-}
  bifoldr f g z = go z
    where go z' Tip             = z'
          go z' (Bin _ k v l r) = go (f k (g v (go z' r))) l
  {-# INLINE bifoldr #-}
  bifoldl f g z = go z
    where go z' Tip             = z'
          go z' (Bin _ k v l r) = go (g (f (go z' l) k) v) r
  {-# INLINE bifoldl #-}
  bifoldMap f g t = go t
    where go Tip = mempty
          go (Bin 1 k v _ _) = f k `mappend` g v
          go (Bin _ k v l r) = go l `mappend` (f k `mappend` (g v `mappend` go r))
  {-# INLINE bifoldMap #-}

instance (NFData k, NFData a) => NFData (Map k a) where
    rnf Tip = ()
    rnf (Bin _ kx x l r) = rnf kx `seq` rnf x `seq` rnf l `seq` rnf r

-- | @since 0.8
instance NFData k => NFData1 (Map k) where
  liftRnf rnfx = go
    where
    go Tip              = ()
    go (Bin _ kx x l r) = rnf kx `seq` rnfx x `seq` go l `seq` go r

-- | @since 0.8
instance NFData2 Map where
  liftRnf2 rnfkx rnfx = go
    where
    go Tip              = ()
    go (Bin _ kx x l r) = rnfkx kx `seq` rnfx x `seq` go l `seq` go r

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Ord k, Read k, Read e) => Read (Map k e) where
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | \(O(1)\).  Decompose a map into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the pieces
-- returned will be in ascending order (all elements in the first submap less than all
-- elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d')],fromList [(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than three submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
--
-- @since 0.5.4
splitRoot :: Map k b -> [Map k b]
splitRoot orig =
  case orig of
    Tip           -> []
    Bin _ k v l r -> [l, singleton k v, r]
{-# INLINE splitRoot #-}
