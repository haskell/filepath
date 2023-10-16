-- |
-- Module      :  OsString
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of platform specific short 'OsString', which is:
--
-- 1. on windows wide char bytes (@[Word16]@)
-- 2. on unix char bytes (@[Word8]@)
--
-- It captures the notion of syscall specific encoding (or the lack thereof) to avoid roundtrip issues
-- and memory fragmentation by using unpinned byte arrays. Bytes are not touched or interpreted.
module System.OsString
  (
  -- * String types
    OsString

  -- * OsString construction
  , encodeUtf
  , encodeWith
  , encodeFS
  , osstr
  , empty
  , singleton
  , pack

  -- * OsString deconstruction
  , decodeUtf
  , decodeWith
  , decodeFS
  , unpack

  -- * Word types
  , OsChar

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar

  -- * Basic interface
  , snoc
  , cons
  , last
  , tail
  , uncons
  , head
  , init
  , unsnoc
  , null
  , length

  -- * Transforming OsString
  , map
  , reverse
  , intercalate

  -- * Reducing OsStrings (folds)
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr'
  , foldr1
  , foldr1'

  -- * Special folds
  , all
  , any
  , concat

  -- * Generating and unfolding OsStrings
  , replicate
  , unfoldr
  , unfoldrN

  -- * Substrings
  -- ** Breaking strings
  , take
  , takeEnd
  , takeWhileEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhileEnd
  , dropWhile
  , break
  , breakEnd
  , span
  , spanEnd
  , splitAt
  , split
  , splitWith
  , stripSuffix
  , stripPrefix

  -- * Predicates
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  -- ** Search for arbitrary susbstrings
  , breakSubstring

  -- * Searching OsStrings
  -- ** Searching by equality
  , elem
  , find
  , filter
  , partition

  -- * Indexing OsStrings
  , index
  , indexMaybe
  , (!?)
  , elemIndex
  , elemIndices
  , count
  , findIndex
  , findIndices
  )
where

import System.OsString.Internal
    ( unsafeFromChar
    , toChar
    , encodeUtf
    , encodeWith
    , encodeFS
    , osstr
    , pack
    , empty
    , singleton
    , decodeUtf
    , decodeWith
    , decodeFS
    , unpack
    , snoc
    , cons
    , last
    , tail
    , uncons
    , head
    , init
    , unsnoc
    , null
    , length
    , map
    , reverse
    , intercalate
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr'
    , foldr1
    , foldr1'
    , all
    , any
    , concat
    , replicate
    , unfoldr
    , unfoldrN
    , take
    , takeEnd
    , takeWhileEnd
    , takeWhile
    , drop
    , dropEnd
    , dropWhileEnd
    , dropWhile
    , break
    , breakEnd
    , span
    , spanEnd
    , splitAt
    , split
    , splitWith
    , stripSuffix
    , stripPrefix
    , isInfixOf
    , isPrefixOf
    , isSuffixOf
    , breakSubstring
    , elem
    , find
    , filter
    , partition
    , index
    , indexMaybe
    , (!?)
    , elemIndex
    , elemIndices
    , count
    , findIndex
    , findIndices
    )
import System.OsString.Internal.Types
    ( OsString, OsChar )
import Prelude ()
