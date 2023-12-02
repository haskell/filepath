-- |
-- Module      :  System.OsPath.Data.ByteString.Short.Word16
-- Copyright   :  © 2022 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- ShortByteStrings encoded as UTF16-LE, suitable for windows FFI calls.
--
-- Word16s are *always* in BE encoding (both input and output), so e.g. 'pack'
-- takes a list of BE encoded @[Word16]@ and produces a UTF16-LE encoded ShortByteString.
--
-- Likewise, 'unpack' takes a UTF16-LE encoded ShortByteString and produces a list of BE encoded @[Word16]@.
--
-- Indices and lengths are always in respect to Word16, not Word8.
--
-- All functions will error out if the input string is not a valid UTF16 stream (uneven number of bytes).
-- So use this module with caution.
module System.OsPath.Data.ByteString.Short.Word16 {-# DEPRECATED "Use System.OsString.Data.ByteString.Short.Word16 from os-string >= 2.0.0 package instead. This module will be removed in filepath >= 1.5." #-} (
    -- * The @ShortByteString@ type and representation
    ShortByteString(..),

    -- * Introducing and eliminating 'ShortByteString's
    empty,
    singleton,
    pack,
    unpack,
    fromShort,
    toShort,

    -- * Basic interface
    snoc,
    cons,
    append,
    last,
    tail,
    uncons,
    uncons2,
    head,
    init,
    unsnoc,
    null,
    length,
    numWord16,

    -- * Transforming ShortByteStrings
    map,
    reverse,
    intercalate,

    -- * Reducing 'ShortByteString's (folds)
    foldl,
    foldl',
    foldl1,
    foldl1',

    foldr,
    foldr',
    foldr1,
    foldr1',

    -- ** Special folds
    all,
    any,
    concat,

    -- ** Generating and unfolding ByteStrings
    replicate,
    unfoldr,
    unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    takeWhileEnd,
    takeWhile,
    drop,
    dropEnd,
    dropWhile,
    dropWhileEnd,
    breakEnd,
    break,
    span,
    spanEnd,
    splitAt,
    split,
    splitWith,
    stripSuffix,
    stripPrefix,

    -- * Predicates
    isInfixOf,
    isPrefixOf,
    isSuffixOf,

    -- ** Search for arbitrary substrings
    breakSubstring,

    -- * Searching ShortByteStrings

    -- ** Searching by equality
    elem,

    -- ** Searching with a predicate
    find,
    filter,
    partition,

    -- * Indexing ShortByteStrings
    index,
    indexMaybe,
    (!?),
    elemIndex,
    elemIndices,
    count,
    findIndex,
    findIndices,

    -- ** Encoding validation
    -- isValidUtf8,

    -- * Low level conversions
    -- ** Packing 'CString's and pointers
    packCWString,
    packCWStringLen,
    newCWString,
   
    -- ** Using ShortByteStrings as 'CString's
    useAsCWString,
    useAsCWStringLen
  )
where

import Prelude hiding
    ( Foldable(..)
    , all
    , any
    , reverse
    , break
    , concat
    , drop
    , dropWhile
    , filter
    , head
    , init
    , last
    , map
    , replicate
    , span
    , splitAt
    , tail
    , take
    , takeWhile
    )
import System.OsPath.Data.ByteString.Short.Word16.Hidden