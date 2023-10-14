{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True
--
#if defined(WINDOWS)
#define WINDOWS_DOC
#else
#define POSIX_DOC
#endif

module System.OsString.MODULE_NAME
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
#else
    PosixString
  , PosixChar
#endif

  -- * String construction
  , encodeUtf
  , encodeWith
  , encodeFS
  , fromBytes
  , pstr
  , singleton
  , empty
  , pack

  -- * String deconstruction
  , decodeUtf
  , decodeWith
  , decodeFS
  , unpack

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

  -- ** Special folds
  , all
  , any
  , concat

  -- ** Generating and unfolding OsStrings
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



import System.OsString.Internal.Types (
#ifdef WINDOWS
  WindowsString(..), WindowsChar(..)
#else
  PosixString(..), PosixChar(..)
#endif
  )

import Data.Char
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.ByteString.Internal
    ( ByteString )
import Control.Exception
    ( SomeException, try, displayException )
import Control.DeepSeq ( force )
import Data.Bifunctor ( first )
import GHC.IO
    ( evaluate, unsafePerformIO )
import qualified GHC.Foreign as GHC
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )


import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#ifdef WINDOWS
import System.OsPath.Encoding
import System.IO
    ( TextEncoding, utf16le )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16
import qualified System.OsPath.Data.ByteString.Short as BS8
#else
import System.OsPath.Encoding
import System.IO
    ( TextEncoding, utf8 )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import qualified System.OsPath.Data.ByteString.Short as BS
#endif
import GHC.Stack (HasCallStack)
import Prelude hiding (last, tail, head, init, null, length, map, reverse, foldl, foldr, foldl1, foldr1, all, any, concat, replicate, take, takeWhile, drop, dropWhile, break, span, splitAt, elem, filter)
import Data.Bifunctor ( bimap )



#ifdef WINDOWS_DOC
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF16-LE (strictly), which is a pretty good guess.
--
-- Throws an 'EncodingException' if encoding fails.
#else
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF8 (strictly), which is a good guess.
--
-- Throws an 'EncodingException' if encoding fails.
#endif
encodeUtf :: MonadThrow m => String -> m PLATFORM_STRING
#ifdef WINDOWS
encodeUtf = either throwM pure . encodeWith utf16le
#else
encodeUtf = either throwM pure . encodeWith utf8
#endif

-- | Encode a 'String' with the specified encoding.
encodeWith :: TextEncoding
           -> String
           -> Either EncodingException PLATFORM_STRING
encodeWith enc str = unsafePerformIO $ do
#ifdef WINDOWS
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> WindowsString <$> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> PosixString <$> BS.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif

#ifdef WINDOWS_DOC
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. This is safe to 'unsafePerformIO'/'unsafeDupablePerformIO'.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
encodeFS :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
encodeFS = fmap WindowsString . encodeWithBaseWindows
#else
encodeFS = fmap PosixString . encodeWithBasePosix
#endif


#ifdef WINDOWS_DOC
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF16-LE (strictly), which is a pretty good.
--
-- Throws a 'EncodingException' if decoding fails.
#else
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
#endif
decodeUtf :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
decodeUtf = either throwM pure . decodeWith utf16le
#else
decodeUtf = either throwM pure . decodeWith utf8
#endif

#ifdef WINDOWS
-- | Decode a 'WindowsString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding
           -> PLATFORM_STRING
           -> Either EncodingException String
decodeWith winEnc (WindowsString ba) = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen winEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
-- | Decode a 'PosixString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding
       -> PLATFORM_STRING
       -> Either EncodingException String
decodeWith unixEnc (PosixString ba) = unsafePerformIO $ do
  r <- try @SomeException $ BS.useAsCStringLen ba $ \fp -> GHC.peekCStringLen unixEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif


#ifdef WINDOWS_DOC
-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. 'unsafePerformIO'/'unsafeDupablePerformIO' are safe, however.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
decodeFS :: PLATFORM_STRING -> IO String
#ifdef WINDOWS
decodeFS (WindowsString ba) = decodeWithBaseWindows ba
#else
decodeFS (PosixString ba) = decodeWithBasePosix ba
#endif


#ifdef WINDOWS_DOC
-- | Constructs a platform string from a ByteString.
--
-- This ensures valid UCS-2LE.
-- Note that this doesn't expand Word8 to Word16 on windows, so you may get invalid UTF-16.
--
-- Throws 'EncodingException' on invalid UCS-2LE (although unlikely).
#else
-- | Constructs a platform string from a ByteString.
--
-- This is a no-op.
#endif
fromBytes :: MonadThrow m
          => ByteString
          -> m PLATFORM_STRING
#ifdef WINDOWS
fromBytes bs =
  let ws = WindowsString . BS16.toShort $ bs
  in either throwM (const . pure $ ws) $ decodeWith ucs2le ws
#else
fromBytes = pure . PosixString . BS.toShort
#endif


#ifdef WINDOWS_DOC
-- | QuasiQuote a 'WindowsString'. This accepts Unicode characters
-- and encodes as UTF-16LE on windows.
#else
-- | QuasiQuote a 'PosixString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix.
#endif
pstr :: QuasiQuoter
pstr =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF16le ErrorOnCodingFailure) s
      lift ps
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF8 ErrorOnCodingFailure) s
      lift ps
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#endif


-- | Unpack a platform string to a list of platform words.
unpack :: PLATFORM_STRING -> [PLATFORM_WORD]
#ifdef WINDOWS
unpack (WindowsString ba) = WindowsChar <$> BS16.unpack ba
#else
unpack (PosixString ba) = PosixChar <$> BS.unpack ba
#endif


-- | Pack a list of platform words to a platform string.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to platform string is probably not what
-- you want, because it will truncate unicode code points.
pack :: [PLATFORM_WORD] -> PLATFORM_STRING
#ifdef WINDOWS
pack = WindowsString . BS16.pack . fmap (\(WindowsChar w) -> w)
#else
pack = PosixString . BS.pack . fmap (\(PosixChar w) -> w)
#endif

singleton :: PLATFORM_WORD -> PLATFORM_STRING
#ifdef WINDOWS
singleton = WindowsString . BS16.singleton . getWindowsChar
#else
singleton = PosixString . BS.singleton . getPosixChar
#endif

empty :: PLATFORM_STRING
empty = mempty


#ifdef WINDOWS
-- | Truncates to 2 octets.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = WindowsChar . fromIntegral . fromEnum
#else
-- | Truncates to 1 octet.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = PosixChar . fromIntegral . fromEnum
#endif

-- | Converts back to a unicode codepoint (total).
toChar :: PLATFORM_WORD -> Char
#ifdef WINDOWS
toChar (WindowsChar w) = chr $ fromIntegral w
#else
toChar (PosixChar w) = chr $ fromIntegral w
#endif

-- | /O(n)/ Append a byte to the end of a 'OsString'
--
-- @since 1.4.200.0
snoc :: PLATFORM_STRING -> PLATFORM_WORD -> PLATFORM_STRING
#ifdef WINDOWS
snoc (WindowsString s) (WindowsChar w) = WindowsString (BS16.snoc s w)
#else
snoc (PosixString s) (PosixChar w) = PosixString (BS.snoc s w)
#endif

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
-- @since 1.4.200.0
cons :: PLATFORM_WORD -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
cons (WindowsChar w) (WindowsString s) = WindowsString (BS16.cons w s)
#else
cons (PosixChar w) (PosixString s) = PosixString (BS.cons w s)
#endif


-- | /O(1)/ Extract the last element of a OsString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
-- @since 1.4.200.0
last :: HasCallStack => PLATFORM_STRING -> PLATFORM_WORD
#ifdef WINDOWS
last (WindowsString s) = WindowsChar (BS16.last s)
#else
last (PosixString s) = PosixChar (BS.last s)
#endif

-- | /O(n)/ Extract the elements after the head of a OsString, which must be non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'uncons' instead.
--
-- @since 1.4.200.0
tail :: HasCallStack => PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
tail (WindowsString s) = WindowsString (BS16.tail s)
#else
tail (PosixString s) = PosixString (BS.tail s)
#endif

-- | /O(n)/ Extract the 'head' and 'tail' of a OsString, returning 'Nothing'
-- if it is empty.
--
-- @since 1.4.200.0
uncons :: PLATFORM_STRING -> Maybe (PLATFORM_WORD, PLATFORM_STRING)
#ifdef WINDOWS
uncons (WindowsString s) = (bimap WindowsChar WindowsString) <$> (BS16.uncons s)
#else
uncons (PosixString s) = (bimap PosixChar PosixString) <$> (BS.uncons s)
#endif

-- | /O(1)/ Extract the first element of a OsString, which must be non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'uncons' instead.
--
-- @since 1.4.200.0
head :: HasCallStack => PLATFORM_STRING -> PLATFORM_WORD
#ifdef WINDOWS
head (WindowsString s) = WindowsChar (BS16.head s)
#else
head (PosixString s) = PosixChar (BS.head s)
#endif

-- | /O(n)/ Return all the elements of a 'OsString' except the last one.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
-- @since 1.4.200.0
init :: HasCallStack => PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
init (WindowsString s) = WindowsString (BS16.init s)
#else
init (PosixString s) = PosixString (BS.init s)
#endif

-- | /O(n)/ Extract the 'init' and 'last' of a OsString, returning 'Nothing'
-- if it is empty.
--
-- @since 1.4.200.0
unsnoc :: PLATFORM_STRING -> Maybe (PLATFORM_STRING, PLATFORM_WORD)
#ifdef WINDOWS
unsnoc (WindowsString s) = (bimap WindowsString WindowsChar) <$> (BS16.unsnoc s)
#else
unsnoc (PosixString s) = (bimap PosixString PosixChar) <$> (BS.unsnoc s)
#endif

-- | /O(1)/. The empty 'OsString'.
--
-- @since 1.4.200.0
null :: PLATFORM_STRING -> Bool
#ifdef WINDOWS
null (WindowsString s) = BS16.null s
#else
null (PosixString s) = BS.null s
#endif

-- | /O(1)/ The length of a 'OsString'.
--
-- @since 1.4.200.0
length :: PLATFORM_STRING -> Int
#ifdef WINDOWS
length (WindowsString s) = BS16.length s
#else
length (PosixString s) = BS.length s
#endif

-- | /O(n)/ 'map' @f xs@ is the OsString obtained by applying @f@ to each
-- element of @xs@.
--
-- @since 1.4.200.0
map :: (PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
map f (WindowsString s) = WindowsString (BS16.map (getWindowsChar . f . WindowsChar) s)
#else
map f (PosixString s) = PosixString (BS.map (getPosixChar . f . PosixChar) s)
#endif

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
-- @since 1.4.200.0
reverse :: PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
reverse (WindowsString s) = WindowsString (BS16.reverse s)
#else
reverse (PosixString s) = PosixString (BS.reverse s)
#endif

-- | /O(n)/ The 'intercalate' function takes a 'OsString' and a list of
-- 'OsString's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- @since 1.4.200.0
intercalate :: PLATFORM_STRING -> [PLATFORM_STRING] -> PLATFORM_STRING
#ifdef WINDOWS
intercalate (WindowsString s) xs = WindowsString (BS16.intercalate s (fmap getWindowsString xs))
#else
intercalate (PosixString s) xs = PosixString (BS.intercalate s (fmap getPosixString xs))
#endif

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a OsString, reduces the
-- OsString using the binary operator, from left to right.
--
-- @since 1.4.200.0
foldl :: (a -> PLATFORM_WORD -> a) -> a -> PLATFORM_STRING -> a
#ifdef WINDOWS
foldl f a (WindowsString s) = BS16.foldl (\a' c -> f a' (WindowsChar c)) a s
#else
foldl f a (PosixString s) = BS.foldl (\a' c -> f a' (PosixChar c)) a s
#endif

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
-- @since 1.4.200.0
foldl'
  :: (a -> PLATFORM_WORD -> a) -> a -> PLATFORM_STRING -> a
#ifdef WINDOWS
foldl' f a (WindowsString s) = BS16.foldl' (\a' c -> f a' (WindowsChar c)) a s
#else
foldl' f a (PosixString s) = BS.foldl' (\a' c -> f a' (PosixChar c)) a s
#endif

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'OsString's.
-- An exception will be thrown in the case of an empty OsString.
--
-- @since 1.4.200.0
foldl1 :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
#ifdef WINDOWS
foldl1 f (WindowsString s) = WindowsChar $ BS16.foldl1 (\a' c -> getWindowsChar $ f (WindowsChar a') (WindowsChar c)) s
#else
foldl1 f (PosixString s) = PosixChar $ BS.foldl1 (\a' c -> getPosixChar $ f (PosixChar a') (PosixChar c)) s
#endif

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty OsString.
--
-- @since 1.4.200.0
foldl1'
  :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
#ifdef WINDOWS
foldl1' f (WindowsString s) = WindowsChar $ BS16.foldl1' (\a' c -> getWindowsChar $ f (WindowsChar a') (WindowsChar c)) s
#else
foldl1' f (PosixString s) = PosixChar $ BS.foldl1' (\a' c -> getPosixChar $ f (PosixChar a') (PosixChar c)) s
#endif

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a OsString,
-- reduces the OsString using the binary operator, from right to left.
--
-- @since 1.4.200.0
foldr :: (PLATFORM_WORD -> a -> a) -> a -> PLATFORM_STRING -> a
#ifdef WINDOWS
foldr f a (WindowsString s) = BS16.foldr (\c a' -> f (WindowsChar c) a') a s
#else
foldr f a (PosixString s) = BS.foldr (\c a' -> f (PosixChar c) a') a s
#endif

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
-- @since 1.4.200.0
foldr'
  :: (PLATFORM_WORD -> a -> a) -> a -> PLATFORM_STRING -> a
#ifdef WINDOWS
foldr' f a (WindowsString s) = BS16.foldr' (\c a' -> f (WindowsChar c) a') a s
#else
foldr' f a (PosixString s) = BS.foldr' (\c a' -> f (PosixChar c) a') a s
#endif

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'OsString's
-- An exception will be thrown in the case of an empty OsString.
--
-- @since 1.4.200.0
foldr1 :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
#ifdef WINDOWS
foldr1 f (WindowsString s) = WindowsChar $ BS16.foldr1 (\c a' -> getWindowsChar $ f (WindowsChar c) (WindowsChar a')) s
#else
foldr1 f (PosixString s) = PosixChar $ BS.foldr1 (\c a' -> getPosixChar $ f (PosixChar c) (PosixChar a')) s
#endif

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
--
--
-- @since 1.4.200.0
-- @since 1.4.200.0
foldr1'
  :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
#ifdef WINDOWS
foldr1' f (WindowsString s) = WindowsChar $ BS16.foldr1' (\c a' -> getWindowsChar $ f (WindowsChar c) (WindowsChar a')) s
#else
foldr1' f (PosixString s) = PosixChar $ BS.foldr1' (\c a' -> getPosixChar $ f (PosixChar c) (PosixChar a')) s
#endif

-- | /O(n)/ Applied to a predicate and a 'OsString', 'all' determines
-- if all elements of the 'OsString' satisfy the predicate.
--
-- @since 1.4.200.0
all :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Bool
#ifdef WINDOWS
all f (WindowsString s) = BS16.all (f . WindowsChar) s
#else
all f (PosixString s) = BS.all (f . PosixChar) s
#endif

-- | /O(n)/ Applied to a predicate and a 'OsString', 'any' determines if
-- any element of the 'OsString' satisfies the predicate.
--
-- @since 1.4.200.0
any :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Bool
#ifdef WINDOWS
any f (WindowsString s) = BS16.any (f . WindowsChar) s
#else
any f (PosixString s) = BS.any (f . PosixChar) s
#endif

-- /O(n)/ Concatenate a list of OsStrings.
--
-- @since 1.4.200.0
concat :: [PLATFORM_STRING] -> PLATFORM_STRING
concat = mconcat

-- | /O(n)/ 'replicate' @n x@ is a OsString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- @since 1.4.200.0
replicate :: Int -> PLATFORM_WORD -> PLATFORM_STRING
#ifdef WINDOWS
replicate i (WindowsChar w) = WindowsString $ BS16.replicate i w
#else
replicate i (PosixChar w) = PosixString $ BS.replicate i w
#endif

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- OsString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the OsString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- This function is not efficient/safe. It will build a list of @[Word8]@
-- and run the generator until it returns `Nothing`, otherwise recurse infinitely,
-- then finally create a 'OsString'.
--
-- If you know the maximum length, consider using 'unfoldrN'.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
-- @since 1.4.200.0
unfoldr :: (a -> Maybe (PLATFORM_WORD, a)) -> a -> PLATFORM_STRING
#ifdef WINDOWS
unfoldr f a = WindowsString $ BS16.unfoldr (fmap (first getWindowsChar) . f) a
#else
unfoldr f a = PosixString $ BS.unfoldr (fmap (first getPosixChar) . f) a
#endif

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a OsString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
-- @since 1.4.200.0
unfoldrN :: forall a. Int -> (a -> Maybe (PLATFORM_WORD, a)) -> a -> (PLATFORM_STRING, Maybe a)
#ifdef WINDOWS
unfoldrN n f a = first WindowsString $ BS16.unfoldrN n (fmap (first getWindowsChar) . f) a
#else
unfoldrN n f a = first PosixString $ BS.unfoldrN n (fmap (first getPosixChar) . f) a
#endif

-- | /O(n)/ 'take' @n@, applied to a OsString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- @since 1.4.200.0
take :: Int -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
take n (WindowsString s) = WindowsString $ BS16.take n s
#else
take n (PosixString s) = PosixString $ BS.take n s
#endif

-- | /O(n)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
-- @since 1.4.200.0
takeEnd :: Int -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
takeEnd n (WindowsString s) = WindowsString $ BS16.takeEnd n s
#else
takeEnd n (PosixString s) = PosixString $ BS.takeEnd n s
#endif

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 1.4.200.0
takeWhileEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
takeWhileEnd f (WindowsString s) = WindowsString $ BS16.takeWhileEnd (f . WindowsChar) s
#else
takeWhileEnd f (PosixString s) = PosixString $ BS.takeWhileEnd (f . PosixChar) s
#endif

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
--
-- @since 1.4.200.0
takeWhile :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
takeWhile f (WindowsString s) = WindowsString $ BS16.takeWhile (f . WindowsChar) s
#else
takeWhile f (PosixString s) = PosixString $ BS.takeWhile (f . PosixChar) s
#endif

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or 'empty' if @n > 'length' xs@.
--
-- @since 1.4.200.0
drop :: Int -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
drop n (WindowsString s) = WindowsString $ BS16.drop n s
#else
drop n (PosixString s) = PosixString $ BS.drop n s
#endif

-- | /O(n)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
--
-- @since 1.4.200.0
dropEnd :: Int -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
dropEnd n (WindowsString s) = WindowsString $ BS16.dropEnd n s
#else
dropEnd n (PosixString s) = PosixString $ BS.dropEnd n s
#endif

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
--
-- @since 1.4.200.0
dropWhile :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
dropWhile f (WindowsString s) = WindowsString $ BS16.dropWhile (f . WindowsChar) s
#else
dropWhile f (PosixString s) = PosixString $ BS.dropWhile (f . PosixChar) s
#endif

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 1.4.200.0
dropWhileEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
dropWhileEnd f (WindowsString s) = WindowsString $ BS16.dropWhileEnd (f . WindowsChar) s
#else
dropWhileEnd f (PosixString s) = PosixString $ BS.dropWhileEnd (f . PosixChar) s
#endif

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
--
-- @since 1.4.200.0
breakEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
breakEnd f (WindowsString s) = bimap WindowsString WindowsString $ BS16.breakEnd (f . WindowsChar) s
#else
breakEnd f (PosixString s) = bimap PosixString PosixString $ BS.breakEnd (f . PosixChar) s
#endif

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
-- @since 1.4.200.0
break :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
break f (WindowsString s) = bimap WindowsString WindowsString $ BS16.break (f . WindowsChar) s
#else
break f (PosixString s) = bimap PosixString PosixString $ BS.break (f . PosixChar) s
#endif

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
-- @since 1.4.200.0
span :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
span f (WindowsString s) = bimap WindowsString WindowsString $ BS16.span (f . WindowsChar) s
#else
span f (PosixString s) = bimap PosixString PosixString $ BS.span (f . PosixChar) s
#endif

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('takeWhileEnd' p &&& 'dropWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) sbs
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse sbs) in (reverse y, reverse x)
--
-- @since 1.4.200.0
spanEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
spanEnd f (WindowsString s) = bimap WindowsString WindowsString $ BS16.spanEnd (f . WindowsChar) s
#else
spanEnd f (PosixString s) = bimap PosixString PosixString $ BS.spanEnd (f . PosixChar) s
#endif

-- | /O(n)/ 'splitAt' @n sbs@ is equivalent to @('take' n sbs, 'drop' n sbs)@.
--
-- @since 1.4.200.0
splitAt :: Int -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
splitAt n (WindowsString s) = bimap WindowsString WindowsString $ BS16.splitAt n s
#else
splitAt n (PosixString s) = bimap PosixString PosixString $ BS.splitAt n s
#endif

-- | /O(n)/ Break a 'OsString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- @since 1.4.200.0
split :: PLATFORM_WORD -> PLATFORM_STRING -> [PLATFORM_STRING]
#ifdef WINDOWS
split (WindowsChar w) (WindowsString s) = WindowsString <$> BS16.split w s
#else
split (PosixChar w) (PosixString s) = PosixString <$> BS.split w s
#endif

-- | /O(n)/ Splits a 'OsString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
-- @since 1.4.200.0
splitWith :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> [PLATFORM_STRING]
#ifdef WINDOWS
splitWith f (WindowsString s) = WindowsString <$> BS16.splitWith (f . WindowsChar) s
#else
splitWith f (PosixString s) = PosixString <$> BS.splitWith (f . PosixChar) s
#endif

-- | /O(n)/ The 'stripSuffix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
--
-- @since 1.4.200.0
stripSuffix :: PLATFORM_STRING -> PLATFORM_STRING -> Maybe PLATFORM_STRING
#ifdef WINDOWS
stripSuffix (WindowsString a) (WindowsString b) = WindowsString <$> BS16.stripSuffix a b
#else
stripSuffix (PosixString a) (PosixString b) = PosixString <$> BS.stripSuffix a b
#endif

-- | /O(n)/ The 'stripPrefix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 1.4.200.0
stripPrefix :: PLATFORM_STRING -> PLATFORM_STRING -> Maybe PLATFORM_STRING
#ifdef WINDOWS
stripPrefix (WindowsString a) (WindowsString b) = WindowsString <$> BS16.stripPrefix a b
#else
stripPrefix (PosixString a) (PosixString b) = PosixString <$> BS.stripPrefix a b
#endif


-- | Check whether one string is a substring of another.
--
-- @since 1.4.200.0
isInfixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
#ifdef WINDOWS
isInfixOf (WindowsString a) (WindowsString b) = BS16.isInfixOf a b
#else
isInfixOf (PosixString a) (PosixString b) = BS.isInfixOf a b
#endif

-- |/O(n)/ The 'isPrefixOf' function takes two OsStrings and returns 'True'
--
-- @since 1.4.200.0
isPrefixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
#ifdef WINDOWS
isPrefixOf (WindowsString a) (WindowsString b) = BS16.isPrefixOf a b
#else
isPrefixOf (PosixString a) (PosixString b) = BS.isPrefixOf a b
#endif

-- | /O(n)/ The 'isSuffixOf' function takes two OsStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- @since 1.4.200.0
isSuffixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
#ifdef WINDOWS
isSuffixOf (WindowsString a) (WindowsString b) = BS16.isSuffixOf a b
#else
isSuffixOf (PosixString a) (PosixString b) = BS.isSuffixOf a b
#endif


-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurrence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
--
-- @since 1.4.200.0
breakSubstring :: PLATFORM_STRING -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
breakSubstring (WindowsString a) (WindowsString b) = bimap WindowsString WindowsString $ BS16.breakSubstring a b
#else
breakSubstring (PosixString a) (PosixString b) = bimap PosixString PosixString $ BS.breakSubstring a b
#endif

-- | /O(n)/ 'elem' is the 'OsString' membership predicate.
--
-- @since 1.4.200.0
elem :: PLATFORM_WORD -> PLATFORM_STRING -> Bool
#ifdef WINDOWS
elem (WindowsChar w) (WindowsString s) = BS16.elem w s
#else
elem (PosixChar w) (PosixString s) = BS.elem w s
#endif

-- | /O(n)/ The 'find' function takes a predicate and a OsString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
-- @since 1.4.200.0
find :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Maybe PLATFORM_WORD
#ifdef WINDOWS
find f (WindowsString s) = WindowsChar <$> BS16.find (f . WindowsChar) s
#else
find f (PosixString s) = PosixChar <$> BS.find (f . PosixChar) s
#endif

-- | /O(n)/ 'filter', applied to a predicate and a OsString,
-- returns a OsString containing those characters that satisfy the
-- predicate.
--
-- @since 1.4.200.0
filter :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
#ifdef WINDOWS
filter f (WindowsString s) = WindowsString $ BS16.filter (f . WindowsChar) s
#else
filter f (PosixString s) = PosixString $ BS.filter (f . PosixChar) s
#endif

-- | /O(n)/ The 'partition' function takes a predicate a OsString and returns
-- the pair of OsStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p sbs, filter (not . p) sbs)
--
-- @since 1.4.200.0
partition :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
#ifdef WINDOWS
partition f (WindowsString s) = bimap WindowsString WindowsString $ BS16.partition (f . WindowsChar) s
#else
partition f (PosixString s) = bimap PosixString PosixString $ BS.partition (f . PosixChar) s
#endif

-- | /O(1)/ 'OsString' index (subscript) operator, starting from 0.
--
-- @since 1.4.200.0
index :: HasCallStack => PLATFORM_STRING -> Int -> PLATFORM_WORD
#ifdef WINDOWS
index (WindowsString s) n = WindowsChar $ BS16.index s n
#else
index (PosixString s) n = PosixChar $ BS.index s n
#endif

-- | /O(1)/ 'OsString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 1.4.200.0
indexMaybe :: PLATFORM_STRING -> Int -> Maybe PLATFORM_WORD
#ifdef WINDOWS
indexMaybe (WindowsString s) n = WindowsChar <$> BS16.indexMaybe s n
#else
indexMaybe (PosixString s) n = PosixChar <$> BS.indexMaybe s n
#endif

-- | /O(1)/ 'OsString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 1.4.200.0
(!?) :: PLATFORM_STRING -> Int -> Maybe PLATFORM_WORD
(!?) = indexMaybe

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'OsString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
--
-- @since 1.4.200.0
elemIndex :: PLATFORM_WORD -> PLATFORM_STRING -> Maybe Int
#ifdef WINDOWS
elemIndex (WindowsChar w) (WindowsString s) = BS16.elemIndex w s
#else
elemIndex (PosixChar w) (PosixString s) = BS.elemIndex w s
#endif

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
--
-- @since 1.4.200.0
elemIndices :: PLATFORM_WORD -> PLATFORM_STRING -> [Int]
#ifdef WINDOWS
elemIndices (WindowsChar w) (WindowsString s) = BS16.elemIndices w s
#else
elemIndices (PosixChar w) (PosixString s) = BS.elemIndices w s
#endif

-- | count returns the number of times its argument appears in the OsString
--
-- @since 1.4.200.0
count :: PLATFORM_WORD -> PLATFORM_STRING -> Int
#ifdef WINDOWS
count (WindowsChar w) (WindowsString s) = BS16.count w s
#else
count (PosixChar w) (PosixString s) = BS.count w s
#endif

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'OsString' and
-- returns the index of the first element in the OsString
-- satisfying the predicate.
--
-- @since 1.4.200.0
findIndex :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Maybe Int
#ifdef WINDOWS
findIndex f (WindowsString s) = BS16.findIndex (f . WindowsChar) s
#else
findIndex f (PosixString s) = BS.findIndex (f . PosixChar) s
#endif

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
--
-- @since 1.4.200.0
findIndices :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> [Int]
#ifdef WINDOWS
findIndices f (WindowsString s) = BS16.findIndices (f . WindowsChar) s
#else
findIndices f (PosixString s) = BS.findIndices (f . PosixChar) s
#endif
