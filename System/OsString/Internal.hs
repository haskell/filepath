{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.OsString.Internal where

import System.OsString.Internal.Types

import Control.Monad.Catch
    ( MonadThrow )
import Data.ByteString
    ( ByteString )
import Data.Char
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import System.IO
    ( TextEncoding )

import System.OsPath.Encoding ( EncodingException(..) )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import qualified System.OsString.Windows as PF
#else
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import qualified System.OsString.Posix as PF
#endif
import GHC.Stack (HasCallStack)
import Data.Coerce (coerce)




-- | Partial unicode friendly encoding.
--
-- On windows this encodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this encodes as UTF8 (strictly), which is a good guess.
--
-- Throws a 'EncodingException' if encoding fails.
encodeUtf :: MonadThrow m => String -> m OsString
encodeUtf = fmap OsString . PF.encodeUtf

-- | Encode an 'OsString' given the platform specific encodings.
encodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> String
           -> Either EncodingException OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
encodeWith _ winEnc str = OsString <$> PF.encodeWith winEnc str
#else
encodeWith unixEnc _ str = OsString <$> PF.encodeWith unixEnc str
#endif

-- | Like 'encodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
encodeFS :: String -> IO OsString
encodeFS = fmap OsString . PF.encodeFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
decodeUtf :: MonadThrow m => OsString -> m String
decodeUtf (OsString x) = PF.decodeUtf x

-- | Decode an 'OsString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> OsString
           -> Either EncodingException String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
decodeWith _ winEnc (OsString x) = PF.decodeWith winEnc x
#else
decodeWith unixEnc _ (OsString x) = PF.decodeWith unixEnc x
#endif


-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
decodeFS :: OsString -> IO String
decodeFS (OsString x) = PF.decodeFS x


-- | Constructs an @OsString@ from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
fromBytes :: MonadThrow m
          => ByteString
          -> m OsString
fromBytes = fmap OsString . PF.fromBytes


-- | QuasiQuote an 'OsString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows.
osstr :: QuasiQuoter
osstr =
  QuasiQuoter
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  { quoteExp = \s -> do
      osp <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF16le ErrorOnCodingFailure) $ s
      lift osp
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp = \s -> do
      osp <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF8 ErrorOnCodingFailure) $ s
      lift osp
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#endif


-- | Unpack an 'OsString' to a list of 'OsChar'.
unpack :: OsString -> [OsChar]
unpack = coerce PF.unpack


-- | Pack a list of 'OsChar' to an 'OsString'
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'OsString' is probably not what
-- you want, because it will truncate unicode code points.
pack :: [OsChar] -> OsString
pack = coerce PF.pack

empty :: OsString
empty = mempty

singleton :: OsChar -> OsString
singleton = coerce PF.singleton


-- | Truncates on unix to 1 and on Windows to 2 octets.
unsafeFromChar :: Char -> OsChar
unsafeFromChar = coerce PF.unsafeFromChar

-- | Converts back to a unicode codepoint (total).
toChar :: OsChar -> Char
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toChar (OsChar (WindowsChar w)) = chr $ fromIntegral w
#else
toChar (OsChar (PosixChar w)) = chr $ fromIntegral w
#endif

-- | /O(n)/ Append a byte to the end of a 'OsString'
--
-- @since 1.4.200.0
snoc :: OsString -> OsChar -> OsString
snoc = coerce PF.snoc

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
-- @since 1.4.200.0
cons :: OsChar -> OsString -> OsString
cons = coerce PF.cons

-- | /O(1)/ Extract the last element of a OsString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
-- @since 1.4.200.0
last :: HasCallStack => OsString -> OsChar
last = coerce PF.last

-- | /O(n)/ Extract the elements after the head of a OsString, which must be non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'uncons' instead.
--
-- @since 1.4.200.0
tail :: HasCallStack => OsString -> OsString
tail = coerce PF.tail

-- | /O(n)/ Extract the 'head' and 'tail' of a OsString, returning 'Nothing'
-- if it is empty.
--
-- @since 1.4.200.0
uncons :: OsString -> Maybe (OsChar, OsString)
uncons = coerce PF.uncons

-- | /O(1)/ Extract the first element of a OsString, which must be non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'uncons' instead.
--
-- @since 1.4.200.0
head :: HasCallStack => OsString -> OsChar
head = coerce PF.head

-- | /O(n)/ Return all the elements of a 'OsString' except the last one.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
-- @since 1.4.200.0
init :: HasCallStack => OsString -> OsString
init = coerce PF.init

-- | /O(n)/ Extract the 'init' and 'last' of a OsString, returning 'Nothing'
-- if it is empty.
--
-- @since 1.4.200.0
unsnoc :: OsString -> Maybe (OsString, OsChar)
unsnoc = coerce PF.unsnoc

-- | /O(1)/ Test whether a 'OsString' is empty.
--
-- @since 1.4.200.0
null :: OsString -> Bool
null = coerce PF.null

-- | /O(1)/ The length of a 'OsString'.
--
-- @since 1.4.200.0
length :: OsString -> Int
length = coerce PF.length

-- | /O(n)/ 'map' @f xs@ is the OsString obtained by applying @f@ to each
-- element of @xs@.
--
-- @since 1.4.200.0
map :: (OsChar -> OsChar) -> OsString -> OsString
map = coerce PF.map

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
-- @since 1.4.200.0
reverse :: OsString -> OsString
reverse = coerce PF.reverse

-- | /O(n)/ The 'intercalate' function takes a 'OsString' and a list of
-- 'OsString's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- @since 1.4.200.0
intercalate :: OsString -> [OsString] -> OsString
intercalate = coerce PF.intercalate

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a OsString, reduces the
-- OsString using the binary operator, from left to right.
--
-- @since 1.4.200.0
foldl :: forall a. (a -> OsChar -> a) -> a -> OsString -> a
foldl = coerce (PF.foldl @a)

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
-- @since 1.4.200.0
foldl' :: forall a. (a -> OsChar -> a) -> a -> OsString -> a
foldl' = coerce (PF.foldl' @a)

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'OsString's.
-- An exception will be thrown in the case of an empty OsString.
--
-- @since 1.4.200.0
foldl1 :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldl1 = coerce PF.foldl1

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty OsString.
--
-- @since 1.4.200.0
foldl1' :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldl1' = coerce PF.foldl1'


-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a OsString,
-- reduces the OsString using the binary operator, from right to left.
--
-- @since 1.4.200.0
foldr :: forall a. (OsChar -> a -> a) -> a -> OsString -> a
foldr = coerce (PF.foldr @a)

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
-- @since 1.4.200.0
foldr' :: forall a. (OsChar -> a -> a) -> a -> OsString -> a
foldr' = coerce (PF.foldr' @a)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'OsString's
-- An exception will be thrown in the case of an empty OsString.
--
-- @since 1.4.200.0
foldr1 :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldr1 = coerce PF.foldr1

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
--
-- @since 1.4.200.0
foldr1' :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldr1' = coerce PF.foldr1'

-- | /O(n)/ Applied to a predicate and a 'OsString', 'all' determines
-- if all elements of the 'OsString' satisfy the predicate.
--
-- @since 1.4.200.0
all :: (OsChar -> Bool) -> OsString -> Bool
all = coerce PF.all

-- | /O(n)/ Applied to a predicate and a 'OsString', 'any' determines if
-- any element of the 'OsString' satisfies the predicate.
--
-- @since 1.4.200.0
any :: (OsChar -> Bool) -> OsString -> Bool
any = coerce PF.any

-- /O(n)/ Concatenate a list of OsStrings.
--
-- @since 1.4.200.0
concat :: [OsString] -> OsString
concat = mconcat

-- | /O(n)/ 'replicate' @n x@ is a OsString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- @since 1.4.200.0
replicate :: Int -> OsChar -> OsString
replicate = coerce PF.replicate

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
unfoldr :: forall a. (a -> Maybe (OsChar, a)) -> a -> OsString
unfoldr = coerce (PF.unfoldr @a)

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
unfoldrN :: forall a. Int -> (a -> Maybe (OsChar, a)) -> a -> (OsString, Maybe a)
unfoldrN = coerce (PF.unfoldrN @a)

-- | /O(n)/ 'take' @n@, applied to a OsString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- @since 1.4.200.0
take :: Int -> OsString -> OsString
take = coerce PF.take

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
takeEnd :: Int -> OsString -> OsString
takeEnd = coerce PF.takeEnd

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 1.4.200.0
takeWhileEnd :: (OsChar -> Bool) -> OsString -> OsString
takeWhileEnd = coerce PF.takeWhileEnd

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
--
-- @since 1.4.200.0
takeWhile :: (OsChar -> Bool) -> OsString -> OsString
takeWhile = coerce PF.takeWhile

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or 'empty' if @n > 'length' xs@.
--
-- @since 1.4.200.0
drop :: Int -> OsString -> OsString
drop = coerce PF.drop

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
dropEnd :: Int -> OsString -> OsString
dropEnd = coerce PF.dropEnd

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
--
-- @since 1.4.200.0
dropWhile :: (OsChar -> Bool) -> OsString -> OsString
dropWhile = coerce PF.dropWhile

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 1.4.200.0
dropWhileEnd :: (OsChar -> Bool) -> OsString -> OsString
dropWhileEnd = coerce PF.dropWhileEnd

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
--
-- @since 1.4.200.0
breakEnd :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
breakEnd = coerce PF.breakEnd

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
-- @since 1.4.200.0
break :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
break = coerce PF.break

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
-- @since 1.4.200.0
span :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
span = coerce PF.span

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
spanEnd :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
spanEnd = coerce PF.spanEnd

-- | /O(n)/ 'splitAt' @n sbs@ is equivalent to @('take' n sbs, 'drop' n sbs)@.
--
-- @since 1.4.200.0
splitAt :: Int -> OsString -> (OsString, OsString)
splitAt = coerce PF.splitAt

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
split :: OsChar -> OsString -> [OsString]
split = coerce PF.split

-- | /O(n)/ Splits a 'OsString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
-- @since 1.4.200.0
splitWith :: (OsChar -> Bool) -> OsString -> [OsString]
splitWith = coerce PF.splitWith

-- | /O(n)/ The 'stripSuffix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
--
-- @since 1.4.200.0
stripSuffix :: OsString -> OsString -> Maybe OsString
stripSuffix = coerce PF.stripSuffix

-- | /O(n)/ The 'stripPrefix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 1.4.200.0
stripPrefix :: OsString -> OsString -> Maybe OsString
stripPrefix = coerce PF.stripPrefix


-- | Check whether one string is a substring of another.
--
-- @since 1.4.200.0
isInfixOf :: OsString -> OsString -> Bool
isInfixOf = coerce PF.isInfixOf

-- |/O(n)/ The 'isPrefixOf' function takes two OsStrings and returns 'True'
--
-- @since 1.4.200.0
isPrefixOf :: OsString -> OsString -> Bool
isPrefixOf = coerce PF.isPrefixOf

-- | /O(n)/ The 'isSuffixOf' function takes two OsStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- @since 1.4.200.0
isSuffixOf :: OsString -> OsString -> Bool
isSuffixOf = coerce PF.isSuffixOf

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
breakSubstring :: OsString -> OsString -> (OsString, OsString)
breakSubstring = coerce PF.breakSubstring

-- | /O(n)/ 'elem' is the 'OsString' membership predicate.
--
-- @since 1.4.200.0
elem :: OsChar -> OsString -> Bool
elem = coerce PF.elem

-- | /O(n)/ The 'find' function takes a predicate and a OsString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
-- @since 1.4.200.0
find :: (OsChar -> Bool) -> OsString -> Maybe OsChar
find = coerce PF.find

-- | /O(n)/ 'filter', applied to a predicate and a OsString,
-- returns a OsString containing those characters that satisfy the
-- predicate.
--
-- @since 1.4.200.0
filter :: (OsChar -> Bool) -> OsString -> OsString
filter = coerce PF.filter

-- | /O(n)/ The 'partition' function takes a predicate a OsString and returns
-- the pair of OsStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p sbs, filter (not . p) sbs)
--
-- @since 1.4.200.0
partition :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
partition = coerce PF.partition

-- | /O(1)/ 'OsString' index (subscript) operator, starting from 0.
--
-- @since 1.4.200.0
index :: HasCallStack => OsString -> Int -> OsChar
index = coerce PF.index

-- | /O(1)/ 'OsString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 1.4.200.0
indexMaybe :: OsString -> Int -> Maybe OsChar
indexMaybe = coerce PF.indexMaybe

-- | /O(1)/ 'OsString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 1.4.200.0
(!?) :: OsString -> Int -> Maybe OsChar
(!?) = indexMaybe

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'OsString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
--
-- @since 1.4.200.0
elemIndex :: OsChar -> OsString -> Maybe Int
elemIndex = coerce PF.elemIndex

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
--
-- @since 1.4.200.0
elemIndices :: OsChar -> OsString -> [Int]
elemIndices = coerce PF.elemIndices

-- | count returns the number of times its argument appears in the OsString
--
-- @since 1.4.200.0
count :: OsChar -> OsString -> Int
count = coerce PF.count

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'OsString' and
-- returns the index of the first element in the OsString
-- satisfying the predicate.
--
-- @since 1.4.200.0
findIndex :: (OsChar -> Bool) -> OsString -> Maybe Int
findIndex = coerce PF.findIndex

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
--
-- @since 1.4.200.0
findIndices :: (OsChar -> Bool) -> OsString -> [Int]
findIndices = coerce PF.findIndices

