{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternGuards #-}

-- |
-- Module      :  System.FilePath.Internal.Parser
-- Copyright   :  (c) Julian Ospald 2021
-- License     :  BSD3
--
-- Maintainer  :  hasufell@posteo.de
-- Stability   :  stable
-- Portability :  portable
--
module System.FilePath.Internal.Parser where

import Data.Foldable (asum)
import Control.Applicative ((<|>), Alternative, empty, (<*>), liftA2, many)
import Control.Monad (MonadPlus, mplus)
import Data.Char(toLower, toUpper, isAsciiLower, isAsciiUpper)
import Data.Maybe(isJust)
import Data.List(stripPrefix, isSuffixOf)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE



{--
      ; ABNF for windows paths
      ; based on https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/62e862f4-2a51-452e-8eeb-dc4ff5ee33cc?redirectedfrom=MSDN
      ; missing: unix separators

  filepath           = namespace *"\" namespace-tail
                     / UNC
                     / [ disk ] *"\" relative-path
                     / disk *"\"

  relative-path      = 1*(path-name 1*"\") [ file-name ] / file-name
  path-name          = 1*pchar
  file-name          = 1*pchar [ stream ]

  ; namespaces
  namespace          = file-namespace / device-namespace / nt-namespace
  namespace-tail     = ( disk 1*"\" relative-path ; C:foo\bar is not valid
                                                  ; namespaced paths are all absolute
                       / disk *"\"
                       / relative-path
                       )
  file-namespace     = "\" "\" "?" "\"
  device-namespace   = "\" "\" "." "\"
  nt-namespace       = "\" "?" "?" "\"

  UNC                = "\\" 1*pchar "\" 1*pchar  [ 1*"\" [ relative-path ] ]

  disk               = ALPHA ":"

  stream             = ":" 1*schar [ ":" 1*schar ] / ":" ":" 1*schar

  ; path compontent charactes (all printable chars except '\')
  pchar               = %x21-5B / %x5D-7E
  ; stream compontent charactes (all printable chars except '\' and ':')
  schar               = %x21-39 / %x3B-5B / %x5D-7E
--}


newtype Parser i o = Parser { runParser :: [i] -> ([i], Maybe o ) }

instance Functor (Parser i) where
  fmap f (Parser st) = Parser $ \stream -> case st stream of
    (next, Nothing) -> (next, Nothing)
    (next, Just o)  -> (next, Just $ f o)
                                           
instance Applicative (Parser i) where
  pure a = Parser $ \stream -> (stream, Just a)
  Parser ff <*> Parser xx = Parser $ \stream -> case ff stream of
    (next, Nothing) -> (next, Nothing)
    (next1, Just f)  -> case xx next1 of
      (next, Nothing) -> (next, Nothing)
      (next2, Just o)  -> (next2, Just $ f o)

instance Monad (Parser i) where
  return a = Parser $ \stream -> (stream, Just a)
  (Parser a) >>= mf = Parser $ \stream -> case a stream of
    (next, Nothing) -> (next, Nothing)
    (next, Just o)  -> runParser (mf o) next

instance Alternative (Parser i) where
  empty = Parser $ \_ -> ([], Nothing)
  Parser l <|> Parser r = Parser $ \stream -> case l stream of
    (_, Nothing) -> r stream -- we backtrack
    (next, Just o)  -> (next, Just o)

instance MonadPlus (Parser i)

getParse :: Parser i o -> [i] -> Maybe o
getParse p i = snd . runParser p $ i

parseElem :: Eq e => e -> Parser e e
parseElem e = satisfy (==e)

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the token that is actually
-- parsed.
satisfy :: (i -> Bool) -> Parser i i
satisfy f = Parser $ \stream -> case stream of
  [] -> ([], Nothing)
  (x:xs)
    | f x -> (xs, Just x)
    | otherwise -> (x:xs, Nothing) -- backtrack


-- | @string s@ parses a sequence of characters that identically match
-- @s@. Returns the parsed string (i.e. @s@).
string :: Eq i => [i] -> Parser i [i]
string str = Parser $ \stream -> case stream of
  [] -> ([], Nothing)
  xs -> case stripPrefix str xs of
    Just next -> (next, Just str)
    Nothing   -> (stream, Nothing) -- backtrack

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Parser i Bool
atEnd = Parser $ \stream -> case stream of
  [] -> ([], Just True)
  _  -> ([], Just False)

-- | Match only if all input has been consumed.
endOfInput :: Parser i ()
endOfInput = Parser $ \stream -> case stream of
  [] -> ([], Just ())
  _  -> ([], Nothing)


-- | @manyTill p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan


-- | @choice ps@ tries to apply the actions in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- action.
choice :: Alternative f => [f a] -> f a
choice = asum

-- | @many1 p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@.
many1 :: Alternative f => f a -> f (NonEmpty a)
many1 p = liftA2 (:|) p (many p)

optional :: Alternative f => f a -> f (Maybe a)
optional a = fmap Just a <|> pure Nothing



-- filepath = namespace *"\" namespace-tail
--          / UNC
--          / [ disk ] *"\" relative-path
--          / disk *"\"
data WindowsFilePath = NS NameSpace [Separator] NSTail
                     | UNC UNCShare
                     | N (Maybe Char) [Separator] (Maybe RelFilePath)
                     -- ^ This differs from the grammar, because we allow
                     -- empty paths
  deriving (Show, Eq, Ord)


-- namespace-tail     = ( disk 1*"\" relative-path ; C:foo\bar is not valid
--                                                 ; namespaced paths are all absolute
--                      / disk *"\"
--                      / relative-path
--                      )
data NSTail = NST1 Char (NonEmpty Separator) RelFilePath
            | NST2 Char [Separator]
            | NST3 RelFilePath
  deriving (Show, Eq, Ord)

--  UNC = "\\" 1*pchar "\" 1*pchar  [ 1*"\" [ relative-path ] ]
data UNCShare = UNCShare Separator Separator
                         NonEmptyString
                         (NonEmpty Separator)
                         NonEmptyString
                         (Maybe (NonEmpty Separator, Maybe RelFilePath))
  deriving (Show, Eq, Ord)

-- > getParse parseUNCShare "" === Nothing
-- > getParse parseUNCShare "text.txt" === Nothing
-- > getParse parseUNCShare "\\\\/localhost/////\\share" === Nothing
-- > getParse parseUNCShare "//localhost/share" === Just (UNCShare UnixSep UnixSep (fromList "localhost") (fromList [UnixSep]) (fromList "share") Nothing)
-- > getParse parseUNCShare "\\\\localhost/\\share" === Just (UNCShare WindowsSep WindowsSep (fromList "localhost") (fromList [UnixSep, WindowsSep]) (fromList "share") Nothing)
-- > getParse parseUNCShare "\\\\localhost\\share\\my\\path.txt::$DATA" === Just (UNCShare WindowsSep WindowsSep (fromList "localhost") (fromList [WindowsSep]) (fromList "share") (Just (fromList [WindowsSep], Just (Rel1 (fromList [(fromList "my", fromList [WindowsSep])]) (Just (FileName (fromList "path.txt") (Just (DS2 (fromList "$DATA")))))))))
parseUNCShare :: Parser Char UNCShare
parseUNCShare = UNCShare
                <$> parseSeparator <*> parseSeparator
                <*> many1 pchar
                <*> many1 parseSeparator
                <*> many1 pchar
                <*> optional ((,) <$> many1 parseSeparator <*> optional parseRelFilePath)
 where
  pchar = satisfy (flip notElem ['/', '\\'])

--  relative-path = 1*(path-name 1*"\") [ file-name ] / file-name
data RelFilePath = Rel1 (NonEmpty (NonEmptyString, (NonEmpty Separator))) (Maybe FileName)
                 | Rel2 FileName
  deriving (Show, Eq, Ord)

-- > getParse parseRelFilePath "text.txt" === Just (Rel2 (FileName (fromList "text.txt") Nothing))
-- > getParse parseRelFilePath "baz\\foo/bar//text.txt" === Just (Rel1 (fromList [(fromList "baz", fromList [WindowsSep]),(fromList "foo", fromList [UnixSep]), (fromList "bar", fromList [UnixSep, UnixSep])]) (Just $ FileName (fromList "text.txt") Nothing))
-- > getParse parseRelFilePath "baz\\foo/bar//" === Just (Rel1 (fromList [(fromList "baz", fromList [WindowsSep]),(fromList "foo", fromList [UnixSep]), (fromList "bar", fromList [UnixSep, UnixSep])]) Nothing)
-- > getParse parseRelFilePath "" === Nothing
parseRelFilePath :: Parser Char RelFilePath
parseRelFilePath = relpath <|> filename
 where
  relpath  = Rel1 <$> many1 ((,) <$> path_name <*> many1 parseSeparator) <*> optional parseFileName
  filename = Rel2 <$> parseFileName
  path_name = many1 (satisfy (flip notElem ['/', '\\']))

--  file-name = 1*pchar [ stream ]
data FileName = FileName NonEmptyString (Maybe DataStream)
  deriving (Show, Eq, Ord)

-- > getParse parseFileName "text.txt" === Just (FileName (fromList "text.txt") Nothing)
-- > getParse parseFileName "text.txt::::" === Just (FileName (fromList "text.txt::::") Nothing)
-- > getParse parseFileName "text.txt::$DATA" === Just (FileName (fromList "text.txt") (Just (DS2 (fromList "$DATA"))))
-- > getParse parseFileName "text.txt:type:$DATA" === Just (FileName (fromList "text.txt") (Just (DS1 (fromList "type") (Just $ fromList "$DATA"))))
-- > getParse parseFileName "text.txt:type" === Just (FileName (fromList "text.txt") (Just (DS1 (fromList "type") Nothing)))
-- > getParse parseFileName "/text.txt" === Nothing
-- > getParse parseFileName "" === Nothing
-- > getParse parseFileName "text.txt/" === Nothing
parseFileName :: Parser Char FileName
parseFileName = filenameDS <|> filenameAny
 where
  filenameDS  = FileName <$> many1 pchar <*> optional parseDatastream <* endOfInput
  filenameAny = (flip FileName Nothing) <$> many1 pcharAny <* endOfInput
  pchar    = satisfy (flip notElem [':', '/', '\\'])
  pcharAny = satisfy (flip notElem ['/', '\\'])

--  stream = ":" 1*schar [ ":" 1*schar ] / ":" ":" 1*schar
data DataStream = DS1 NonEmptyString (Maybe NonEmptyString)
                | DS2 NonEmptyString -- ::datatype
  deriving (Show, Eq, Ord)

-- > getParse parseDatastream "::$DATA" === Just (DS2 (fromList "$DATA"))
-- > getParse parseDatastream ":type:$DATA" === Just (DS1 (fromList "type") (Just $ fromList "$DATA"))
-- > getParse parseDatastream ":type" === Just (DS1 (fromList "type") Nothing)
-- > getParse parseDatastream ":" === Nothing
-- > getParse parseDatastream "::" === Nothing
-- > getParse parseDatastream "lol" === Nothing
-- > getParse parseDatastream "" === Nothing
parseDatastream :: Parser Char DataStream
parseDatastream = ds1 <|> ds2
 where
  ds1 = DS1 <$> (colon *> many1 schar) <*> optional (colon *> many1 schar)
  ds2 = DS2 <$> (colon *> colon *> many1 schar)
  schar = satisfy (flip notElem [':', '/', '\\'])
  colon = parseElem ':'

data Separator = UnixSep
               | WindowsSep
  deriving (Show, Eq, Ord)

parseSeparator :: Parser Char Separator
parseSeparator = fmap parseSep $ satisfy isPathSeparator
 where
  parseSep :: Char -> Separator
  parseSep '\\' = WindowsSep
  parseSep '/' = UnixSep

  isPathSeparator :: Char -> Bool
  isPathSeparator '/' = True
  isPathSeparator '\\' = True
  isPathSeparator _ = False



type NonEmptyString = NonEmpty Char


-- | Windows API Namespaces
--
-- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#namespaces
-- https://support.microsoft.com/en-us/topic/70b92942-a643-2f2d-2ac6-aad8acad49fb
-- https://superuser.com/a/1096784/854039
-- https://reverseengineering.stackexchange.com/a/15178
-- https://stackoverflow.com/a/25099634
--
-- namespace          = file-namespace / device-namespace / nt-namespace
-- file-namespace     = "\" "\" "?" "\"
-- device-namespace   = "\" "\" "." "\"
-- nt-namespace       = "\" "?" "?" "\"
data NameSpace = FileNameSpace
               | DeviceNameSpace
               | NTNameSpace
  deriving (Show, Eq, Ord)




{--
-- | Parse a filepath into lexemes.
--
-- -> display (parse x) === x
-- -> parse "\\\\?\\UNC\\localhost\\c$\\foo\\bar" === []
parse :: String -> [Lexeme]
parse fp'
  -- '\\?\UNC\share\path'
  | Just (lx, rest) <- parseExtendedUNC fp' = lx         ++ parsePath rest
  -- '\\?\C:\path'
  | Just (lx1, r1)  <- parseNameSpace fp'
  , Just (lx2, r2)  <- parseDisk r1         = lx1 ++ lx2 ++ parsePath r2
  -- '\\.\COM1'
  | Just (lx1@(NS DeviceNameSpace:_), r1) <- parseNameSpace fp'
  , Just (lx2, r2)  <- parseDevice r1       = lx1 ++ lx2 ++ parsePath r2
  -- '\\?\some\other', '\\.\some\other', '\??\some\other'
  -- detects no device/disk
  | Just (lx, rest) <- parseNameSpace fp'   = lx         ++ parsePath rest
  -- 'C:\path'
  | Just (lx, rest) <- parseDisk fp'        = lx         ++ parsePath rest
  -- '\\share\path'
  | Just (lx, rest) <- parseDriveShare fp'  = lx         ++ parsePath rest
  -- 'relative\path' and everything else
  | otherwise = parsePath fp'
 where
  parsePath :: String -> [Lexeme]
  parsePath fp = 
    case parseFileName fp <|> parseSeparators fp of
      Nothing         -> []
      Just (lx, [])   -> lx
      Just (lx, rest) -> lx ++ parsePath rest

  parseFileName :: String -> Maybe ([Lexeme], String)
  parseFileName fp =
    case break isPathSeparator fp of
      ([], _) -> Nothing
      (a, r)  -> Just ([FileName a], r)

  parseSeparators :: String -> Maybe ([Lexeme], String)
  parseSeparators fp
   | null a = Nothing
   | otherwise = Just ([Separators $ fmap parseSep a], b)
      where (a, b) = span isPathSeparator fp

  parseSep :: Char -> Separator
  parseSep '\\' = WindowsSep
  parseSep '/' = UnixSep

  -- prefix only
  -- TODO: upper and lowercase unc
  parseExtendedUNC :: String -> Maybe ([Lexeme], String)
  parseExtendedUNC ('\\':'\\' :'?':'\\':'U':'N':'C':s4:r1) | isPathSeparator s4 = do
    (lx, r2) <- parseSeparators (s4:r1)
    (share, r3) <- parseDriveShareName r2
    pure ([NS FileNameSpace] ++ lx ++ share, r3)
  parseExtendedUNC _ = Nothing
    
  -- prefix only
  parseNameSpace :: String -> Maybe ([Lexeme], String)
  parseNameSpace ('\\':'?' :'?':'\\':rest) = Just ([NS NTNameSpace    ], rest)
  parseNameSpace ('\\':'\\':'?':'\\':rest) = Just ([NS FileNameSpace  ], rest)
  parseNameSpace ('\\':'\\':'.':'\\':rest) = Just ([NS DeviceNameSpace], rest)
  parseNameSpace _                         = Nothing

  -- prefix or after any NameSpace
  parseDisk :: String -> Maybe ([Lexeme], String)
  parseDisk (x:':':rest) | isLetter x = Just ([Disk x], rest)
  parseDisk _ = Nothing

  parseDevice :: String -> Maybe ([Lexeme], String)
  parseDevice fp =
    case break isPathSeparator fp of
      ([], _) -> Nothing
      (a, r)  -> Just ([Device a], r)

  -- \\sharename\
  parseDriveShare :: String -> Maybe ([Lexeme], String)
  parseDriveShare (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 = do
    (lx, rest) <- parseDriveShareName xs
    pure ([Separators [ parseSep s1, parseSep s2]] ++ lx, rest)
  parseDriveShare _ = Nothing

  -- assume you have already seen \\
  -- share\bob -> "share\", "bob"
  parseDriveShareName :: String -> Maybe ([Lexeme], String)
  parseDriveShareName name = do
    case break isPathSeparator name of
      ([], _) -> Nothing
      (share, b)  -> case break isPathSeparator b of
        ([], _) -> Nothing
        (drive, r) -> Just ([Share share drive], r)

  optional :: String -> (String -> Maybe ([Lexeme], String)) -> ([Lexeme], String)
  optional input parser = maybe ([], input) id (parser input)




display :: [Lexeme] -> String
display lxs = case lxs of
                (ns@(NS FileNameSpace):sep@(Separators _):share@(Share _ _):rest) -> 
                  d ns ++ "UNC" ++ d sep ++ d share ++ display rest
                other -> concatMap d other
 where
  d :: Lexeme -> String
  d (NS FileNameSpace)   = "\\\\?\\"
  d (NS DeviceNameSpace) = "\\\\.\\"
  d (NS NTNameSpace)     = "\\??\\"
  d (Disk c)             = c:":"
  d (Device dev)         = dev
  d (Share share drive)  = share
  d (Separators sep)     = sep
  d (FileName fn)        = fn
--}
