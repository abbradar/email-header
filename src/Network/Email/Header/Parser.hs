{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Header parsers. Most exported parsers (with the exception of 'fws',
-- 'cfws', and 'unstructured') are for parsing structured header fields.
-- They expect no leading space and will eat an trailing white space.
module Network.Email.Header.Parser
    ( -- * Utils
      hexPair
    , parseEither
    , parseMaybe
      -- * Whitespace
    , fws
    , cfws
     -- * Combinators
    , lexeme
    , symbol
    , commaSep
      -- * Date and time
    , dateTime
      -- * Addresses
    , address
    , mailbox
    , mailboxList
    , recipient
    , recipientList
      -- * Message IDs
    , messageID
    , messageIDList
      -- * Text
    , phrase
    , phraseList
    , unstructured
      -- * MIME
    , mimeVersion
    , contentType
    , contentTransferEncoding
      -- * Headers
    , headerName
    , header
    , headers
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text             (Parser)
import qualified Data.Attoparsec.Text             as A
import qualified Data.Attoparsec.ByteString.Char8 as AB
import           Data.Char
import           Data.Function
import           Data.Attoparsec.Combinator
import           Data.Bits
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Base64           as Base64
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Builder     as BL
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.List
import qualified Data.Map.Strict                  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.Text                        as T
import           Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TB
import           Data.Word

import           Network.Email.Charset
import           Network.Email.Header.Types   hiding (mimeType)

infixl 3 <+>

-- | Concatenate the results of two parsers.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Repeat and concatenate.
concatMany :: (Alternative f, Monoid a) => f a -> f a
concatMany p = mconcat <$> many p

-- | Return a 'Just' value, and 'fail' a 'Nothing' value.
parseMaybe :: Monad m => String -> Maybe a -> m a
parseMaybe s = maybe (fail s) return

-- | Return a 'Right' value, and 'fail' a 'Left' value.
parseEither :: Monad m => Either String a -> m a
parseEither = either fail return

-- | Run a 'Builder' as a strict 'T.Text'.
toText :: TB.Builder -> T.Text
toText = TL.toStrict . TB.toLazyText

-- | Skip folding whitespace.
fws :: Parser (Maybe Char)
fws = optional $ ' ' <$ (A.skip (== ' ') <|> void (A.string "\r\n ")) <* fws

-- | Check if a char is an non-whitespace control char.
isNoWsCtl :: Char -> Bool
isNoWsCtl = A.inClass "\1-\8\11\12\14-\31\127"

-- | Check if a char is a valid quoted char.

-- | Parse a comment, including all nested comments.
comment :: Parser TL.Text
-- Technically we are expected to parse encoded-words in comments, but we don't need to.
comment = TB.toLazyText <$> fix (scanString '(' ')') -- <|> encodedWord

-- | Skip any comments or folding white space.
cfws :: Parser ()
cfws = void $ optional $ do
  s <- fws
  void (many1 $ comment *> fws) <|> unless (isJust s) (fail "No folding white space")

-- | Parse a value followed by whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* cfws

-- | Parse a character followed by whitespace.
symbol :: Char -> Parser Char
symbol = lexeme . A.char

-- | Quickly (and unsafely) convert a digit to the number it represents.
fromDigit :: Integral a => Char -> a
fromDigit w = fromIntegral (fromEnum w - 48)

-- | Parse a fixed number of digits.
digits :: Integral a => Int -> Parser a
digits 0 = return 0
digits 1 = fromDigit <$> A.digit
digits n = do
    s <- A.take n
    unless (T.all isDigit s) $
        fail $ "expected " ++ show n ++ " digits"
    return $ T.foldl' (\a w -> 10*a + fromDigit w) 0 s

-- | Parse a number lexeme with a fixed number of digits.
number :: Integral a => Int -> Parser a
number = lexeme . digits

-- | Parse a hexadecimal pair.
hexPair :: AB.Parser Word8
hexPair = decode <$> hexDigit <*> hexDigit
  where
    decode a b      = shiftL a 4 .|. b
    hexDigit        = fromIntegral <$> fromHexDigit <$> AB.satisfy (AB.inClass "0-9A-F")
    fromHexDigit w
        | w >= 'A'   = fromEnum w - fromEnum 'A' + 10
        | otherwise = fromEnum w - fromEnum '0'

-- | Check if a char is an ASCII printable char, excluding special characters.
isPrintable :: String -> Char -> Bool
isPrintable specials w = w <= '\126' && w >= '\33' && A.notInClass specials w

-- | Check if a char is an Unicode or ASCII printable char, excluding special characters.
isPrintableU :: String -> Char -> Bool
isPrintableU specials w = not (isAscii w) || isPrintable specials w

-- | Parse an token lexeme consisting of all printable characters, but
--  disallowing the specified special characters.
tokenWith :: String -> Parser T.Text
tokenWith specials = lexeme (A.takeWhile1 $ isPrintableU specials)

-- | Variant of 'tokenWith' disallowing non-ASCII characters.
tokenAsciiWith :: String -> Parser B.ByteString
tokenAsciiWith specials = encodeUtf8 <$> lexeme (A.takeWhile1 $ isPrintable specials)

-- | Parse an atom, which contains ASCII letters, digits, and the
-- characters @"!#$%&\'*+-/=?^_`{|}~"@.
atom :: Parser TB.Builder
atom = TB.fromText <$> tokenWith "()<>[]:;@\\,\"."

-- | MIME special characters.
mimeSpecials :: String
mimeSpecials = "()<>@,;:\\\"/[]?="

-- | A MIME token, which contains ASCII letters, digits, and the
-- characters @"!#$%\'*+-^_`{|}~."@.
token :: Parser T.Text
token = tokenWith mimeSpecials

-- | Variant of 'token' disallowing non-ASCII characters.
tokenAscii :: Parser B.ByteString
tokenAscii = tokenAsciiWith mimeSpecials

-- | A case-insensitive MIME token.
tokenCI :: Parser (CI T.Text)
tokenCI = CI.mk <$> token

-- | A case-insensitive MIME token, disallowing non-ASCII characters.
tokenAsciiCI :: Parser (CI B.ByteString)
tokenAsciiCI = CI.mk <$> tokenAscii

-- | Parse a quoted-string.
quotedString :: Parser TB.Builder
quotedString = lexeme $ scanString '"' '"' mempty

-- | Parse an encoded word, as per RFC 2047.
encodedWord :: Parser TB.Builder
encodedWord = do
    _      <- A.string "=?"
    cname  <- T.unpack <$> tokenNoWS
    _      <- A.char '?'
    mname  <- CI.mk <$> tokenNoWS
    _      <- A.char '?'
    enc    <- encoded
    _      <- A.string "?="

    method <- decodeMethod mname
    body   <- parseEither $ method enc
    charset <- parseMaybe "charset not found" $ lookupCharset cname

    return $ TB.fromText $ toUnicode charset body
  where
    decodeMethod "Q" = return quoted
    decodeMethod "B" = return base64
    decodeMethod _   = fail "Unknown encoding method"

    tokenNoWS        = A.takeWhile1 (isPrintable mimeSpecials)

    encoded          = T.encodeUtf8 <$> A.takeWhile1 (isPrintableU "?")

    quoted           = AB.parseOnly (quotedParse <* AB.endOfInput)

    quotedParse      = BL.toStrict <$> BL.toLazyByteString <$> concatMany quotedChar

    quotedChar       = BL.byteString <$> B.map underscore <$> AB.takeWhile1 (/= '=')
                       <|> BL.word8 <$ AB.char '=' <*> hexPair
                       <|> mempty <$ AB.string "\r\n "

    underscore '_'   = ' '
    underscore c     = c

    base64           = Base64.decode

-- | Return a quoted string as-is.
scanString :: Char -> Char -> Parser TB.Builder -> Parser TB.Builder
scanString start end addrules = TB.singleton <$> A.char start <+> body <+> TB.singleton <$> A.char end
  where body = addrules
               <|> TB.fromText <$> A.takeWhile1 isBodyChar
               <|> TB.singleton <$ A.char '\\' <*> A.satisfy isQuotedChar
        isBodyChar c = isNoWsCtl c || isPrintableU (start:end:"\\") c
        isQuotedChar c = isNoWsCtl c || isPrintableU "" c

-- | Parse an email address, stripping out whitespace and comments.
addrSpec :: Parser T.Text
addrSpec = toText <$> (localPart <+> at <+> domain)
  where
    at            = TB.singleton <$> symbol '@'
    dot           = TB.singleton <$> symbol '.'
    dotSep p      = p <+> concatMany (dot <+> p)

    addrAtom      = atom
    domainLiteral = lexeme $ scanString '[' ']' mempty

    localPart     = dotSep (atom <|> quotedString)
    domain        = dotSep addrAtom <|> domainLiteral

-- | Parse an address specification in angle brackets.
angleAddrSpec :: Parser T.Text
angleAddrSpec = symbol '<' *> addrSpec <* symbol '>'

-- | Parse two or more occurences of @p@, separated by @sep@.
sepBy2 :: Alternative f => f a -> f b -> f [a]
sepBy2 p sep = (:) <$> p <*> many1 (sep *> p)

-- | Parse a list of elements, with possibly null entries in between
-- separators. At least one entry or separator will be parsed.
optionalSepBy1 :: Alternative f => f a -> f b -> f [a]
optionalSepBy1 p sep = catMaybes <$> sepBy2 (optional p) sep
                   <|> return <$> p

-- | Parse a list of elements, separated by commas.
commaSep :: Parser a -> Parser [a]
commaSep p = optionalSepBy1 p (symbol ',')

-- | Parse a date and time.
-- TODO: non-numeric timezones (such as \"PDT\") are considered equivalent
-- to UTC time.
dateTime :: Parser ZonedTime
dateTime = do
    wday  <- optional dayOfWeek
    zoned <- zonedTime
    let (_, _, expected) =
            toWeekDate . localDay . zonedTimeToLocalTime $ zoned
    case wday of
        Just actual | actual /= expected
          -> fail "day of week does not match date"
        _ -> return zoned
  where
    dayOfWeek = dayName <* symbol ','
    localTime = LocalTime <$> date <*> timeOfDay
    zonedTime = ZonedTime <$> localTime <*> timeZone

    date      = do
        d <- lexeme A.decimal
        m <- month
        y <- year
        parseMaybe "invalid date" $ fromGregorianValid y m d

    year      =              number 4
            <|> (+ 1900) <$> number 3
            <|> adjust   <$> number 2
      where
        adjust n | n < 50    = 2000 + n
                 | otherwise = 1900 + n

    timeOfDay = do
        h <- number 2
        m <- symbol ':' *> number 2
        s <- option (0 :: Int) (symbol ':' *> number 2)
        parseMaybe "invalid time of day" $
            makeTimeOfDayValid h m (fromIntegral s)

    timeZone  = minutesToTimeZone <$> timeZoneOffset
            <|> return utc

    timeZoneOffset = lexeme . A.signed $ do
        hh <- digits 2
        mm <- digits 2
        if mm >= 60
            then fail "invalid time zone"
            else return $ hh * 60 + mm

    listIndex = lexeme . choice . map (\(n, s) -> n <$ A.string s) . zip [1..]
    dayName   = listIndex [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
    month     = listIndex [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                          , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                          ]

-- | Parse an email address in angle brackets.
angleAddr :: Parser Address
angleAddr = Address <$> angleAddrSpec

-- | Parse an email address.
address :: Parser Address
address = Address <$> addrSpec

-- | Parse a 'Mailbox'.
mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional phrase <*> angleAddr
      <|> Mailbox Nothing <$> address

-- | Parse a list of @'Mailbox'es@.
mailboxList :: Parser [Mailbox]
mailboxList = commaSep mailbox

-- | Parse a 'Recipient'.
recipient :: Parser Recipient
recipient = Group <$> phrase <* symbol ':' <*> mailboxList <* symbol ';'
        <|> Individual <$> mailbox

-- | Parse a list of @'Recipient's@.
recipientList :: Parser [Recipient]
recipientList = commaSep recipient

-- | Parse a message identifier.
messageID :: Parser MessageID
messageID = MessageID <$> angleAddrSpec

-- | Parse a list of message identifiers.
messageIDList :: Parser [MessageID]
messageIDList = many1 messageID

-- | Parse a phrase. Adjacent encoded words are concatenated. White space
-- is reduced to a single space, except when quoted or part of an encoded
-- word.
phrase :: Parser TL.Text
phrase = TB.toLazyText <$> mconcat <$>
         ((:) <$> element <*> many (element <|> TB.singleton <$> symbol '.'))
  where
    element = mconcat <$> many1 (lexeme encodedWord)
          <|> quotedString
          <|> atom

-- | Parse a comma-separated list of phrases.
phraseList :: Parser [TL.Text]
phraseList = commaSep phrase

-- | Parse unstructured text. Adjacent encoded words are concatenated.
-- White space is reduced to a single space, except when part of an encoded
-- word.
unstructured :: Parser TL.Text
unstructured = TB.toLazyText <$> mconcat <$> intersperse (TB.singleton ' ') <$> many element
  where
    element = mconcat <$> many1 (encodedWord <* fws)
              <|> word <* fws

    word    = TB.fromText <$> A.takeWhile1 (\x -> isPrintableU "" x || x == ' ')

-- | Parse the MIME version (which should be 1.0).
mimeVersion :: Parser (Int, Int)
mimeVersion = (,) <$> number 1 <* symbol '.' <*> number 1

-- | Parse the content type.
contentType :: Parser (MimeType, Parameters)
contentType = (,) <$> mimeType <*> parameters
  where
    mimeType   = MimeType <$> tokenAsciiCI <* symbol '/' <*> tokenAsciiCI
    parameters = Map.fromList <$> many (symbol ';' *> parameter)
    parameter  = (,) <$> tokenCI <* symbol '=' <*> (token <|> toText <$> quotedString)

-- | Parse the content transfer encoding.
contentTransferEncoding :: Parser (CI B.ByteString)
contentTransferEncoding = tokenAsciiCI

-- | Parse the ASCII header name.
headerName :: Parser HeaderName
headerName = CI.mk <$> encodeUtf8 <$> A.takeWhile1 (isPrintable ":")

-- | Parse one header field, without parsing insides.
header :: Parser Header
header = (,) <$> headerName <* A.char ':' <*> (TB.toLazyText <$> body)
  where body = do
          str <- TB.fromText <$> A.takeWhile (/= '\r')
          next <- end
                  <|> (<>) <$> (TB.singleton <$> A.anyChar) <*> body
          return $ str <> next
        end = do
          _ <- A.string "\r\n"
          c <- A.peekChar'
          unless (c /= ' ') $ fail "Folding whitespace"
          return mempty

-- | Split multiple headers.
headers :: Parser Headers
headers = many1 header
