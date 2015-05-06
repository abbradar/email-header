{-# LANGUAGE OverloadedStrings #-}
-- | Formatting and pretty-printing header types.
module Network.Email.Header.Pretty
    ( -- * Combinators
      commaSep
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
    ) where

import           Control.Arrow
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import           Data.ByteString.Internal     (w2c)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as E
import           Data.Text.Lazy.Builder       (Builder)
import qualified Data.Text.Lazy.Builder       as B
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Char
import qualified Data.Map                     as Map
import           Data.Monoid
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.Text.Lazy               as L
import           Data.Word

import           Network.Email.Charset
import           Network.Email.Header.Doc
import           Network.Email.Header.Layout  as F
import           Network.Email.Header.Types

-- | Separate a group with commas.
commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f = sep . punctuate "," . map f

-- | Surround a 'Doc' with angle brackets.
angle :: Doc -> Doc
angle d = "<" <> d <> ">"

-- | Render a case-insensitive 'T.Text'.
textCI :: CI T.Text -> Doc
textCI = text . CI.original

-- | Render a case-insensitive 'T.Text'.
byteStringCI :: CI BS.ByteString -> Doc
byteStringCI = byteString . CI.original

-- | Format a date and time.
dateTime :: ZonedTime -> Doc
dateTime (ZonedTime local zone) = localTime local </> timeZone zone
  where
    localTime (LocalTime day tod) = date day </> timeOfDay tod

    date day = dayNames !! (w - 1) <> "," </>
        pad_ 2 d </> months !! (m - 1) </> pad0 4 (fromInteger y)
      where
        (y, m, d) = toGregorian day
        (_, _, w) = toWeekDate day

    timeOfDay (TimeOfDay h m s) =
        pad0 2 h <> ":" <> pad0 2 m <> ":" <> pad0 2 (floor s)

    timeZone = signed timeZoneOffset . timeZoneMinutes

    timeZoneOffset n = pad0 2 hh <> pad0 2 mm
      where
        (hh, mm) = n `divMod` 60

    pad c w n = string $ replicate (w - length s) c ++ s
      where
        s = show n

    pad_ = pad ' '
    pad0 = pad '0'

    signed f n
        | n >= 0    = "+" <> f n
        | otherwise = "-" <> f (negate n)

    dayNames = [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
    months   = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
               , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
               ]

-- | Format an address.
address :: Address -> Doc
address (Address s) = text s

-- | Format an address with angle brackets.
angleAddr :: Address -> Doc
angleAddr = angle . address

-- | Format a 'Mailbox'.
mailbox :: Mailbox -> Doc
mailbox (Mailbox n a) = case n of
    Nothing   -> address a
    Just name -> phrase name </> angleAddr a

-- | Format a list of 'Mailbox'es.
mailboxList :: [Mailbox] -> Doc
mailboxList = commaSep mailbox

-- | Format a 'Recipient'.
recipient :: Recipient -> Doc
recipient (Individual m)  = mailbox m
recipient (Group name ms) = phrase name <> ":" </> mailboxList ms <> ";"

-- | Format a list of 'Recipient's.
recipientList :: [Recipient] -> Doc
recipientList = commaSep recipient

-- | Format a message identifier
messageID :: MessageID -> Doc
messageID (MessageID s) = angle (text s)

-- | Format a list of message identifiers.
messageIDList :: [MessageID] -> Doc
messageIDList = commaSep messageID

-- | Convert a word to a hexadecimal value.
hex :: Word8 -> Builder
hex w = toHexDigit a <> toHexDigit b
  where
    (a, b)          = w `divMod` 16
    toHexDigit n
        | n < 10    = B.singleton $ w2c (n + 48)
        | otherwise = B.singleton $ w2c (n + 55)

-- | Encode a word.
encodeWord :: RenderOptions -> L.Text -> (Int, Builder)
encodeWord r = encodeWith (encoding r) . L.toStrict
  where
    useUtf8           = utf8mime r
                     && (charsetName (charset r) == "UTF-8")

    encodeWith QP     = if useUtf8
                        then encodeQUtf8
                        else encodeQ . fromUnicode (charset r)
    encodeWith Base64 = encodeBase64 . fromUnicode (charset r)

    encodeQ           = first getSum .
                        BS.foldr (\w a -> encodeWord8 w <> a) mempty

    encodeQUtf8       = first getSum .
                        T.foldr (\w a -> encodeChar w <> a) mempty
    encodeWord8 w
        | w == 32      = (Sum 1, B.singleton '_')
        | isIllegal w = (Sum 3, B.singleton '=' <> hex w)
        | otherwise   = (Sum 1, B.singleton $ w2c w)
    
    encodeChar c
        | c == ' ' = (Sum 1, B.singleton '_')
        | isAscii c && isIllegal (fromIntegral $ fromEnum c) =
              BS.foldr (\w a -> (Sum 3, B.singleton '=' <> hex w) <> a) mempty $ E.encodeUtf8 $ T.singleton c
        | otherwise   = (Sum 1, B.singleton c)

    isIllegal w       = w < 33
                     || w > 126
                     || w `BS.elem` "()<>[]:;@\\\",?=_"

    encodeBase64 b    = let e = Base64.encode b
                        in (BS.length e, B.fromText $ E.decodeUtf8 e)

-- | Split nonempty text into a layout that fits the given width and the
-- remainder.
-- TODO: inefficient
splitWord :: RenderOptions -> Int -> L.Text -> (Layout Builder, L.Text)
splitWord r w t =
    first (uncurry F.span) .
    last .
    takeWhile1 (fits . fst) .
    map (first (encodeWord r)) .
    drop 1 $
    zip (L.inits t) (L.tails t)
  where
    fits (l, _) = l <= w

    takeWhile1 _ []     = []
    takeWhile1 p (x:xs) = x : takeWhile p xs

-- | Layout text as an encoded word.
layoutText :: RenderOptions -> Bool -> L.Text -> Layout Builder
layoutText r h t0
    | L.null t0 = mempty
    | h         = prefix <> uncurry F.span (encodeWord r t0) <> postfix
    | otherwise = splitLines t0
  where
    name    = map toLower . charsetName $ charset r

    method  = case encoding r of
        QP     -> 'Q'
        Base64 -> 'B'

    prefix  = F.span (5 + length name) $
        B.fromText "=?" <>
        B.fromString name <>
        B.singleton '?' <>
        B.singleton method <>
        B.singleton '?'

    postfix = F.span 2 (B.fromText "?=")

    padding = 7 + length name

    splitLines t = F.position $ \p ->
        let (l, t') = splitWord r (lineWidth r - padding - p) t
        in  prefix <> l <> postfix <>
            (if L.null t' then mempty else newline r <> splitLines t')

-- | Encode text as an encoded word.
encodeText :: L.Text -> Doc
encodeText t = prim $ \r h -> layoutText r h t

-- | Encode text, given a predicate that checks for illegal characters.
renderText :: (Char -> Bool) -> L.Text -> Doc
renderText isIllegalChar t
    | mustEncode = encodeText t
    | otherwise  = sep (map lazyText ws)
  where
    ws         = L.words t

    mustEncode = L.unwords ws /= t
              || any ("=?" `L.isPrefixOf`) ws
              || L.any isIllegalChar t

-- | Format a phrase. The text is encoded as is, unless:
--
-- * The text contains leading or trailing whitespace, or more than one space
--   between words
--
-- * Any word begins with @=?@
--
-- * Any word contains illegal characters
phrase :: L.Text -> Doc
phrase = renderText (\c -> c > '~' || c < '!' || c `elem` ("()<>[]:;@\\\"," :: String))

-- | Format a list of phrases.
phraseList :: [L.Text] -> Doc
phraseList = commaSep phrase

-- | Format unstructured text. The text is encoded as is, unless:
--
-- * The text contains leading or trailing whitespace, or more than one space
--   between words
--
-- * Any word begins with @=?@
--
-- * Any word contains illegal characters
unstructured :: L.Text -> Doc
unstructured = renderText (\c -> c > '~' || c < '!')

-- | Format the MIME version.
mimeVersion ::  Int -> Int -> Doc
mimeVersion major minor = int major <> "." <> int minor
  where
    int = string . show

-- | Format the content type and parameters.
contentType :: MimeType -> Parameters -> Doc
contentType (MimeType t s) params = sep . punctuate ";" $
    renderMimeType : map renderParam (Map.toList params)
  where
    renderMimeType     = byteStringCI t <> "/" <> byteStringCI s
    renderParam (k, v) = textCI k <> "=" <> text v

-- | Format the content transfer encoding.
contentTransferEncoding :: CI BS.ByteString -> Doc
contentTransferEncoding = byteStringCI
