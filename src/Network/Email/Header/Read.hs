{-# LANGUAGE OverloadedStrings #-}
-- | Reading common header fields.
-- This module is intended to be imported qualified:
--
-- > import qualified Network.Email.Header.Read as H
module Network.Email.Header.Read
    ( -- * Parsing
      field
      -- * Origination date field
    , date
      -- * Originator fields
    , from
    , sender
    , replyTo
      -- * Destination address fields
    , to
    , cc
    , bcc
      -- * Identification fields
    , messageID
    , inReplyTo
    , references
      -- * Informational fields
    , subject
    , comments
    , keywords
      -- * Resent fields
    , resentDate
    , resentFrom
    , resentSender
    , resentTo
    , resentCc
    , resentBcc
    , resentMessageID
      -- * MIME fields
    , mimeVersion
    , contentType
    , contentTransferEncoding
    , contentID
    , boundary
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import qualified Data.Map                    as M
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text.Lazy
import qualified Data.ByteString             as B
import           Data.CaseInsensitive        (CI)
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Encoding          as T
import           Data.Time.LocalTime

import qualified Network.Email.Header.Parser as P
import           Network.Email.Header.Types

-- | Lookup and parse a header with a parser.
field :: MonadThrow m => HeaderName -> Parser a -> Headers -> m a
field k p hs = do
    body <- case lookup k hs of
        Nothing -> throwM $ MissingHeader k
        Just b  -> return b
    case parse (P.cfws *> p <* endOfInput) body of
        Fail _ _ s -> throwM $ HeaderParseError (k, body) s
        Done _ a   -> return a

-- | Get the value of the @Date:@ field.
date :: MonadThrow m => Headers -> m ZonedTime
date = field "Date" P.dateTime

-- | Get the value of the @From:@ field.
from :: MonadThrow m => Headers -> m [Mailbox]
from = field "From" P.mailboxList

-- | Get the value of the @Sender:@ field.
sender :: MonadThrow m => Headers -> m Mailbox
sender = field "Sender" P.mailbox

-- | Get the value of the @Reply-To:@ field.
replyTo :: MonadThrow m => Headers -> m [Recipient]
replyTo = field "Reply-To" P.recipientList

-- | Get the value of the @To:@ field.
to :: MonadThrow m => Headers -> m [Recipient]
to = field "To" P.recipientList

-- | Get the value of the @Cc:@ field.
cc :: MonadThrow m => Headers -> m [Recipient]
cc = field "Cc" P.recipientList

-- | Get the value of the @Bcc:@ field.
bcc :: MonadThrow m => Headers -> m (Maybe [Recipient])
bcc = field "Bcc" (optional P.recipientList)

-- | Get the value of the @Message-ID:@ field.
messageID :: MonadThrow m => Headers -> m MessageID
messageID = field "Message-ID" P.messageID

-- | Get the value of the @In-Reply-To:@ field.
inReplyTo :: MonadThrow m => Headers -> m [MessageID]
inReplyTo = field "In-Reply-To" (many1 P.messageID)

-- | Get the value of the @References:@ field.
references :: MonadThrow m => Headers -> m [MessageID]
references = field "References" (many1 P.messageID)

-- | Get the value of the @Subject:@ field.
subject :: MonadThrow m => Headers -> m TL.Text
subject = field "Subject" P.unstructured

-- | Get the value of the @Comments:@ field.
comments :: MonadThrow m => Headers -> m TL.Text
comments = field "Comments" P.unstructured

-- | Get the value of the @Keywords:@ field.
keywords :: MonadThrow m => Headers -> m [TL.Text]
keywords = field "Keywords" P.phraseList

-- | Get the value of the @Resent-Date:@ field.
resentDate :: MonadThrow m => Headers -> m ZonedTime
resentDate = field "Resent-Date" P.dateTime

-- | Get the value of the @Resent-From:@ field.
resentFrom :: MonadThrow m => Headers -> m [Mailbox]
resentFrom = field "Resent-From" P.mailboxList

-- | Get the value of the @Resent-Sender:@ field.
resentSender :: MonadThrow m => Headers -> m Mailbox
resentSender = field "Resent-Sender" P.mailbox

-- | Get the value of the @Resent-To:@ field.
resentTo :: MonadThrow m => Headers -> m [Recipient]
resentTo = field "Resent-To" P.recipientList

-- | Get the value of the @Resent-Cc:@ field.
resentCc :: MonadThrow m => Headers -> m [Recipient]
resentCc = field "Resent-Cc" P.recipientList

-- | Get the value of the @Resent-Bcc:@ field.
resentBcc :: MonadThrow m => Headers -> m (Maybe [Recipient])
resentBcc = field "Resent-Bcc" (optional P.recipientList)

-- | Get the value of the @Resent-Message-ID:@ field.
resentMessageID :: MonadThrow m => Headers -> m MessageID
resentMessageID = field "Resent-Message-ID" P.messageID

-- | Get the value of the @MIME-Version:@ field.
mimeVersion :: MonadThrow m => Headers -> m (Int, Int)
mimeVersion = field "MIME-Version" P.mimeVersion

-- | Get the value of the @Content-Type:@ field.
contentType :: MonadThrow m => Headers -> m (MimeType, Parameters)
contentType = field "Content-Type" P.contentType

-- | Get the value of the @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: MonadThrow m => Headers -> m (CI B.ByteString)
contentTransferEncoding =
    field "Content-Transfer-Encoding" P.contentTransferEncoding

-- | Get the value of the @Content-ID:@ field.
contentID :: MonadThrow m => Headers -> m MessageID
contentID = field "Content-ID" P.messageID

-- | Get the value of the MIME multipart boundary.
boundary :: MonadThrow m => Headers -> m B.ByteString
boundary hdrs = do
  (tp, params) <- contentType hdrs
  unless (mimeType tp == "multipart") $ throwM $
    InvalidHeader "Content-Type" "Content is not multipart"
  case M.lookup "boundary" params of
   Nothing -> throwM $ InvalidHeader "Content-Type" "Multipart content has undefined boundary"
   Just r -> return $ T.encodeUtf8 r
