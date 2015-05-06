{-# LANGUAGE DeriveDataTypeable #-}
-- | Email header types.
module Network.Email.Header.Types
    ( -- * Header types
      HeaderName
    , Header
    , Headers
      -- * Email types
    , Address(..)
    , Mailbox(..)
    , Recipient(..)
    , MessageID(..)
    , MimeType(..)
    , Parameters
      -- * Exceptions
    , HeaderException(..)
    ) where

import           Control.Exception
import qualified Data.ByteString      as B
import           Data.CaseInsensitive (CI)
import           Data.Map.Strict      (Map)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import           Data.Typeable

-- | An email header name.
type HeaderName = CI B.ByteString

-- | An email header.
type Header = (HeaderName, L.Text)

-- | A set of email headers, in order.
type Headers = [Header]

-- | An email address.
newtype Address = Address T.Text
    deriving (Eq, Ord, Show)

-- | A 'Mailbox' receives mail.
data Mailbox = Mailbox
    { displayName    :: Maybe L.Text
    , mailboxAddress :: Address
    } deriving (Eq, Show)

-- | A 'Recipient' is used to indicate senders and recipients of messages.
-- It may either be an individual 'Mailbox', or a named group of
-- @'Mailbox'es@.
data Recipient
    = Individual Mailbox
    | Group L.Text [Mailbox]
    deriving (Eq, Show)

-- | A message identifier, which has a similar format to an email address.
newtype MessageID = MessageID T.Text
    deriving (Eq, Ord, Show)

-- | A MIME type.
data MimeType = MimeType
    { mimeType    :: CI B.ByteString
    , mimeSubtype :: CI B.ByteString
    } deriving (Eq, Ord, Show)

-- | MIME content type parameters.
type Parameters = Map (CI T.Text) T.Text

-- | Email header exceptions.
data HeaderException
      -- | A required header is missing.
    = MissingHeader HeaderName
      -- | A header field could not be parsed.
    | HeaderParseError Header String
    deriving (Show, Typeable)

instance Exception HeaderException
