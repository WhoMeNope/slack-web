{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Conversation
-- Description:
--
----------------------------------------------------------------------

module Web.Slack.Conversation
  ( Conversation(..)
  , ConversationType(..)
  , ListReq(..)
  , ListRsp(..)
  )
  where

import Control.Applicative ((<|>))
import Data.Aeson.TH
import Data.Aeson.Types
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Web.FormUrlEncoded
import Web.Internal.HttpApiData

import Web.Slack.Common
import Web.Slack.Util

-- |
--
--
data ConversationType
  = TypePublicChannel
  | TypePrivateChannel
  | TypeMpim
  | TypeIm
  deriving (Eq, Generic, Show)

instance ToHttpApiData ConversationType where
  toQueryParam TypePublicChannel = "public_channel"
  toQueryParam TypePrivateChannel = "private_channel"
  toQueryParam TypeMpim = "mpim"
  toQueryParam TypeIm = "im"

instance ToHttpApiData [ConversationType] where
  toQueryParam types = pack . intercalate "," $
    (unpack . toQueryParam) <$> types

-- |
--
--

data Purpose =
  Purpose
    { purposeValue :: Text
    , purposeCreator :: Text
    , purposeLastSet :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "purpose") ''Purpose)

-- |
--
--

data Topic =
  Topic
    { topicValue :: Text
    , topicCreator :: Text
    , topicLastSet :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "topic") ''Topic)

-- |
--
--
data Im =
  Im
    { imId :: Text
    , imUser :: Text
    , imIsUserDeleted :: Bool
    , imCreated :: POSIXTime
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "im") ''Im)

-- |
--
--
data Mpim =
  Mpim
    { mpimId :: Text
    , mpimName :: Text
    , mpimCreated :: POSIXTime
    , mpimCreator :: UserId
    , mpimIsPrivate :: Bool
    , mpimTopic :: Topic
    , mpimPurpose :: Purpose
    }
  deriving (Eq, Show)

$(deriveFromJSON (jsonOpts "mpim") ''Mpim)

-- |
--
--
data Channel =
  Channel
    { channelId :: Text
    , channelName :: Text
    , channelCreated :: Integer
    , channelCreator :: UserId
    , channelIsPrivate :: Bool
    , channelIsArchived :: Bool
    , channelIsGeneral :: Bool
    , channelLastRead :: Maybe Text
    , channelLatest :: Maybe Text
    , channelUnreadCount :: Maybe Integer
    , channelUnreadCountDisplay :: Maybe Integer
    , channelTopic :: Topic
    , channelPurpose :: Purpose
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "channel") ''Channel)

-- |
--
--
data Conversation
  = ConversationChannel Channel
  | ConversationIm Im
  | ConversationMpim Mpim
  deriving (Eq, Show)

instance FromJSON Conversation where
  parseJSON = withObject "conversation" $ \c -> do
    conversationType <- conversationTypeFromJSON c
    case conversationType of
      TypePublicChannel  -> ConversationChannel <$> (parseJSON $ Object c)
      TypePrivateChannel -> ConversationChannel <$> (parseJSON $ Object c)
      TypeIm   -> ConversationIm   <$> (parseJSON $ Object c)
      TypeMpim -> ConversationMpim <$> (parseJSON $ Object c)

conversationTypeFromJSON :: Object -> Parser ConversationType
conversationTypeFromJSON c = do
  isIm <- (c .: "is_im" :: Parser Bool) <|> (return False)
  isMpim <- (c .: "is_mpim" :: Parser Bool) <|> (return False)
  isChannel <- (c .: "is_channel" :: Parser Bool) <|> (return False)
  isGroup <- (c .: "is_group" :: Parser Bool) <|> (return False)
  -- Instant Message
  if isIm == True
  then return TypeIm
  -- Multi Party Instant Message
  else if isMpim == True
  then return TypeMpim
  -- Group -> Private Channel
  else if isGroup == True
  then return TypePrivateChannel
  -- Channel
  else if isChannel == True
  then do
    isPrivate <- c .: "is_private" :: Parser Bool
    case isPrivate of
      True -> return TypePrivateChannel
      False -> return TypePublicChannel
  else do
    fail "Could not decode conversation type."

-- |
--
--
data ListReq =
  ListReq
    { listReqExcludeArchived :: Maybe Bool
    , listReqTypes :: Maybe [ConversationType]
    }
  deriving (Eq, Generic, Show)

instance ToForm ListReq where
  toForm =
    genericToForm (formOpts "listReq")

-- |
--
--
data ListRsp =
  ListRsp
    { listRspChannels :: [Conversation]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
