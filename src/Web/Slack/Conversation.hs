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

import Data.Aeson.TH
import Data.Aeson.Types
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Web.FormUrlEncoded
import Web.Internal.HttpApiData

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
data Im =
  Im
    { imId :: Text
    , imUser :: Text
    }
  deriving (Eq, Show)

-- |
--
--
data Mpim =
  Mpim
    { mpimId :: Text
    , mpimName :: Text
    }
  deriving (Eq, Show)

-- |
--
--
data Channel =
  Channel
    { channelId :: Text
    , channelName :: Text
    , channelPrivate :: Bool
    }
  deriving (Eq, Show)

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
    return $ case conversationType of
      TypePublicChannel ->
        ConversationChannel $ Channel "" "" False
      TypePrivateChannel ->
        ConversationChannel $ Channel "" "" True
      TypeIm ->
        ConversationIm $ Im "" ""
      TypeMpim ->
        ConversationMpim $ Mpim "" ""

conversationTypeFromJSON :: Object -> Parser ConversationType
conversationTypeFromJSON c = do
  isIm <- c .: "is_im" :: Parser Bool
  isMpim <- c .: "is_mpim" :: Parser Bool
  isChannel <- c .: "is_channel" :: Parser Bool
  isGroup <- c .: "is_group" :: Parser Bool
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
