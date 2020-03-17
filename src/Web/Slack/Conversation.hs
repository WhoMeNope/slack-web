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
  = PublicChannel
  | PrivateChannel
  | Mpim
  | Im
  deriving (Eq, Generic, Show)

instance ToHttpApiData ConversationType where
  toQueryParam PublicChannel = "public_channel"
  toQueryParam PrivateChannel = "private_channel"
  toQueryParam Mpim = "mpim"
  toQueryParam Im = "im"

instance ToHttpApiData [ConversationType] where
  toQueryParam types = pack . intercalate "," $
    (unpack . toQueryParam) <$> types

-- |
--
--
data Conversation =
  Conversation
    { conversationId :: Text
    , conversationName :: Text
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversation") ''Conversation)

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
