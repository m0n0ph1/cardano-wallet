{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- A mock metadata-server for testing, metadata requests. Created using the
-- metadata-server Haskell source code as a reference.

module Cardano.Wallet.TokenMetadata.MockServer
    ( withMetadataServer
    , queryServerStatic

    -- * Helpers
    , assetIdFromSubject
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetLogo (..)
    , AssetURL (..)
    , AssetUnit (..)
    , TokenName (..)
    , TokenPolicyId (..)
    )
import Cardano.Wallet.TokenMetadata
    ( BatchRequest (..)
    , BatchResponse (..)
    , Property (..)
    , PropertyValue
    , Signature (..)
    , Subject (..)
    , Subject
    , SubjectProperties (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), eitherDecodeFileStrict, object, (.=) )
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertToBase )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Network.URI
    ( parseURI )
import Network.Wai.Handler.Warp
    ( withApplication )
import Servant.API
    ( (:>), JSON, Post, ReqBody )
import Servant.Server
    ( Handler (..), Server, serve )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                              Mock metadata-server
-------------------------------------------------------------------------------}

-- | The batch query API, excerpted from
-- @metadata-server/metadata-lib/src/Cardano/Metadata/Server/API.hs@.
type MetadataQueryApi = "metadata" :> "query"
    :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse

-- | Start a metadata server.
--
-- To be used with @queryServerStatic@.
withMetadataServer
    :: (Server MetadataQueryApi)
    -> (TokenMetadataServer -> IO a)
    -> IO a
withMetadataServer mkServer action = withApplication app (action . mkUrl)
  where
    app = pure $ serve (Proxy @MetadataQueryApi) mkServer
    mkUrl port = TokenMetadataServer
        $ fromMaybe (error "withMetadataServer: bad uri")
        $ parseURI
        $ "http://localhost:" ++ show port ++ "/"

-- | Serve a json file.
--
-- Will filter the json and only serve metadata for the requested subjects.
queryServerStatic :: FilePath -> BatchRequest -> Handler BatchResponse
queryServerStatic golden (BatchRequest subs _) = do
    BatchResponse mds <- either (error . show) id <$> liftIO (eitherDecodeFileStrict golden)
    let m = HM.fromList [(view #subject md, md) | md <- mds]
    pure $ BatchResponse . mapMaybe (`HM.lookup` m) $ subs

-- | The reverse of subjectToAssetId
assetIdFromSubject :: Subject -> AssetId
assetIdFromSubject =
    mk . BS.splitAt 32 . unsafeFromHex . T.encodeUtf8 . unSubject
  where
    mk (p, n) = AssetId (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)

{-------------------------------------------------------------------------------
                              JSON orphans
-------------------------------------------------------------------------------}

instance FromJSON BatchRequest where

instance ToJSON SubjectProperties where
   toJSON (SubjectProperties s o (n,d,a,u,l,t)) = object
       [ "subject" .= s
       , "owner" .= o
       , "name" .= n
       , "description" .= d
       , "acronym" .= a
       , "url" .= u
       , "logo" .= l
       , "unit" .= t
       ]

instance ToJSON (PropertyValue name) => ToJSON (Property name) where
    toJSON (Property v s) = object [ "value" .= v, "anSignatures" .= s ]

instance ToJSON Signature where
    toJSON (Signature s k) = object
        [ "signature" .= hex s
        , "publicKey" .= hex k
        ]
      where
        hex = T.decodeLatin1 . convertToBase Base16

instance ToJSON BatchResponse where
    toJSON (BatchResponse subs) = object
        [ "subjects" .= subs
        ]

instance ToJSON AssetLogo where
    toJSON = toJSON . B8.unpack . convertToBase Base64 . unAssetLogo

instance ToJSON AssetUnit where
    toJSON AssetUnit{name,decimals} = object
        [ "name" .= name
        , "decimals" .= decimals
        ]

instance ToJSON AssetURL where
    toJSON = toJSON . show . unAssetURL
