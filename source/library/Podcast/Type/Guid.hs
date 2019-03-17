module Podcast.Type.Guid
  ( Guid
  , fromString
  , toString
  )
where

import qualified Data.UUID as Uuid

newtype Guid
  = Guid Uuid.UUID
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Guid
fromString string = case Uuid.fromString string of
  Nothing -> Left ("invalid Guid: " ++ show string)
  Just uuid -> Right (Guid uuid)

toString :: Guid -> String
toString (Guid uuid) = Uuid.toString uuid
