module PlutusTx (
    BuiltinData,
    ToData (..),
    FromData (..),
    UnsafeFromData (..),
) where

import Prelude

data BuiltinData = BuiltinData

class ToData a where
    toBuiltinData :: a -> BuiltinData

instance ToData Integer where
    toBuiltinData = const BuiltinData

instance ToData Bool where
    toBuiltinData = const BuiltinData

instance ToData (a, b) where
    toBuiltinData = const BuiltinData

instance ToData (a, b, c) where
    toBuiltinData = const BuiltinData

instance ToData (a, b, c, d) where
    toBuiltinData = const BuiltinData

instance ToData [a] where
    toBuiltinData = const BuiltinData

class FromData a where
    fromBuiltinData :: BuiltinData -> Maybe a

instance FromData Integer where
    fromBuiltinData = Just . const 0

instance FromData Bool where
    fromBuiltinData = Just . const True

instance (FromData a, FromData b) => FromData (a, b) where
    fromBuiltinData dat = do
        a <- fromBuiltinData dat
        b <- fromBuiltinData dat
        Just (a, b)

instance (FromData a, FromData b, FromData c) => FromData (a, b, c) where
    fromBuiltinData dat = do
        a <- fromBuiltinData dat
        b <- fromBuiltinData dat
        c <- fromBuiltinData dat
        Just (a, b, c)

instance (FromData a, FromData b, FromData c, FromData d) => FromData (a, b, c, d) where
    fromBuiltinData dat = do
        a <- fromBuiltinData dat
        b <- fromBuiltinData dat
        c <- fromBuiltinData dat
        d <- fromBuiltinData dat
        Just (a, b, c, d)

instance FromData [a] where
    fromBuiltinData = Just . const []

class UnsafeFromData a where
    unsafeFromBuiltinData :: BuiltinData -> a

instance UnsafeFromData Integer where
    unsafeFromBuiltinData = const 0

instance UnsafeFromData Bool where
    unsafeFromBuiltinData = const True

instance (UnsafeFromData a, UnsafeFromData b) => UnsafeFromData (a, b) where
    unsafeFromBuiltinData dat = (unsafeFromBuiltinData dat, unsafeFromBuiltinData dat)

instance (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c) => UnsafeFromData (a, b, c) where
    unsafeFromBuiltinData dat = (unsafeFromBuiltinData dat, unsafeFromBuiltinData dat, unsafeFromBuiltinData dat)

instance (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c, UnsafeFromData d) => UnsafeFromData (a, b, c, d) where
    unsafeFromBuiltinData dat = (unsafeFromBuiltinData dat, unsafeFromBuiltinData dat, unsafeFromBuiltinData dat, unsafeFromBuiltinData dat)

instance UnsafeFromData [a] where
    unsafeFromBuiltinData = const []