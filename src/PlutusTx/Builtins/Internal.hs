{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
-- This ensures that we don't put *anything* about these functions into the interface
-- file, otherwise GHC can be clever about the ones that are always error, even though
-- they're NOINLINE!
{-# OPTIONS_GHC -O0 #-}
-- | This module contains the special Haskell names that are used to map to builtin types or functions
-- in Plutus Core.
--
-- Most users should not use this module directly, but rather use 'PlutusTx.Builtins'.
module PlutusTx.Builtins.Internal where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import PlutusTx.Utils (mustBeReplaced)
import Prettyprinter (Pretty (..), viaShow)
import Text.Hex (encodeHex, Text, decodeHex)

{-
We do not use qualified import because the whole module contains off-chain code
which is replaced later with on-chain implementations by the plutus-tx-plugin.
-}
import Prelude as Haskell
import Data.Aeson (ToJSON (..), FromJSON (..), ToJSONKey, FromJSONKey)
import Data.ByteString (ByteString)

{- Note [Builtin name definitions]
The builtins here have definitions so they can be used in off-chain code too.

However they *must* be replaced by the compiler when used in Plutus Tx code, so
in particular they must *not* be inlined, otherwise we can't spot them to replace
them.
-}

{- Note [Delaying error]
The Plutus Core 'error' builtin is of type 'forall a . a', but the
one we expose here is of type 'forall a . () -> a'.

This is because it's hard to get the evaluation order right with
the non-delayed version - it's easy to end up with it getting thrown
unconditionally, or before some other effect (e.g. tracing). On the other
hand, it's much easier to work with the delayed version.

But why not just define that in the library? i.e.

    error = \_ -> Builtins.error

The answer is that GHC is eager to inline and reduce this function, which
does the Wrong Thing. We can't stop GHC doing this (at the moment), but
for most of our functions it's not a *semantic* problem. Here, however,
it is a problem. So we just expose the delayed version as the builtin.
-}

{-# NOINLINE error #-}
error :: BuiltinUnit -> a
error = mustBeReplaced "error"

{-
BOOL
-}

newtype BuiltinBool = BuiltinBool Bool

{-# NOINLINE true #-}
true :: BuiltinBool
true = coerce True

{-# NOINLINE false #-}
false :: BuiltinBool
false = coerce False

{-# NOINLINE ifThenElse #-}
ifThenElse :: BuiltinBool -> a -> a -> a
ifThenElse (BuiltinBool b) x y = if b then x else y

{-
UNIT
-}

newtype BuiltinUnit = BuiltinUnit ()

{-# NOINLINE unitval #-}
unitval :: BuiltinUnit
unitval = BuiltinUnit ()

{-# NOINLINE chooseUnit #-}
chooseUnit :: BuiltinUnit -> a -> a
chooseUnit (BuiltinUnit ()) a = a

{-
INTEGER
-}

type BuiltinInteger = Integer

{-# NOINLINE addInteger #-}
addInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
addInteger = coerce ((+) @Integer)

{-# NOINLINE subtractInteger #-}
subtractInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
subtractInteger = coerce ((-) @Integer)

{-# NOINLINE multiplyInteger #-}
multiplyInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
multiplyInteger = coerce ((*) @Integer)

{-# NOINLINE divideInteger #-}
divideInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
divideInteger = coerce (div @Integer)

{-# NOINLINE modInteger #-}
modInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
modInteger = coerce (mod @Integer)

{-# NOINLINE quotientInteger #-}
quotientInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
quotientInteger = coerce (quot @Integer)

{-# NOINLINE remainderInteger #-}
remainderInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
remainderInteger = coerce (rem @Integer)

{-# NOINLINE lessThanInteger #-}
lessThanInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinBool
lessThanInteger = coerce ((<) @Integer)

{-# NOINLINE lessThanEqualsInteger #-}
lessThanEqualsInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinBool
lessThanEqualsInteger = coerce ((<=) @Integer)

{-# NOINLINE equalsInteger #-}
equalsInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinBool
equalsInteger = coerce ((==) @Integer)

{-
BYTESTRING
-}

-- See Note [Opaque builtin types]
-- | An opaque type representing Plutus Core ByteStrings.
data BuiltinByteString = BuiltinByteString BS.ByteString

instance Haskell.Show BuiltinByteString where
    show (BuiltinByteString bs) = show . encodeHex $ bs
instance Haskell.Eq BuiltinByteString where
    (==) (BuiltinByteString bs) (BuiltinByteString bs') = (==) bs bs'
instance Haskell.Ord BuiltinByteString where
    compare (BuiltinByteString bs) (BuiltinByteString bs') = compare bs bs'
instance Haskell.Semigroup BuiltinByteString where
    (<>) (BuiltinByteString bs) (BuiltinByteString bs') = BuiltinByteString $ (<>) bs bs'
instance Haskell.Monoid BuiltinByteString where
    mempty = BuiltinByteString mempty
instance ToJSON BuiltinByteString where
    toJSON (BuiltinByteString bs) = toJSON $ encodeHex bs
instance FromJSON BuiltinByteString where
    parseJSON v = do
        bs <- (decodeHex :: Text -> Maybe ByteString) <$> parseJSON v
        maybe (fail "A valid hex string is expected!") (return . BuiltinByteString) bs
instance ToJSONKey BuiltinByteString where
instance FromJSONKey BuiltinByteString where
instance BA.ByteArrayAccess BuiltinByteString where
    length (BuiltinByteString bs) = BA.length bs
    withByteArray (BuiltinByteString bs) = BA.withByteArray bs
instance BA.ByteArray BuiltinByteString where
    allocRet i p = fmap (fmap BuiltinByteString) $ BA.allocRet i p

instance Pretty BuiltinByteString where
    pretty = viaShow

{-# NOINLINE appendByteString #-}
appendByteString :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString
appendByteString (BuiltinByteString b1) (BuiltinByteString b2) = BuiltinByteString $ BS.append b1 b2

{-# NOINLINE consByteString #-}
consByteString :: BuiltinInteger -> BuiltinByteString -> BuiltinByteString
consByteString n (BuiltinByteString b) = BuiltinByteString $ BS.cons (fromIntegral n) b

{-# NOINLINE sliceByteString #-}
sliceByteString :: BuiltinInteger -> BuiltinInteger -> BuiltinByteString -> BuiltinByteString
sliceByteString start n (BuiltinByteString b) = BuiltinByteString $ BS.take (fromIntegral n) (BS.drop (fromIntegral start) b)

{-# NOINLINE lengthOfByteString #-}
lengthOfByteString :: BuiltinByteString -> BuiltinInteger
lengthOfByteString (BuiltinByteString b) = toInteger $ BS.length b

{-# NOINLINE indexByteString #-}
indexByteString :: BuiltinByteString -> BuiltinInteger -> BuiltinInteger
indexByteString (BuiltinByteString b) i = toInteger $ BS.index b (fromInteger i)

{-# NOINLINE emptyByteString #-}
emptyByteString :: BuiltinByteString
emptyByteString = BuiltinByteString BS.empty

{-# NOINLINE equalsByteString #-}
equalsByteString :: BuiltinByteString -> BuiltinByteString -> BuiltinBool
equalsByteString (BuiltinByteString b1) (BuiltinByteString b2) = BuiltinBool $ b1 == b2

{-# NOINLINE lessThanByteString #-}
lessThanByteString :: BuiltinByteString -> BuiltinByteString -> BuiltinBool
lessThanByteString (BuiltinByteString b1) (BuiltinByteString b2) = BuiltinBool $ b1 < b2

{-# NOINLINE lessThanEqualsByteString #-}
lessThanEqualsByteString :: BuiltinByteString -> BuiltinByteString -> BuiltinBool
lessThanEqualsByteString (BuiltinByteString b1) (BuiltinByteString b2) = BuiltinBool $ b1 <= b2

{-
PAIR
-}

newtype BuiltinPair a b = BuiltinPair (a, b)
    deriving newtype (Haskell.Show, Haskell.Eq, Haskell.Ord)

{-# NOINLINE fst #-}
fst :: BuiltinPair a b -> a
fst (BuiltinPair (a, _)) = a

{-# NOINLINE snd #-}
snd :: BuiltinPair a b -> b
snd (BuiltinPair (_, b)) = b


{-
LIST
-}

newtype BuiltinList a = BuiltinList [a]
    deriving newtype (Haskell.Show, Haskell.Eq, Haskell.Ord)

{-# NOINLINE null #-}
null :: BuiltinList a -> BuiltinBool
null (BuiltinList (_:_)) = coerce False
null (BuiltinList [])    = coerce True

{-# NOINLINE head #-}
head :: BuiltinList a -> a
head (BuiltinList (x:_)) = x
head (BuiltinList [])    = Haskell.error "empty list"

{-# NOINLINE tail #-}
tail :: BuiltinList a -> BuiltinList a
tail (BuiltinList (_:xs)) = coerce xs
tail (BuiltinList [])     = Haskell.error "empty list"

{-# NOINLINE chooseList #-}
chooseList :: BuiltinList a -> b -> b-> b
chooseList (BuiltinList [])    b1 _ = b1
chooseList (BuiltinList (_:_)) _ b2 = b2



{-# NOINLINE mkCons #-}
mkCons :: a -> BuiltinList a -> BuiltinList a
mkCons a (BuiltinList as) = BuiltinList (a:as)


