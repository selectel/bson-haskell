-- | A BSON document is a JSON-like object with a standard binary encoding
-- defined at bsonspec.org. This implements version 1.0 of that spec.
--
-- Use the GHC language extension /OverloadedStrings/ to automatically convert
-- String literals to Text

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Bson (
	-- * Document
	Document, (!?), look, lookup, valueAt, at, include, exclude, merge,
	-- * Field
	Field(..), (=:), (=?),
	Label,
	-- * Value
	Value(..), Val(..), fval, cast, typed, typeOfVal,
	-- * Special Bson value types
	Binary(..), Function(..), UUID(..), MD5(..), UserDefined(..),
	Regex(..), Javascript(..), Symbol(..), MongoStamp(..), MinMaxKey(..),
	-- ** ObjectId
	ObjectId(..), timestamp, genObjectId
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM)
import Data.Bits (shift, (.|.))
import Data.Int (Int32, Int64)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.List (find, findIndex)
import Data.Maybe (maybeToList, mapMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime,
                              utcTimeToPOSIXSeconds, getPOSIXTime)
import Data.Time.Format ()  -- for Show and Read instances of UTCTime
import Data.Typeable hiding (cast)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric (readHex, showHex)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read(..))

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as R (lift, readS_to_Prec)

import Control.Monad.Identity (runIdentity)
import Network.BSD (getHostName)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Crypto.Hash.MD5 as MD5

getProcessID :: IO Int
-- ^ Get the current process id.
getProcessID = c_getpid

foreign import ccall unsafe "getpid"
   c_getpid :: IO Int

roundTo :: (RealFrac a) => a -> a -> a
-- ^ Round second number to nearest multiple of first number. Eg: roundTo (1/1000) 0.12345 = 0.123
roundTo mult n = fromIntegral (round (n / mult)) * mult

showHexLen :: (Show n, Integral n) => Int -> n -> ShowS
-- ^ showHex of n padded with leading zeros if necessary to fill d digits
showHexLen d n = showString (replicate (d - sigDigits n) '0') . showHex n  where
	sigDigits 0 = 1
	sigDigits n' = truncate (logBase 16 $ fromIntegral n') + 1

-- * Document

type Document = [Field]
-- ^ A BSON document is a list of 'Field's

-- | Recursively lookup a nested field in a Document.
(!?) :: Val a => Document -> Label -> Maybe a
doc !? label = foldM (flip lookup) doc (init chunks) >>= lookup (last chunks)
  where
    chunks = T.split (== '.') label

look :: (Monad m) => Label -> Document -> m Value
-- ^ Value of field in document, or fail (Nothing) if field not found
look k doc = maybe notFound (return . value) (find ((k ==) . label) doc) where
	notFound = fail $ "expected " ++ show k ++ " in " ++ show doc

lookup :: (Val v, Monad m) => Label -> Document -> m v
-- ^ Lookup value of field in document and cast to expected type. Fail (Nothing) if field not found or value not of expected type.
lookup k doc = cast =<< look k doc

valueAt :: Label -> Document -> Value
-- ^ Value of field in document. Error if missing.
valueAt k = runIdentity . look k

at :: (Val v) => Label -> Document -> v
-- ^ Typed value of field in document. Error if missing or wrong type.
at k doc = result
	where
		result = maybe err id (lookup k doc)
		err = error $ "expected (" ++ show k ++ " :: " ++ show (typeOf result) ++ ") in " ++ show doc

include :: [Label] -> Document -> Document
-- ^ Only include fields of document in label list
include keys doc = mapMaybe (\k -> find ((k ==) . label) doc) keys

exclude :: [Label] -> Document -> Document
-- ^ Exclude fields from document in label list
exclude keys doc = filter (\(k := _) -> notElem k keys) doc

merge :: Document -> Document -> Document
-- ^ Merge documents with preference given to first one when both have the same label. I.e. for every (k := v) in first argument, if k exists in second argument then replace its value with v, otherwise add (k := v) to second argument.
merge es doc = foldl f doc es where
	f doc (k := v) = case findIndex ((k ==) . label) doc of
		Nothing -> doc ++ [k := v]
		Just i -> let (x, _ : y) = splitAt i doc in x ++ [k := v] ++ y

-- * Field

infix 0 :=, =:, =?

data Field = (:=) {label :: !Label, value :: Value}  deriving (Typeable, Eq, Ord)
-- ^ A BSON field is a named value, where the name (label) is a string and the value is a BSON 'Value'

(=:) :: (Val v) => Label -> v -> Field
-- ^ Field with given label and typed value
k =: v = k := val v

(=?) :: (Val a) => Label -> Maybe a -> Document
-- ^ If Just value then return one field document, otherwise return empty document
k =? ma = maybeToList (fmap (k =:) ma)

instance Show Field where
	showsPrec d (k := v) = showParen (d > 0) $ showString (' ' : T.unpack k) . showString ": " . showsPrec 1 v

type Label = Text
-- ^ The name of a BSON field

-- * Value

-- | A BSON value is one of the following types of values
data Value =
	Float Double |
	String Text |
	Doc Document |
	Array [Value] |
	Bin Binary |
	Fun Function |
	Uuid UUID |
	Md5 MD5 |
	UserDef UserDefined |
	ObjId ObjectId |
	Bool Bool |
	UTC UTCTime |
	Null |
	RegEx Regex |
	JavaScr Javascript |
	Sym Symbol |
	Int32 Int32 |
	Int64 Int64 |
	Stamp MongoStamp |
	MinMax MinMaxKey
	deriving (Typeable, Eq, Ord)

instance Show Value where
	showsPrec d v = fval (showsPrec d) v

fval :: (forall a . (Val a) => a -> b) -> Value -> b
-- ^ Apply generic function to typed value
fval f v = case v of
	Float x -> f x
	String x -> f x
	Doc x -> f x
	Array x -> f x
	Bin x -> f x
	Fun x -> f x
	Uuid x -> f x
	Md5 x -> f x
	UserDef x -> f x
	ObjId x -> f x
	Bool x -> f x
	UTC x -> f x
	Null -> f (Nothing :: Maybe Value)
	RegEx x -> f x
	JavaScr x -> f x
	Sym x -> f x
	Int32 x -> f x
	Int64 x -> f x
	Stamp x -> f x
	MinMax x -> f x

-- * Value conversion

cast :: (Val a, Monad m) => Value -> m a
-- ^ Convert Value to expected type, or fail (Nothing) if not of that type
cast v = maybe notType return castingResult
	where
		castingResult = cast' v
		unMaybe :: Maybe a -> a
		unMaybe = undefined
		notType = fail $ "expected " ++ show (typeOf $ unMaybe castingResult) ++ ": " ++ show v

typed :: (Val a) => Value -> a
-- ^ Convert Value to expected type. Error if not that type.
typed = runIdentity . cast

typeOfVal :: Value -> TypeRep
-- ^ Type of typed value
typeOfVal = fval typeOf

-- ** conversion class

-- | Haskell types of this class correspond to BSON value types
class (Typeable a, Show a, Eq a) => Val a where

	val :: a -> Value

	valList :: [a] -> Value
	valList = Array . map val

	valMaybe :: Maybe a -> Value
	valMaybe = maybe Null val

	cast' :: Value -> Maybe a

	cast'List :: Value -> Maybe [a]
	cast'List (Array x) = mapM cast x
	cast'List _ = Nothing

	cast'Maybe :: Value -> Maybe (Maybe a)
	cast'Maybe Null = Just Nothing
	cast'Maybe v = fmap Just (cast' v)

instance Val Double where
	val = Float
	cast' (Float x) = Just x
	cast' (Int32 x) = Just (fromIntegral x)
	cast' (Int64 x) = Just (fromIntegral x)
	cast' _ = Nothing

instance Val Float where
	val = Float . realToFrac
	cast' (Float x) = Just (realToFrac x)
	cast' (Int32 x) = Just (fromIntegral x)
	cast' (Int64 x) = Just (fromIntegral x)
	cast' _ = Nothing

instance Val Text where
	val = String
	cast' (String x) = Just x
	cast' (Sym (Symbol x)) = Just x
	cast' _ = Nothing

instance Val Char where
	val x = valList [x]
	valList = String . T.pack
	cast' v = cast'List v >>= safeHead
		where
			safeHead list = case list of
				x : _ -> Just x
				_ -> Nothing
	cast'List (String x) = Just $ T.unpack x
	cast'List (Sym (Symbol x)) = Just $ T.unpack x
	cast'List _ = Nothing

instance Val Field where
	val x = valList [x]
	valList = Doc
	cast' _ = Nothing
	cast'List v = case v of
		Doc x -> Just x
		_ -> Nothing

instance Val Value where
	val = id
	cast' = Just

instance (Val a) => Val [a] where
	val = valList
	cast' = cast'List

instance Val Binary where
	val = Bin
	cast' (Bin x) = Just x
	cast' _ = Nothing

instance Val Function where
	val = Fun
	cast' (Fun x) = Just x
	cast' _ = Nothing

instance Val UUID where
	val = Uuid
	cast' (Uuid x) = Just x
	cast' _ = Nothing

instance Val MD5 where
	val = Md5
	cast' (Md5 x) = Just x
	cast' _ = Nothing

instance Val UserDefined where
	val = UserDef
	cast' (UserDef x) = Just x
	cast' _ = Nothing

instance Val ObjectId where
	val = ObjId
	cast' (ObjId x) = Just x
	cast' _ = Nothing

instance Val Bool where
	val = Bool
	cast' (Bool x) = Just x
	cast' _ = Nothing

instance Val UTCTime where
	val = UTC
	cast' (UTC x) = Just x
	cast' _ = Nothing

instance Val POSIXTime where
	val = UTC . posixSecondsToUTCTime . roundTo (1/1000)
	cast' (UTC x) = Just (utcTimeToPOSIXSeconds x)
	cast' _ = Nothing

instance (Val a) => Val (Maybe a) where
	val = valMaybe
	cast' = cast'Maybe

instance Val Regex where
	val = RegEx
	cast' (RegEx x) = Just x
	cast' _ = Nothing

instance Val Javascript where
	val = JavaScr
	cast' (JavaScr x) = Just x
	cast' _ = Nothing

instance Val Symbol where
	val = Sym
	cast' (Sym x) = Just x
	cast' (String x) = Just (Symbol x)
	cast' _ = Nothing

instance Val Int32 where
	val = Int32
	cast' (Int32 x) = Just x
	cast' (Int64 x) = fitInt x
	cast' (Float x) = Just (round x)
	cast' _ = Nothing

instance Val Int64 where
	val = Int64
	cast' (Int64 x) = Just x
	cast' (Int32 x) = Just (fromIntegral x)
	cast' (Float x) = Just (round x)
	cast' _ = Nothing

instance Val Int where
	val n = maybe (Int64 $ fromIntegral n) Int32 (fitInt n)
	cast' (Int32 x) = Just (fromIntegral x)
	cast' (Int64 x) = Just (fromEnum x)
	cast' (Float x) = Just (round x)
	cast' _ = Nothing

instance Val Integer where
	val n = maybe (maybe err Int64 $ fitInt n) Int32 (fitInt n)  where
		err = error $ show n ++ " is too large for Bson Int Value"
	cast' (Int32 x) = Just (fromIntegral x)
	cast' (Int64 x) = Just (fromIntegral x)
	cast' (Float x) = Just (round x)
	cast' _ = Nothing

instance Val MongoStamp where
	val = Stamp
	cast' (Stamp x) = Just x
	cast' _ = Nothing

instance Val MinMaxKey where
	val = MinMax
	cast' (MinMax x) = Just x
	cast' _ = Nothing

fitInt :: (Integral n, Integral m, Bounded m) => n -> Maybe m
-- ^ If number fits in type m then cast to m, otherwise Nothing
fitInt n =
	if fromIntegral (minBound `asTypeOf` result) <= n && n <= fromIntegral (maxBound `asTypeOf` result)
		then Just result
		else Nothing
	where
		result = fromIntegral n

-- * Haskell types corresponding to special Bson value types

-- ** Binary types

newtype Binary = Binary S.ByteString  deriving (Typeable, Show, Read, Eq, Ord)

newtype Function = Function S.ByteString  deriving (Typeable, Show, Read, Eq, Ord)

newtype UUID = UUID S.ByteString  deriving (Typeable, Show, Read, Eq, Ord)

newtype MD5 = MD5 S.ByteString  deriving (Typeable, Show, Read, Eq, Ord)

newtype UserDefined = UserDefined S.ByteString  deriving (Typeable, Show, Read, Eq, Ord)

-- ** Regex

data Regex = Regex Text Text  deriving (Typeable, Show, Read, Eq, Ord)
-- ^ The first string is the regex pattern, the second is the regex options string. Options are identified by characters, which must be listed in alphabetical order. Valid options are *i* for case insensitive matching, *m* for multiline matching, *x* for verbose mode, *l* to make \\w, \\W, etc. locale dependent, *s* for dotall mode (\".\" matches everything), and *u* to make \\w, \\W, etc. match unicode.

-- ** Javascript

data Javascript = Javascript Document Text deriving (Typeable, Show, Eq, Ord)
-- ^ Javascript code with possibly empty environment mapping variables to values that the code may reference

-- ** Symbol

newtype Symbol = Symbol Text  deriving (Typeable, Show, Read, Eq, Ord)

-- ** MongoStamp

newtype MongoStamp = MongoStamp Int64  deriving (Typeable, Show, Read, Eq, Ord)

-- ** MinMax

data MinMaxKey = MinKey | MaxKey  deriving (Typeable, Show, Read, Eq, Ord)

-- ** ObjectId

data ObjectId = Oid Word32 Word64  deriving (Typeable, Eq, Ord)
-- ^ A BSON ObjectID is a 12-byte value consisting of a 4-byte timestamp (seconds since epoch), a 3-byte machine id, a 2-byte process id, and a 3-byte counter. Note that the timestamp and counter fields must be stored big endian unlike the rest of BSON. This is because they are compared byte-by-byte and we want to ensure a mostly increasing order.

instance Show ObjectId where
	showsPrec _ (Oid x y) = showHexLen 8 x . showHexLen 16 y

instance Read ObjectId where
	readPrec = do
		[(x, "")] <- readHex <$> R.lift (R.count 8 R.get)
		y <- R.readS_to_Prec $ const readHex
		return (Oid x y)

timestamp :: ObjectId -> UTCTime
-- ^ Time when objectId was created
timestamp (Oid time _) = posixSecondsToUTCTime (fromIntegral time)

genObjectId :: IO ObjectId
-- ^ Create a fresh ObjectId
genObjectId = do
	time <- truncate <$> getPOSIXTime
	pid <- fromIntegral <$> getProcessID
	inc <- nextCount
	return $ Oid time (composite machineId pid inc)
 where
	machineId :: Word24
	machineId = unsafePerformIO (makeWord24 . S.unpack . S.take 3 . MD5.hash . SC.pack <$> getHostName)
 	{-# NOINLINE machineId #-}
 	counter :: IORef Word24
 	counter = unsafePerformIO (newIORef 0)
 	{-# NOINLINE counter #-}
 	nextCount :: IO Word24
 	nextCount = atomicModifyIORef counter $ \n -> (wrap24 (n + 1), n)

composite :: Word24 -> Word16 -> Word24 -> Word64
composite mid pid inc = fromIntegral mid `shift` 40 .|. fromIntegral pid `shift` 24 .|. fromIntegral inc

type Word24 = Word32
-- ^ low 3 bytes only, high byte must be zero

wrap24 :: Word24 -> Word24
wrap24 n = n `mod` 0x1000000

makeWord24 :: [Word8] -> Word24
-- ^ Put last 3 bytes into a Word24. Expected to be called on very short list
makeWord24 = foldl (\a b -> a `shift` 8 .|. fromIntegral b) 0

{- Authors: Tony Hannan <tony@10gen.com>
   Copyright 2010 10gen Inc.
   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License. -}
