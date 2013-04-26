-- | Standard binary encoding of BSON documents, version 1.0. See bsonspec.org

module Data.Bson.Binary (
	putDocument, getDocument,
	putDocumentWithSize, getDocumentWithSize,
	putDouble, getDouble,
	putInt32, getInt32,
	putInt64, getInt64,
	putCString, getCString
) where

import Prelude hiding (length, concat)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Binary.Get (Get, runGet, getWord8, getWord32be, getWord64be,
                        getWord32le, getWord64le, getLazyByteStringNul,
                        getLazyByteString, getByteString, lookAhead)
import Data.Binary.Put (Put, PutM, runPut, putWord8, putWord32le, putWord64le,
                        putWord32be, putWord64be, putLazyByteString,
                        putByteString)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word8)

import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Bson (Document, Value(..), ObjectId(..), MongoStamp(..), Symbol(..),
                  Javascript(..), UserDefined(..), Regex(..), MinMaxKey(..),
                  Binary(..), UUID(..), Field(..), MD5(..), Function(..))

putField :: Field -> Put
-- ^ Write binary representation of element
putField (k := v) = case v of
	Float x -> putTL 0x01 >> putDouble x
	String x -> putTL 0x02 >> putString x
	Doc x -> putTL 0x03 >> putDocument x
	Array x -> putTL 0x04 >> putArray x
	Bin (Binary x) -> putTL 0x05 >> putBinary 0x00 x
	Fun (Function x) -> putTL 0x05 >> putBinary 0x01 x
	Uuid (UUID x) -> putTL 0x05 >> putBinary 0x04 x
	Md5 (MD5 x) -> putTL 0x05 >> putBinary 0x05 x
	UserDef (UserDefined x) -> putTL 0x05 >> putBinary 0x80 x
	ObjId x -> putTL 0x07 >> putObjectId x
	Bool x -> putTL 0x08 >> putBool x
	UTC x -> putTL 0x09 >> putUTC x
	Null -> putTL 0x0A
	RegEx x -> putTL 0x0B >> putRegex x
	JavaScr (Javascript env code) -> if null env
		then putTL 0x0D >> putString code
		else putTL 0x0F >> putClosure code env
	Sym x -> putTL 0x0E >> putSymbol x
	Int32 x -> putTL 0x10 >> putInt32 x
	Int64 x -> putTL 0x12 >> putInt64 x
	Stamp x -> putTL 0x11 >> putMongoStamp x
	MinMax x -> case x of
		MinKey -> putTL 0xFF
		MaxKey -> putTL 0x7F
 where
	putTL t = putTag t >> putLabel k

getField :: Get Field
-- ^ Read binary representation of Element
getField = do
	t <- getTag
	k <- getLabel
	v <- case t of
		0x01 -> Float <$> getDouble
		0x02 -> String <$> getString
		0x03 -> Doc <$> getDocument
		0x04 -> Array <$> getArray
		0x05 -> getBinary >>= \(s, b) -> case s of
			0x00 -> return $ Bin (Binary b)
			0x01 -> return $ Fun (Function b)
			0x03 -> return $ Uuid (UUID b)
			0x04 -> return $ Uuid (UUID b)
			0x05 -> return $ Md5 (MD5 b)
			0x80 -> return $ UserDef (UserDefined b)
			_ -> fail $ "unknown Bson binary subtype " ++ show s
		0x07 -> ObjId <$> getObjectId
		0x08 -> Bool <$> getBool
		0x09 -> UTC <$> getUTC
		0x0A -> return Null
		0x0B -> RegEx <$> getRegex
		0x0D -> JavaScr . Javascript [] <$> getString
		0x0F -> JavaScr . uncurry (flip Javascript) <$> getClosure
		0x0E -> Sym <$> getSymbol
		0x10 -> Int32 <$> getInt32
		0x12 -> Int64 <$> getInt64
		0x11 -> Stamp <$> getMongoStamp
		0xFF -> return (MinMax MinKey)
		0x7F -> return (MinMax MaxKey)
		_ -> fail $ "unknown Bson value type " ++ show t
	return (k := v)

putTag = putWord8
getTag = getWord8

putLabel = putCString
getLabel = getCString

putDouble = putFloat64le
getDouble = getFloat64le

putInt32 :: Int32 -> Put
putInt32 = putWord32le . fromIntegral

getInt32 :: Get Int32
getInt32 = fromIntegral <$> getWord32le

putInt64 :: Int64 -> Put
putInt64 = putWord64le . fromIntegral

getInt64 :: Get Int64
getInt64 = fromIntegral <$> getWord64le

putCString :: Text -> Put
putCString x = do
	putByteString $ TE.encodeUtf8 x
	putWord8 0

getCString :: Get Text
getCString = TE.decodeUtf8 . SC.concat . LC.toChunks <$> getLazyByteStringNul

putString :: Text -> Put
putString x = let b = TE.encodeUtf8 x in do
	putInt32 $ toEnum (SC.length b + 1)
	putByteString b
	putWord8 0

getString :: Get Text
getString = do
	len <- subtract 1 <$> getInt32
	b <- getByteString (fromIntegral len)
	getWord8
	return $ TE.decodeUtf8 b

putDocumentWithSize :: Document -> PutM Int32
putDocumentWithSize es =
  let b = runPut (mapM_ putField es)
      len = (toEnum . fromEnum) (LC.length b + 5)  -- include length and null terminator
  in do
    putInt32 len
    putLazyByteString b
    putWord8 0
    return len

putDocument :: Document -> Put
putDocument es = const () <$> putDocumentWithSize es

getDocumentWithSize :: Get (Document,Int32)
getDocumentWithSize = do
	len <- subtract 4 <$> getInt32
	b <- getLazyByteString (fromIntegral len)
	return (runGet getFields b, len)
 where
	getFields = lookAhead getWord8 >>= \done -> if done == 0
		then return []
		else (:) <$> getField <*> getFields

getDocument :: Get Document
getDocument = fst <$> getDocumentWithSize

putArray :: [Value] -> Put
putArray vs = putDocument (zipWith f [0..] vs)
	where f i v = (T.pack $! show i) := v

getArray :: Get [Value]
getArray = map value <$> getDocument

type Subtype = Word8

putBinary :: Subtype -> ByteString -> Put
putBinary t x = let len = toEnum (SC.length x) in do
	putInt32 len
	putTag t
	putByteString x

getBinary :: Get (Subtype, ByteString)
getBinary = do
	len <- getInt32
	t <- getTag
	x <- getByteString (fromIntegral len)
	return (t, x)

{-putBinary :: Subtype -> ByteString -> Put
-- When Binary subtype (0x02) insert extra length field before bytes
putBinary t x = let len = toEnum (length x) in do
	putInt32 $ len + if t == 0x02 then 4 else 0
	putTag t
	when (t == 0x02) (putInt32 len)
	putByteString x-}

{-getBinary :: Get (Subtype, ByteString)
-- When Binary subtype (0x02) there is an extra length field before bytes
getBinary = do
	len <- getInt32
	t <- getTag
	len' <- if t == 0x02 then getInt32 else return len
	x <- getByteString (fromIntegral len')
	return (t, x)-}

putRegex (Regex x y) = putCString x >> putCString y
getRegex = Regex <$> getCString <*> getCString

putSymbol (Symbol x) = putString x
getSymbol = Symbol <$> getString

putMongoStamp (MongoStamp x) = putInt64 x
getMongoStamp = MongoStamp <$> getInt64

putObjectId (Oid x y) = putWord32be x >> putWord64be y
getObjectId = Oid <$> getWord32be <*> getWord64be

putBool x = putWord8 (if x then 1 else 0)
getBool = (> 0) <$> getWord8

putUTC :: UTCTime -> Put
-- store milliseconds since Unix epoch
putUTC x = putInt64 $ round (utcTimeToPOSIXSeconds x * 1000)

getUTC :: Get UTCTime
-- stored as milliseconds since Unix epoch
getUTC = posixSecondsToUTCTime . (/ 1000) . fromIntegral <$> getInt64

putClosure :: Text -> Document -> Put
putClosure x y = let b = runPut (putString x >> putDocument y) in do
	putInt32 $ (toEnum . fromEnum) (LC.length b + 4)  -- including this length field
	putLazyByteString b

getClosure :: Get (Text, Document)
getClosure = do
	getInt32
	x <- getString
	y <- getDocument
	return (x, y)


{- Authors: Tony Hannan <tony@10gen.com>
   Copyright 2010 10gen Inc.
   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License. -}
