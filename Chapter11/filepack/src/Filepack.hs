{-# LANGUAGE OverloadedStrings #-}

module Filepack where

import Data.Binary (Word16, Word32, Word8)
import Data.Bits (Bits (shift, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Posix.Types (CMode (..), FileMode)
import Text.Read (readEither)

data FileContents
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)

data FileData = FileData
  { fileName :: FilePath,
    fileSize :: Word32,
    filePermissions :: FileMode,
    fileData :: FileContents
  }
  deriving (Eq, Read, Show)

newtype FilePack = FilePack {getPackedFiles :: [FileData]}
  deriving (Eq, Read, Show)

packFiles :: FilePack -> ByteString
packFiles = B64.encode . BC.pack . show

unpackFiles :: ByteString -> Either String FilePack
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack

class Encode a where
  encode :: a -> ByteString

class Decode a where
  decode :: ByteString -> Either String a

instance Encode ByteString where
  encode = id

instance Decode ByteString where
  decode = Right

instance Encode Text where
  encode = encodeUtf8

instance Decode Text where
  decode = Right . decodeUtf8

instance Encode String where
  encode = BC.pack

instance Decode String where
  decode = Right . BC.unpack

word16ToBytes :: Word16 -> (Word8, Word8)
word16ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. shift word (-8)
   in (a, b)

word16ToByteString :: Word16 -> ByteString
word16ToByteString word =
  let (a, b) = word16ToBytes word
   in BS.pack [a, b]

consWord16 :: Word16 -> ByteString -> ByteString
consWord16 word bytestring =
  let packedWord = word16ToByteString word
   in packedWord <> bytestring

instance Encode Word16 where
  encode = word16ToByteString

word16FromBytes :: (Word8, Word8) -> Word16
word16FromBytes (a, b) =
  let a' = fromIntegral a
      b' = shift (fromIntegral b) 8
   in a' .|. b'

bytestringToWord16 :: ByteString -> Either String Word16
bytestringToWord16 bytestring =
  case BS.unpack bytestring of
    [a, b] -> Right $ word16FromBytes (a, b)
    _otherwise ->
      let l = show $ BS.length bytestring
       in Left ("Expecting 4 bytes but got " <> l)

instance Decode Word16 where
  decode = bytestringToWord16

instance Encode FileMode where
  encode (CMode fMode) = encode fMode

instance Decode FileMode where
  decode = fmap CMode . decode

sampleFilePack :: FilePack
sampleFilePack =
  FilePack
    [ FileData "stringFile" 0 0 $ StringFileContents "hello",
      FileData "textFile" 0 0 $ TextFileContents "hello text",
      FileData "binaryFile" 0 0 $ ByteStringFileContents "hello bytestring"
    ]

testPackFile :: ByteString
testPackFile =
  packFiles sampleFilePack

testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile

testRoundTrip :: FilePack -> Bool
testRoundTrip pack =
  Right pack == unpackFiles (packFiles pack)
