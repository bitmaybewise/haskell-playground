{-# LANGUAGE OverloadedStrings #-}

module Filepack where

import Data.Binary (Word32, Word8)
import Data.Bits (Bits (shift, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Posix (FileMode)
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

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. (shift word (-8))
      c = fromIntegral $ 255 .&. (shift word (-16))
      d = fromIntegral $ 255 .&. (shift word (-24))
   in (a, b, c, d)

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a, b, c, d) = word32ToBytes word
   in BS.pack [a, b, c, d]

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToByteString word
   in packedWord <> bytestring

instance Encode Word32 where
  encode = word32ToByteString

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
  let a' = fromIntegral a
      b' = shift (fromIntegral b) 8
      c' = shift (fromIntegral c) 16
      d' = shift (fromIntegral d) 24
   in a' .|. b' .|. c' .|. d'

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
    _otherwise ->
      let l = show $ BS.length bytestring
       in Left ("Expecting 4 bytes but got " <> l)

instance Decode Word32 where
  decode = bytestringToWord32

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
