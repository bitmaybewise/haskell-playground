{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

data FileData a = FileData
  { fileName :: FilePath,
    fileSize :: Word32,
    filePermissions :: FileMode,
    fileData :: a
  }
  deriving (Eq, Read, Show)

newtype FilePack a = FilePack {getPackedFiles :: [FileData a]}
  deriving (Eq, Read, Show)

packFiles :: (Show a) => FilePack a -> ByteString
packFiles = B64.encode . BC.pack . show

unpackFiles :: (Read a) => ByteString -> Either String (FilePack a)
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack

class Encode a where
  encode :: a -> ByteString
  encode = BS.drop 4 . encodeWithSize

  encodeWithSize :: a -> ByteString
  encodeWithSize a =
    let s = encode a
        l = fromIntegral $ BS.length s
     in word32ToByteString l <> s
  {-# MINIMAL encode | encodeWithSize #-}

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

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. shift word (-8)
      c = fromIntegral $ 255 .&. shift word (-16)
      d = fromIntegral $ 255 .&. shift word (-32)
   in (a, b, c, d)

word16ToByteString :: Word16 -> ByteString
word16ToByteString word =
  let (a, b) = word16ToBytes word
   in BS.pack [a, b]

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a, b, c, d) = word32ToBytes word
   in BS.pack [a, b, c, d]

consWord16 :: Word16 -> ByteString -> ByteString
consWord16 word bytestring =
  let packedWord = word16ToByteString word
   in packedWord <> bytestring

instance Encode Word16 where
  encode = word16ToByteString
  encodeWithSize w =
    let (a, b) = word16ToBytes w
     in BS.pack [2, 0, a, b]

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w =
    let (a, b, c, d) = word32ToBytes w
     in BS.pack [4, 0, 0, 0, a, b, c, d]

word16FromBytes :: (Word8, Word8) -> Word16
word16FromBytes (a, b) =
  let a' = fromIntegral a
      b' = shift (fromIntegral b) 8
   in a' .|. b'

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
  let a' = fromIntegral a
      b' = shift (fromIntegral b) 8
      c' = shift (fromIntegral c) 16
      d' = shift (fromIntegral d) 32
   in a' .|. b' .|. c' .|. d'

bytestringToWord16 :: ByteString -> Either String Word16
bytestringToWord16 bytestring =
  case BS.unpack bytestring of
    [a, b] -> Right $ word16FromBytes (a, b)
    _otherwise ->
      let l = show $ BS.length bytestring
       in Left ("Expecting 2 bytes but got " <> l)

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
    _otherwise ->
      let l = show $ BS.length bytestring
       in Left ("Expecting 4 bytes but got " <> l)

instance Decode Word16 where
  decode = bytestringToWord16

instance Decode Word32 where
  decode = bytestringToWord32

instance Encode FileMode where
  encode :: FileMode -> ByteString
  encode (CMode fMode) = encode fMode

instance Decode FileMode where
  decode = fmap CMode . decode

instance (Encode a) => Encode (FileData a) where
  encode FileData {..} =
    let encodedFileName = encodeWithSize fileName
        encodedFileSize = encodeWithSize fileSize
        encodedFilePermissions = encodeWithSize filePermissions
        encodedFileData = encodeWithSize fileData
        encodedData =
          encodedFileName
            <> encodedFileSize
            <> encodedFilePermissions
            <> encodedFileData
     in encode encodedData

instance (Decode a) => Decode (FileData a) where
  decode encodedFileData = undefined

instance (Encode a, Encode b) => Encode (a, b) where
  encode (a, b) =
    encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} (Encode a) => Encode [a] where
  encode = encode . foldMap encodeWithSize

-- sampleFilePack :: (Encode a) => FilePack a
-- sampleFilePack =
--   FilePack
--     [ FileData "stringFile" 0 0 $ StringFileContents "hello",
--       FileData "textFile" 0 0 $ TextFileContents "hello text",
--       FileData "binaryFile" 0 0 $ ByteStringFileContents "hello bytestring"
--     ]

-- testPackFile :: ByteString
-- testPackFile =
--   packFiles sampleFilePack

-- testUnpackFile :: Either String (FilePack a)
-- testUnpackFile = unpackFiles testPackFile

-- testRoundTrip :: FilePack a -> Bool
-- testRoundTrip pack =
--   Right pack == unpackFiles (packFiles pack)
