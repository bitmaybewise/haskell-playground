{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Filepack where

import Control.Applicative
import Control.Monad (when)
import Data.Binary (Word16, Word32, Word8)
import Data.Bits (Bits (shift, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Posix.Types (CMode (..), FileMode)

data FileData a = FileData
  { fileName :: FilePath,
    fileSize :: Word32,
    filePermissions :: FileMode,
    fileData :: a
  }
  deriving (Eq, Read, Show)

data Packable = forall a. (Encode a) => Packable {getPackable :: FileData a}

newtype FilePack = FilePack [Packable]

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
  decode =
    execParser $
      FileData
        <$> extractValue
        <*> extractValue
        <*> extractValue
        <*> extractValue

instance (Encode a, Encode b) => Encode (a, b) where
  encode (a, b) =
    encode $ encodeWithSize a <> encodeWithSize b

instance (Decode a, Decode b) => Decode (a, b) where
  decode = execParser $ (,) <$> extractValue <*> extractValue

instance {-# OVERLAPPABLE #-} (Encode a) => Encode [a] where
  encode = encode . foldMap encodeWithSize

instance Encode FilePack where
  encode (FilePack p) = encode p

instance Encode Packable where
  encode (Packable p) = encode p

addFileDataToPack :: (Encode a) => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ Packable a : as

infixr 6 .:

(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack

emptyFilePack :: FilePack
emptyFilePack = FilePack []

testEncodeValue :: ByteString
testEncodeValue =
  let a =
        FileData
          { fileName = "a",
            fileSize = 3,
            filePermissions = 0755,
            fileData = "foo" :: String
          }
      b =
        FileData
          { fileName = "b",
            fileSize = 10,
            filePermissions = 0644,
            fileData = ["hello", "world"] :: [Text]
          }
      c =
        FileData
          { fileName = "c",
            fileSize = 8,
            filePermissions = 0644,
            fileData = (0, "zero") :: (Word32, String)
          }
   in encode $ a .: b .: c .: emptyFilePack

naiveDecodeWord32 :: ByteString -> Either String (Word32, ByteString)
naiveDecodeWord32 inputString = do
  when (BS.length inputString < 4) $
    Left "Error, not enough data to get the size of the next field"
  let (encodedSizePrefix, rest) = BS.splitAt 4 inputString
  sizePrefix <- fromIntegral <$> bytestringToWord32 encodedSizePrefix
  when (sizePrefix /= 4) $
    Left "the field size of a word should be 4"
  when (BS.length rest < fromIntegral sizePrefix) $
    Left "Not enough data for the next field size"
  let (encodedWord, rest') = BS.splitAt sizePrefix rest
  decodedWord <- decode encodedWord
  pure (decodedWord, rest')

extractBytes :: Int -> ByteString -> Either String (ByteString, ByteString)
extractBytes n byteString = do
  when (BS.length byteString < n) $
    Left $
      "Error, extract bytes needs at least " <> show n <> " bytes"
  pure $ BS.splitAt n byteString

nextSegmentSize :: ByteString -> Either String (Word32, ByteString)
nextSegmentSize byteString = do
  (nextSegmentStr, rest) <- extractBytes 4 byteString
  parsedSegmentSize <- bytestringToWord32 nextSegmentStr
  pure (parsedSegmentSize, rest)

newtype FilePackParser a = FilePackParser
  {runParser :: ByteString -> Either String (a, ByteString)}

instance Functor FilePackParser where
  fmap f parser = FilePackParser $ \input -> do
    (parsedValue, result) <- runParser parser input
    pure (f parsedValue, result)

instance Applicative FilePackParser where
  pure a = FilePackParser $ \s -> pure (a, s)
  f <*> s = FilePackParser $ \input -> do
    (f', initialRemainder) <- runParser f input
    (a, finalRemainder) <- runParser s initialRemainder
    pure (f' a, finalRemainder)

extractValue :: (Decode a) => FilePackParser a
extractValue = FilePackParser $ \input -> do
  when (BS.length input < 4) $
    Left "INput has less than 4 bytes, we can't get a segment size"
  let (rawSegmentSize, rest) = BS.splitAt 4 input
  segmentSize <- fromIntegral <$> bytestringToWord32 rawSegmentSize
  when (BS.length rest < segmentSize) $
    Left "not enough input to parse the next value"
  let (rawSegmentValue, rest') = BS.splitAt segmentSize rest
  case decode rawSegmentValue of
    Left err -> Left err
    Right a -> Right (a, rest')

execParser :: FilePackParser a -> ByteString -> Either String a
execParser parser inputString =
  fst <$> runParser parser inputString

testRoundTrip :: (Decode a, Encode a, Eq a, Show a) => a -> IO ()
testRoundTrip val =
  case decode (encode val) of
    Left err ->
      putStrLn $ "Failed to round-trip value: " <> err
    Right roundTripVal
      | roundTripVal == val ->
          putStrLn "It works!"
      | otherwise -> do
          putStrLn "Round-trip failed!"
          putStrLn $ "expected: " <> show val
          putStrLn $ "got:      " <> show roundTripVal

runRoundTripTest :: IO ()
runRoundTripTest =
  testRoundTrip $
    FileData
      { fileName = "c",
        fileSize = 8,
        filePermissions = 0644,
        fileData = (0, "zero") :: (Word32, String)
      }

extractValues :: (Decode a) => FilePackParser [a]
extractValues = FilePackParser $ \input ->
  if BS.null input
    then Right ([], "")
    else do
      (val, rest) <- runParser extractValue input
      (tail, rest') <- runParser extractValues rest
      pure (val : tail, rest')

parseEven :: FilePackParser Word32
parseEven = FilePackParser $ \input -> do
  (n, rest) <- runParser extractValue input
  when (odd n) $
    Left $
      show n <> ": value is odd"
  pure (n, rest)

parseSome :: FilePackParser a -> FilePackParser [a]
parseSome parseElement = (:) <$> parseElement <*> parseMany parseElement

parseMany :: FilePackParser a -> FilePackParser [a]
parseMany parseElement = parseSome parseElement <|> pure []

extractOptional :: FilePackParser a -> FilePackParser (Maybe a)
extractOptional parseElement = Just <$> parseElement <|> pure Nothing

instance Alternative FilePackParser where
  empty = FilePackParser $ const (Left "empty parser")
  parserA <|> parserB = FilePackParser $ \s ->
    case runParser parserA s of
      Right val -> Right val
      Left _ -> runParser parserB s

instance {-# OVERLAPPABLE #-} (Decode a) => Decode [a] where
  decode = execParser (many extractValue)

testDecodeValue ::
  ByteString ->
  Either
    String
    ( FileData String,
      FileData [Text],
      FileData (Word32, String)
    )
testDecodeValue =
  execParser $
    (,,)
      <$> extractValue
      <*> extractValue
      <*> extractValue

instance Monad FilePackParser where
  return = pure
  (>>=) :: FilePackParser a -> (a -> FilePackParser b) -> FilePackParser b
  valParser >>= mkParser = FilePackParser $ \input -> do
    (val, rest) <- runParser valParser input
    runParser (mkParser val) rest

data FilePackImage
  = FilePackPBM Word32 Word32 [Word32]
  | FilePackPGM Word32 Word32 Word32 [Word32]
  deriving (Eq, Show)

instance Encode FilePackImage where
  encode (FilePackPBM width height values) =
    encode $
      encodeWithSize @String "pbm"
        <> encodeWithSize width
        <> encodeWithSize height
        -- The Encode instance for list already includes size info
        <> encode values
  encode (FilePackPGM width height maxValue values) =
    encode $
      encodeWithSize @String "pgm"
        <> encodeWithSize width
        <> encodeWithSize height
        <> encodeWithSize maxValue
        -- The Encode instance for list already includes size info
        <> encode values

parsePBM, parsePGM :: FilePackParser FilePackImage
parsePBM =
  FilePackPBM
    <$> extractValue
    <*> extractValue
    <*> many extractValue
parsePGM =
  FilePackPGM
    <$> extractValue
    <*> extractValue
    <*> extractValue
    <*> many extractValue

getNetpbmParser :: String -> FilePackParser FilePackImage
getNetpbmParser tag =
  case tag of
    "pbm" -> parsePBM
    "pgm" -> parsePGM
    otherTag ->
      fail $ "unknown image tag: " <> otherTag

getNetpbmTag :: FilePackParser String
getNetpbmTag = extractValue

-- parseImage ::
--   FilePackParser String ->
--   (String -> FilePackParser FilePackImage) ->
--   FilePackParser FilePackImage
parseImage = getNetpbmTag >>= getNetpbmParser

instance Decode FilePackImage where
  decode = execParser parseImage

instance MonadFail FilePackParser where
  fail errMsg = FilePackParser (const $ Left errMsg)
