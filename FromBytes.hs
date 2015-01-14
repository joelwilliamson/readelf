module FromBytes where

import Data.Bits
import Data.Int
import Data.Word
import Data.List(reverse)

data Endian = Big | Little
            deriving (Show,Eq)

class FromBytes a where
  fromBytes :: [Word8] -> a -- Convert from bytes arrange least significant to most significant (little-endian)
  fromRawBytes :: Endian -> [Word8] -> a
  fromBytes = fromRawBytes Little
  fromRawBytes Big = fromBytes . Data.List.reverse
  fromRawBytes Little = fromBytes

instance FromBytes Word8 where
  fromBytes [] = error "Too few bytes (0 given, 1 expected)"
  fromBytes [x] = fromIntegral x
  fromBytes _ = error "Too many bytes (1 expected)"

instance FromBytes Int8 where
  fromBytes = (fromIntegral :: Word8 -> Int8) . fromBytes

instance FromBytes Word16 where
  fromBytes [] = error "Too few bytes (0 given, 2 expected)"
  fromBytes [_] = error "Too few bytes (1 given, 2 expected)"
  fromBytes [low,high] = fromIntegral high `shift` 8 + fromIntegral low
  fromBytes _ = error "Too many bytes (2 expected)"

instance FromBytes Int16 where
  fromBytes = (fromIntegral :: Word16 -> Int16) . fromBytes

instance FromBytes Word32 where
  fromBytes [d,c,b,a] = fromIntegral a `shift` 24 +
                        fromIntegral b `shift` 16 +
                        fromIntegral c `shift` 8 +
                        fromIntegral d
  fromBytes _ = error "Wrong number of bytes (expected 4)"

instance FromBytes Int32 where
  fromBytes = (fromIntegral :: Word32 -> Int32) . fromBytes

instance FromBytes Word64 where
  fromBytes [h,g,f,e,d,c,b,a] = fromIntegral a `shift` 56 +
                                fromIntegral b `shift` 48 +
                                fromIntegral c `shift` 40 +
                                fromIntegral d `shift` 32 +
                                fromIntegral e `shift` 24 +
                                fromIntegral f `shift` 16 +
                                fromIntegral g `shift` 8 +
                                fromIntegral h
  fromBytes _ = error "Wrong number of bytes (expected 8)"

instance FromBytes Int64 where
  fromBytes = (fromIntegral :: Word64 -> Int64) . fromBytes
