{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString
import Data.Int
import Data.Word
import Data.List(reverse,map)
import Data.Bits

type RawHeader = ByteString

data WordSize = ThirtyTwo | SixtyFour
              deriving (Show,Eq)
                       
data Endian = Big | Little
            deriving (Show,Eq)
                     
data OS = SysV | HPUX | NetBSD | Linux | Solaris | AIX | IRIX | FreeBSD | OpenBSD
        deriving (Show,Eq)
                 
data Kind = Relocatable | Executable | Shared | Core
          deriving (Show,Eq)
                   
data ISA = SPARC | X86 | MIPS | PowerPC | ARM | SuperH | IA64 | AMD64 | AArch64
         deriving (Show,Eq)
                  
data Address = Short Word32 | Long Word64
             deriving (Show,Eq)
                      
type HeaderSize = Word16

data Header = Header {
  magic :: Bool -- Was the magic number found
, wordSize :: WordSize
, endian :: Endian
-- Version omitted since it can only take a single value
, os :: OS
, abiVersion :: ()
, kind :: Kind
, isa :: ISA
, entryPoint :: Address
, programHeader :: Address
, sectionHeader :: Address
-- An implementation defined field
, headerSize :: HeaderSize
, programHeaderSize :: HeaderSize
, programHeaderNum :: Word16
, sectionHeaderSize :: HeaderSize
, sectionHeaderNum :: Word16
, sectionNamesIx :: Word16
} deriving (Show,Eq)

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

slice :: ByteString -> Int -> Int -> [Word8]
slice string bottom top = Data.List.map (index string) $ enumFromTo bottom top

interpretHeader :: RawHeader -> Header
interpretHeader h = Header {..}
  where magic = slice h 0 3 == unpack "\127ELF"
        wordSize = case index h 4 of
          1 -> ThirtyTwo
          2 -> SixtyFour
        endian = case index h 5 of
          1 -> Little
          2 -> Big
        -- Byte 6 reserved for ELF version
        os = case index h 7 of
          0 -> SysV
          1 -> HPUX
          2 -> NetBSD
          3 -> Linux
          6 -> Solaris
          7 -> AIX
          8 -> IRIX
          9 -> FreeBSD
          12 -> OpenBSD
        abiVersion = ()
        kind = case fromRawBytes endian $ slice h 16 17 :: Word16 of
          1 -> Relocatable
          2 -> Executable
          3 -> Shared
          4 -> Core
        isa = case fromRawBytes endian $ slice h 18 19 :: Word16 of
          2 -> SPARC
          3 -> X86
          8 -> MIPS
          20 -> PowerPC --0x14
          40 -> ARM -- 0x28
          42 -> SuperH -- 0x2A
          50 -> IA64 -- 0x32
          62 -> AMD64 -- 0x3E
          183 -> AArch64 -- 0xB7
          x -> error $ "Didn't recognize ISA code: " ++ show x
        entryPoint = readValue 24 24
        programHeader = readValue 28 32
        sectionHeader = readValue 32 40
        headerSize = readInt 40 52
        programHeaderSize = readInt 42 54
        programHeaderNum = readInt 44 56
        sectionHeaderSize = readInt 46 58
        sectionHeaderNum = readInt 48 60
        sectionNamesIx = readInt 50 62
        readValue s l = case wordSize of
          ThirtyTwo -> Short $ fromRawBytes endian $ slice h s $ s + 3
          SixtyFour -> Long $ fromRawBytes endian $ slice h l $ l + 7
        readInt s l = case wordSize of
          ThirtyTwo -> fromRawBytes endian $ slice h s $ s + 1
          SixtyFour -> fromRawBytes endian $ slice h l $ succ l

main = do
  ls <- Data.ByteString.readFile "ls"
  print $ interpretHeader ls
  
