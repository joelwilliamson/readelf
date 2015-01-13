{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import Data.List(reverse,map)
import Data.Bits

import System.Environment(getArgs)

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

type RawHeader = BS.ByteString

data WordSize = ThirtyTwo | SixtyFour
              deriving (Show,Eq)
                       
data Endian = Big | Little
            deriving (Show,Eq)
                     
data OS = SysV | HPUX | NetBSD | Linux | Solaris | AIX | IRIX | FreeBSD | OpenBSD | StandAlone
        deriving (Show,Eq)
                 
data Kind = Relocatable | Executable | Shared | Core
          deriving (Show,Eq)
                   
data ISA = SPARC | X86 | MIPS | PowerPC | ARM | SuperH | IA64 | AMD64 | AArch64
         deriving (Show,Eq)
                  
data Address = Short Word32 | Long Word64
             deriving (Show,Eq)

arrayIndex :: (Integral a, Integral b) => Address -> a -> b -> Address
arrayIndex (Long addr) ix size = Long $ addr + (fromIntegral ix) * (fromIntegral size)
arrayIndex (Short addr) ix size = Short $ addr + (fromIntegral ix) * (fromIntegral size)

type HeaderSize = Word16

data Header = Header {
  magic :: Bool -- Was the magic number found
, wordSize :: WordSize
, endian :: Endian
-- Version omitted since it can only take a single value
, os :: Maybe OS
, abiVersion :: ()
, kind :: Maybe Kind
, isa :: Maybe ISA
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

slice :: (Integral a, Enum a) => BS.ByteString -> a -> a -> [Word8]
slice string bottom top = map (BS.index string . fromIntegral) $ enumFromTo bottom top

oss :: [(Word8,OS)]
oss = [(0,SysV)
      ,(1,HPUX)
      ,(2,NetBSD)
      ,(3,Linux)
      ,(6,Solaris)
      ,(7,AIX)
      ,(8,IRIX)
      ,(9,FreeBSD)
      ,(12,OpenBSD)
      ,(255,StandAlone)]

kinds :: [(Word16,Kind)]
kinds = [(1,Relocatable)
        ,(2,Executable)
        ,(3,Shared)
        ,(4,Core)]

isas :: [(Word16,ISA)]
isas = [(2,SPARC)
       ,(3,X86)
       ,(8,MIPS)
       ,(20,PowerPC)
       ,(40,ARM)
       ,(42,SuperH)
       ,(50,IA64)
       ,(62,AMD64)
       ,(183,AArch64)]

interpretProgramHeader :: RawHeader -> Header
interpretProgramHeader h = Header {..}
  where magic = slice h 0 3 == BS.unpack "\x7fELF"
        wordSize = case BS.index h 4 of
          1 -> ThirtyTwo
          2 -> SixtyFour
        endian = case BS.index h 5 of
          1 -> Little
          2 -> Big
        -- Byte 6 reserved for ELF version
        os = BS.index h 7 `lookup` oss
        abiVersion = ()
        kind = (fromRawBytes endian $ slice h 16 17) `lookup` kinds
        isa = (fromRawBytes endian $ slice h 18 19) `lookup` isas
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

data SectionKind = Null | ProgramBits | SymbolTable | StringTable
                 | Rela -- "Rela" relocation entries
                 | SymbolHashTable | DynamicLinkTable | Notes
                 | Empty | Rel -- "Rel" relocation entries
                 | DynamicLoaderTable
                 deriving (Show,Eq)
                          
sectionKinds :: [(Word32,SectionKind)]
sectionKinds = [(0,Null)
               ,(1,ProgramBits)
               ,(2,SymbolTable)
               ,(3,StringTable)
               ,(4,Rela)
               ,(5,SymbolHashTable)
               ,(6,DynamicLinkTable)
               ,(7,Notes)
               ,(8,Empty)
               ,(9,Rel)
               ,(11,DynamicLoaderTable)]

data SectionFlags = SectionFlags {
  write :: Bool -- Section contains writable data
, alloc :: Bool -- Section is allocated in memory image of program
, exec :: Bool -- Section contains executable instructions
} deriving (Show,Eq)

sectionFlags :: Word64 -> SectionFlags
sectionFlags x = SectionFlags {
  write = testBit x 0,
  alloc = testBit x 1,
  exec = testBit x 2 }

data SectionHeader = SectionHeader {
  name :: Word32
, sKind :: Maybe SectionKind
, flags :: SectionFlags
, virtualAddress :: Address
, offset :: Address
, size :: Word64
, link :: Word32
, info :: Word32
, alignment :: Word64
, entrySize :: Word64
} deriving (Show,Eq)

interpretSectionHeader64 h endian (Long start) = SectionHeader {..}
  where name = 000
        sKind = readValue 4 4 `lookup` sectionKinds
        flags = sectionFlags $ readValue 8 8
        virtualAddress = Long $ readValue 16 8
        offset = Long $ readValue 24 8
        size = readValue 32 8
        link = readValue 40 4
        info = readValue 44 4
        alignment = readValue 48 8
        entrySize = readValue 56 8
        readValue offset size =
          fromRawBytes endian
          $ slice h (fromIntegral $ start + offset)
          (fromIntegral $ start + offset + size - 1)

          
main = do
  args <- getArgs
  let file = case args of
        [] -> "ls"
        [file] -> file
        l -> error $ "Couldn't handle arguments: " ++ show l
  ls <- BS.readFile file
  let ph = interpretProgramHeader ls
  print ph
  let startAddress headerNum =
        case sectionHeader ph of
          Long addr -> Long $ fromIntegral addr + (fromIntegral headerNum) * (fromIntegral $ sectionHeaderSize ph)
  mapM_ (\n -> print $ interpretSectionHeader64 ls (endian ph) $ startAddress n) [1..(sectionHeaderNum ph)]
  -- Filter for the string table, parse that next for better info display
  
