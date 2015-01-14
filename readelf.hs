{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Readelf where

import qualified Data.ByteString as BS
import Data.Int
import Data.Maybe(fromMaybe)
import Data.Word
import Data.List(reverse,map)
import Data.Bits

import FromBytes

import qualified Text.PrettyPrint.Boxes as PP
import System.Environment(getArgs)

class PrettyPrint a where
  prettyPrint :: a -> PP.Box

type RawHeader = BS.ByteString

data WordSize = ThirtyTwo | SixtyFour
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

instance PrettyPrint Header where
  prettyPrint Header {..} = (PP.<+>) labels values
          where labels = PP.vcat PP.left
                         $ map (PP.text . (++":"))
                         ["ELF magic number","Word Size","Endianness","OS",
                          "ABI Version", "Type","Instruction Set","Entry Point Address",
                          "Program Header","Section Header","ELF Header size",
                          "Program Header Size","# of Program Headers",
                          "Section Header Size","# of Section Headers",
                          "Section Names string table Index"]
                values = PP.vcat PP.left
                         $ map PP.text
                         [show magic, show wordSize, show endian, show os, show abiVersion,
                          show kind, show isa, show entryPoint, show programHeader,
                          show sectionHeader, show headerSize, show programHeaderSize,
                          show programHeaderNum, show sectionHeaderSize,
                          show sectionHeaderNum, show sectionNamesIx]

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
  where magic = slice h 0 3 == BS.unpack "\127ELF"
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
} deriving Eq

instance Show SectionFlags where
  show SectionFlags {..} = (if write then "W" else "")
                           ++ (if alloc then "A" else "")
                           ++ (if exec then "X" else "")

sectionFlags :: Word64 -> SectionFlags
sectionFlags x = SectionFlags {
  write = testBit x 0,
  alloc = testBit x 1,
  exec = testBit x 2 }

data SectionHeader = SectionHeader {
  name :: BS.ByteString
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

instance PrettyPrint SectionHeader where
  prettyPrint SectionHeader {..} = (PP.<+>) labels values
    where labels = PP.vcat PP.left
                   $ map (PP.text . (++":"))
                   ["name","type","flags","Virtual Address",
                    "offset","size","link","info","alignment",
                    "entry size"]
          values = PP.vcat PP.left
                   $ map PP.text [show name,
                                  maybe "" show sKind,
                                  show flags,
                                  show virtualAddress,
                                  show offset,
                                  show size,
                                  show link,
                                  show info,
                                  show alignment,
                                  show entrySize]
type StringTable = [(Int,BS.ByteString)]

instance PrettyPrint [(Int,BS.ByteString)] where
  prettyPrint l = (PP.<+>) labels values
    where labels = PP.vcat PP.left
                   $ map (PP.text . show . fst) l
          values = PP.vcat PP.left
                   $ map (PP.text . show . snd) l

readStringTable :: Integral a => BS.ByteString -> Address -> a -> StringTable
readStringTable h (Long start') size = map (\ (f,s) -> (f,BS.pack s))
                                       $ splitOn 0 0
                                       $ slice h start (start + size - 1)
  where start = fromIntegral start'

-- The program header ph, has fields pointing to the start of the section header
-- table, the size of a section header, and the index of the string table header.
-- This gives us &strTbl = &secTbl + secSize*strTblIx.
getStringTableFromRaw64 :: BS.ByteString -> Header -> [(Int,BS.ByteString)]
getStringTableFromRaw64 h header=
  readStringTable h (arrayIndex strTbl 0 tableSize) tableSize
  where secTbl = sectionHeader header
        strTblIx = sectionNamesIx header
        secSize = fromIntegral $ sectionHeaderSize header
        strTblHeader@(Long section) = arrayIndex secTbl strTblIx secSize
        strTbl = Long $ fromRawBytes (endian header)
                     $ slice h (section + 24) (section + 31)
        tableSize = fromRawBytes (endian header)
                    $ slice h (section + 32) (section + 39) :: Word64

interpretSectionHeader64 :: BS.ByteString -> Endian -> Address -> StringTable -> SectionHeader
interpretSectionHeader64 h endian (Long start) str = SectionHeader {..}
  where name = fromMaybe "" $ (fromIntegral $ (readValue 0 4::Word32)) `lookup` str
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

-- This is used to build an association list from a string.
-- It prepends how far into the string each substring was found.
-- It might be better to just leave the string unprocessed, index
-- in on each call, and parse to find the end of that C-string.
-- Currently, we do O(nm), where n is the number of strings, m is
-- average length, preprocessing, then each lookup is O(n). Indexing
-- would be no preprocessing (or maybe copy to seperate storage O(nm))
-- then O(m) index/parse.
splitOn :: Eq a => Int -> a -> [a] -> [(Int,[a])]
splitOn i x [] = [(i,[])]
splitOn i x [n]
  | x == n = [(i,[])]
  | otherwise = [(i,[n])]
splitOn i x (hd:tl)
  | x == hd = (i,[]):r
  | otherwise = (i,hd:(snd $ head r)):(tail r)
  where r = splitOn (succ i) x tl


main = do
  args <- getArgs
  let file = case args of
        [] -> "ls"
        [file] -> file
        l -> error $ "Couldn't handle arguments: " ++ show l
  ls <- BS.readFile file
  let elfHeader = interpretProgramHeader ls
  let prettyElf = prettyPrint elfHeader
  let stringTable = getStringTableFromRaw64 ls elfHeader
  let prettyStringTable = prettyPrint stringTable
  let startAddress headerNum =
        case sectionHeader elfHeader of
          Long addr -> Long $ fromIntegral addr + (fromIntegral headerNum) * (fromIntegral $ sectionHeaderSize elfHeader)
  let prettySections = PP.vsep 1 PP.left $
                       map (\n -> prettyPrint
                                  $ interpretSectionHeader64 ls (endian elfHeader) (startAddress n) stringTable)
                       [0..pred $ sectionHeaderNum elfHeader]
  PP.printBox $ PP.vsep 3 PP.left [prettyElf,prettyStringTable,prettySections]
  return (ls,elfHeader,stringTable)
