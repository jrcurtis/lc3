
module LC3.Assembler ( assembleProgram ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Char (ord)
import qualified Data.Map as M
import Data.Either (lefts, rights)
import Data.Word
import Text.ParserCombinators.Parsec

import LC3.Types

assembleProgram p =
  let r = runParser p_program p (source p) (statements p) in
  case r of
    Right b -> Right . B.append (split16 . fromIntegral . origin $ p) $ b
    err     -> err

-- Given 16 bits, returns a two item list holding the high and low bits.
split16 :: Word16 -> B.ByteString
split16 w = B.pack [ fromIntegral $ 0x00FF .&. shiftR w 8
                   , fromIntegral $ 0x00FF .&. w]

p_program :: GenParser Statement Program B.ByteString
p_program = do bs <- many p_statement
               return . B.concat . map split16 . concat $ bs

p_statement :: GenParser Statement Program [Word16]
p_statement = do
    plains <- try p_unstringz <|> try p_unblkw <|> p_unlabel
    setPosition . position $ plains!!0
    let rs = map assembleInstruction plains
    case lefts rs of
        []   -> return . rights $ rs
        errs -> fail . unlines $ errs

p_unstringz :: GenParser Statement Program [Statement]
p_unstringz = do
    st <- anyToken
    if name st /= ".STRINGZ"
        then fail ""
        else return . unstringz $ st

-- Turns a .STRINGZ Statement into a list of .FILL Statements
unstringz :: Statement -> [Statement]
unstringz s = case s of
    Statement { name = ".STRINGZ", address = adr, arguments = args } ->
        [s { address = adr + n
           , name = ".FILL"
           , arguments = [Immediate c]
           } | (c, n) <- zip (makeStringz $ args !! 0) [0..]]

-- Turns a Stringz argument into a list of ascii codes
makeStringz :: Argument -> [Int]
makeStringz (Stringz s) = [fromIntegral . ord $ c | c <- s] ++ [0x0]

p_unblkw :: GenParser Statement Program [Statement]
p_unblkw = do
    st <- anyToken
    if name st /= ".BLKW"
        then fail ""
        else return . unblkw $ st

-- Turns a .BLKW Statement into a list of .FILL Statements
unblkw :: Statement -> [Statement]
unblkw s = case s of
    Statement { name = ".BLKW", address = adr, arguments = args } ->
        let blocks = case args !! 0 of { Immediate n -> n; _ -> 1 } in
        [s { address = adr + n
           , name = ".FILL"
           , arguments = [args !! 1]
           } | n <- [0..blocks-1]]

p_unlabel :: GenParser Statement Program [Statement]
p_unlabel = do
    st <- anyToken
    prg <- getState
    let tbl = symbolTable prg
    let ist = unlabel tbl st
    case ist of
        Right nst -> return [nst]
        Left err  -> fail err

unlabel :: SymbolTable -> Statement -> Either String Statement
unlabel tbl st =
    let immArgs = map (argBits (name st) (address st) tbl) (arguments st)
    in case lefts immArgs of
        []  -> Right st { arguments = map Immediate . rights $ immArgs }
        err -> Left . unlines $ err

argBits :: String -> Int -> SymbolTable -> Argument -> Either String Int
argBits _ _ _ (Immediate n) = Right n
argBits _ _ _ (Register n)  = Right n
argBits op adr tbl (Label n) =
    if n `M.member` tbl
        then case op of
            ".FILL" -> Right $ M.findWithDefault 0 n tbl -- absolute
            _       -> Right (M.findWithDefault 0 n tbl - adr - 1) -- relative
      else Left $ "unkown label: " ++ show n

-- Only accepts statements that compile to a single instruction and whose
-- arguments have all been converted to immediates.
assembleInstruction :: Statement -> Either String Word16
assembleInstruction Statement { name = nm, arguments = args }
  | nm == "GETC"  = Right 0xF020
  | nm == "OUT"   = Right 0xF021
  | nm == "PUTS"  = Right 0xF022
  | nm == "IN"    = Right 0xF023
  | nm == "PUTSP" = Right 0xF024
  | nm == "HALT"  = Right 0xF025
  | nm == "RTI"   = Right 0x8000
  | nm == "RET"   = Right 0xC1C0
  | nm == ".FILL" = Right . fromIntegral $ (iargs!!0)
  | nm == "NOP"   = Right 0x0
  | nm == "ADD" || nm == "AND" =
      makeBits nm [(3, iargs!!0), (3, iargs!!1), (3, 0), (3, iargs!!2)]
  | nm == "IADD" || nm == "IAND" =
      makeBits (tail nm) [(3, iargs!!0), (3, iargs!!1), (1, 1), (5, iargs!!2)]
  | nm == "NOT" =
      makeBits nm [(3, iargs!!0), (3, iargs!!1), (6, -1)]
  | nm == "TRAP" =
      makeBits nm [(4, 0), (8, iargs!!0)]
  | nm `elem` ["LD", "LDI", "ST", "STI", "BR", "LEA"] =
      makeBits nm [(3, iargs!!0), (9, iargs!!1)]
  | nm == "LDR" || nm == "STR" =
      makeBits nm [(3, iargs!!0), (3, iargs!!1), (6, iargs!!2)]
  | nm == "JSR" =
      makeBits nm [(1, 1), (11, iargs!!0)]
  | nm == "JSRR" =
      makeBits nm [(1, 0), (3, 0), (3, iargs!!0)]
  | nm == "JMP" =
      makeBits nm [(3, 0), (3, iargs!!0)]
  | otherwise = Left $ "unrecognized opcode: " ++ nm
  where iargs = map unImm args



-- Takes an instruction name and a list of pairs of the format
-- (number of bits, value) and combines them into a single 16 bit word,
-- using the name to lookup the opcode value and putting it in the top 4 bits.
-- E.g. to get a word with 11 in the upper 4
-- bits and 123 in the lower 12 bits, you would pass [(4, 11), (12, 123)].
-- Will generate a Left value if a value is too big to fit in the specified
-- bits or if the total number of bits specified is greater than 16.
makeBits :: String -> [(Int, Int)] -> Either String Word16
makeBits nm args =
    let opcode = (4, M.findWithDefault 0xD nm opcodeTable)
        margs = opcode:args
    in case go 16 margs of
        Right bits -> Right $ fromIntegral bits
        err -> err
    where
    go :: Int -> [(Int, Int)] -> Either String Word16
    go _ [] = Right 0x0
    go room ((bits, v):rest)
        | bits > room = Left $ "instruction can't fit argument"
        | otherwise   =
            if not (v `fitsBits` bits)
                then Left $ "argument out of range: " ++ show v
                else let this = fromIntegral (v `shiftL` (room - bits))
                         next = go (room - bits) rest
                         nextMask = 0xFFFF `shiftR` (16 - room + bits)
                     in case next of
                         Right n  -> Right $ this .|. (nextMask .&. n)
                         err      -> err

unImm :: Argument -> Int
unImm (Immediate n) = n

-- Can value n be represented as a 2's complement number with b bits?
-- Let's actually relax that a little and say as an unsigned integer or, if
-- negative, then as a two's complement integer.
fitsBits :: Int -> Int -> Bool
fitsBits n b = (n <= 2^b - 1) && (n >= -2^(b-1))

