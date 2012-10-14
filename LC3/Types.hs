
module LC3.Types where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec.Pos (SourcePos)

type SymbolTable = M.Map String Int

-- Represents an entire program, consisting of an origin in memory and a
-- sequence of statements.
data Program = 
    Program { source      :: String
            , origin      :: Int
            , symbolTable :: SymbolTable
            , statements  :: [Statement]
            } deriving (Show)

-- A statement comprises one line of a program; an instruction indexed by zero
-- or more labels.
data Statement =
    Statement { position    :: SourcePos
              , address     :: Int
              , labels      :: [String]
              , name        :: String
              , arguments   :: [Argument]
              } deriving (Show)

-- An instruction has a mnemonic and 0 or more arguments.
-- Used as an intermediate during parsing.
data Instruction = 
    Instruction { op   :: String
                , args :: [Argument]
                } deriving (Show)

-- Represents an argument passed to an instruction, which can either be a 
-- register (compiled to three bits referencing a register number), an
-- immediate (a literal number in the program converted to some number of
-- bits in the compiled instruction), or a label, which is a string referring
-- to a memory location that is eventually compiled to either an absolute
-- memory address or else an offset from some other address.
data Argument = Register Int | Immediate Int | Label String | Stringz String
                deriving (Show)


-- Opcode mnemonics
opcodes = ["ADD", "AND", "JMP", "JSR", "JSRR", "LD", "LDI", "LDR", "LEA",
           "NOT", "RTI", "ST", "STI", "STR", "TRAP"]
-- The branch instruction is taken separately because its arguments are
-- given in its name and it is specified explicitly so that
-- it can be passed to the reserved words. Maybe there is a more elegant way
-- to make all of these combinations reserved, but to parse them dynamically?
branches = ["BR", "BRp", "BRz", "BRzp", "BRn", "BRnp", "BRnz", "BRnzp"]
-- Trap pseudonyms
traps = ["GETC", "HALT", "IN", "OUT", "PUTS", "PUTSP"]
-- Pseudo ops
pseudoOps = [".BLKW", ".FILL", "RET", ".STRINGZ"]
-- Assembler Directives
directives = [".END", ".ORIG"]

opcodeTable = M.fromList
    ([("ADD", 0x1), ("AND", 0x5), ("NOT", 0x9), ("LD", 0x2), ("LDI", 0xA),
      ("LDR", 0x6), ("ST", 0x3),  ("STI", 0xB), ("STR", 0x7), ("BR", 0x0),
      ("LEA", 0xE), ("JSR", 0x4), ("JSRR", 0x4), ("JMP", 0xC), ("TRAP", 0xF),
      ("RTI", 0x8)]
      :: [(String, Int)])

-- Returns the size in 16 bit words of an instruction
width :: Statement -> Int
width inst | name inst == ".STRINGZ" = case arguments inst !! 0 of
                                            Stringz x -> length x + 1
                                            _         -> 1
           | name inst == ".BLKW"    = case arguments inst !! 0 of
                                            Immediate x -> x
                                            _           -> 1
           | otherwise             = 1
