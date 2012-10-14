
module LC3.Parser
    ( parseProgram
    ) where

import Char (ord, toUpper)
import Data.Bits ((.|.))
import Data.List (elemIndex)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (label, labels)
import Text.ParserCombinators.Parsec.Token

import LC3.Types

-- Given a filename of an LC-3 assembly program, will parse it and return
-- a Program object or an error.
parseProgram = runParser program (0, M.empty)


-- No read instance for binary literals, so here's a thing.
fromBinaryString (x:xs) | x == '-'  = -fromBinaryString xs
                        | otherwise = foldl go 0 (x:xs)
    where go a b | b `elem` "01" = 2 * a + (ord b - ord '0')
                 | otherwise     = error "unexpected input in binary string"

-- Given a string with characters corresponding to fields in a bit vector,
-- returns the int with the proper bits set.
str2bits              :: String -> String -> Int
str2bits table (c:cs) = case elemIndex c $ reverse table of
                            Just i  -> 2^i .|. str2bits table cs
                            Nothing -> str2bits table cs
str2bits _     _      = 0

-- Updates the Map t with all the labels in ls mapped to the address adr.
-- Upon finding an already existing label, returns a left value with an error
-- message. Also increments the current address by the width of the instruction.
updateSymbolTable :: Statement -> AddressTable -> Either String AddressTable
updateSymbolTable statement table =
    let newAddress = fst table + width statement
        newTable   = go (labels statement) (address statement) (snd table)
        go [] _ tbl  = Right tbl
        go (l:ls) adr tbl
            | l `M.member` tbl = Left $ "label " ++ l ++ " already used"
            | otherwise        =
                case go ls adr tbl of
                    Right newTbl -> Right (M.union (M.insert l adr tbl) newTbl)
                    Left err     -> Left err
    in case newTable of
        Right tbl -> Right (newAddress, tbl)
        Left err  -> Left err


type AddressTable = (Int, SymbolTable)

-- Our lexer for LC-3 assembly. Gives us useful methods for dealing with
-- reserved words, case sensitivity, and more!
lexer :: TokenParser AddressTable
lexer =  makeTokenParser
          LanguageDef { commentStart    = ""
                      , commentEnd      = ""
                      , commentLine     = ";"
                      , nestedComments  = False
                      , identStart      = letter <|> oneOf "_"
                      , identLetter     = alphaNum <|> oneOf "_"
                      , opStart         = oneOf ""
                      , opLetter        = oneOf ""
                      , reservedNames   = opcodes ++ branches ++ traps ++
                                          pseudoOps ++ directives
                      , reservedOpNames = []
                      , caseSensitive   = False
                      }

m_charLiteral = charLiteral lexer
m_comma = comma lexer
m_commaSep = commaSep lexer
m_identifier = identifier lexer
m_lexeme = lexeme lexer
m_reserved = reserved lexer
m_stringLiteral = stringLiteral lexer
m_symbol   = symbol lexer
m_whiteSpace = whiteSpace lexer

-- Parser for an entire LC-3 assembly program.
program :: CharParser AddressTable Program
program = do p <- getPosition
             m_whiteSpace
             o <- orig
             setState (o, M.empty) -- Initial memory address
             sts <- many statement
             (_, tbl) <- getState
             return Program { source = sourceName p
                            , origin = o
                            , symbolTable = tbl
                            , statements = sts }

-- The .orig directive at the beginning of the program.
orig :: CharParser AddressTable Int
orig = do m_reserved ".ORIG"
          a <- lc3num
          return a

-- A statement has some number of labels before it, then an instruction.
statement :: CharParser AddressTable Statement
statement = do ls <- many instLabel
               i <- p_instruction
               pos <- getPosition
               (adr, tbl) <- getState
               let s = Statement { position    = pos
                                 , address     = adr
                                 , labels      = ls
                                 , name        = op i
                                 , arguments   = args i }
               case updateSymbolTable s (adr, tbl) of
                   Left err -> fail err
                   Right st -> setState st >> return s

-- A label in from of an instruction may have a trailing colon which is 
-- syntactically and semantically meaningless.
instLabel :: CharParser AddressTable String
instLabel = do l <- m_identifier
               option "" . try . m_symbol $ ":"
               return . map toUpper $ l
            <?> "label"

-- Parser for any instruction.
p_instruction :: CharParser AddressTable Instruction
p_instruction = p_opcodes <|> p_branches <|> p_traps <|> p_pseudoOps <|> end

-- Parser for opcodes.
p_opcodes :: CharParser AddressTable Instruction
p_opcodes = do n <- reservedChoice ["ADD", "AND"]
               r1 <- arg register
               r2 <- arg register
               a <- register <|> immediate
               let i = Instruction { op = n, args = [r1, r2, a] }
               case a of -- If last arg is immediate, mark for assembly
                   Immediate _ -> return i { op = "I" ++ n }
                   _           -> return i

        <|> do n <- reservedChoice ["JMP", "JSRR"]
               r <- register
               return Instruction { op = n, args = [r] }

        <|> do m_reserved "TRAP"
               i <- immediate
               return Instruction { op = "TRAP", args = [i] }

        <|> do m_reserved "JSR"
               l <- argLabel
               return Instruction { op = "JSR", args = [l] }

        <|> do n <- reservedChoice ["LD", "LDI", "ST", "STI", "LEA"]
               r <- arg register
               i <- argLabel
               return Instruction { op = n, args = [r, i] }

        <|> do n <- reservedChoice ["LDR", "STR"]
               r1 <- arg register
               r2 <- arg register
               i <- immediate
               return Instruction { op = n, args = [r1, r2, i] }

        <|> do n <- g_reserved "NOT"
               r1 <- arg register
               r2 <- register
               return Instruction { op = n, args = [r1, r2] }

        <|> (g_reserved "RTI" >>
                 return Instruction { op = "RTI", args = []})

-- Parses a branch instruction
p_branches :: CharParser AddressTable Instruction
p_branches = do n <- reservedChoice branches
                let codes = str2bits "nzp" (drop 2 n) -- The characters after BR
                l <- argLabel
                return Instruction { op = "BR", args = [Immediate codes, l] }

-- Parser for trap pseudonyms. These are simple names and take no arguments.
p_traps :: CharParser AddressTable Instruction
p_traps = choice [m_reserved trap >>
                  return Instruction { op = trap, args = [] }
                      | trap <- traps]

-- Parser for pseudo ops.
p_pseudoOps :: CharParser AddressTable Instruction
p_pseudoOps = do m_reserved ".BLKW"
                 num <- immediate -- Number of words
                 v <- option (Immediate 0)
                             (m_comma >> immediate) -- Initial value
                 return Instruction { op = ".BLKW", args = [num, v] }

          <|> do m_reserved ".FILL"
                 v <- immediate <|> p_charLiteral <|> argLabel
                 return Instruction { op = ".FILL", args = [v] }

          <|> do m_reserved ".STRINGZ"
                 s <- m_stringLiteral
                 return Instruction { op = ".STRINGZ", args = [Stringz s] }

          <|> do m_reserved "RET"
                 return Instruction { op = "RET", args = [] }

end :: CharParser AddressTable Instruction
end = m_reserved ".END" >>
      many anyChar >>
      eof >>
      return Instruction { op = "NOP", args = [] }

-- Parses a parser followed by a comma because apparently ther's no combinator
-- like commaSep that takes a list of parsers to apply in sequence rather than
-- a single parser to apply repeatedly!!!
arg :: CharParser AddressTable a -> CharParser AddressTable a
arg a = do res <- a
           m_comma
           return res

-- A register argument is the letter R and a number from 0-7
register :: CharParser AddressTable Argument
register = m_lexeme (do oneOf "rR"
                        n <- octDigit
                        return $ Register $ ord n - ord '0')
           <?> "register"

-- An immediate argument is simply an lc3num wrapped in an Argument constructor.
immediate :: CharParser AddressTable Argument
immediate = do n <- lc3num
               return $ Immediate n
            <?> "immediate"

p_charLiteral :: CharParser AddressTable Argument
p_charLiteral = do
    c <- m_charLiteral
    return . Immediate . ord $ c

-- Parses a number in LC-3 assembly format. Hexadecimal, binary, or decimal.
-- Wrapped in lexeme because the character parsers for digits only parse a
-- single character.
lc3num :: CharParser AddressTable Int
lc3num = m_lexeme (do oneOf "xX" -- If it starts with x, it's hexadecimal
                      neg <- option "" (string "-")
                      ds <- many1 hexDigit
                      return . read $ neg ++ '0':'x':ds -- Take advantage of read

               <|> do oneOf "bB" -- B indicates binary 
                      neg <- option "" (string "-")
                      ds <- many1 $ oneOf "01"
                      return . fromBinaryString $ (neg ++ ds)

               <|> do char '#' -- Explicit decimal
                      neg <- option "" (string "-")
                      ds <- many1 digit
                      return . read $ neg ++ ds

               <|> do neg <- option "" (string "-")
                      ds <- many1 digit -- Implicit decimal
                      return . read $ neg ++ ds)
               <?> "number"

-- Parses a valid label (without trailing colon) and returns an Argument.
argLabel :: CharParser AddressTable Argument
argLabel = do n <- m_identifier
              return . Label . map toUpper $ n
       <?> "label"

reservedChoice :: [String] -> CharParser AddressTable String
reservedChoice ns = choice [g_reserved n | n <- ns]

-- Get reserved. Works like reserved, but returns the parsed word
g_reserved :: String -> CharParser AddressTable String
g_reserved n = m_reserved n >> return n

