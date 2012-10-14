
module LC3
    ( assemble
    , parseProgram
    , assembleProgram
    , symbolTableFile
    ) where

import qualified Data.ByteString as B
import Data.Either (lefts, rights)
import Data.List (sortBy)
import qualified Data.Map as M
import Numeric (showHex)
import Text.ParserCombinators.Parsec.Pos (newPos)

import LC3.Assembler
import LC3.Parser
import LC3.Types

assemble f s =
  let ep = parseProgram f s in
  case ep of
    Right p  ->
      let eb = assembleProgram p in
      case eb of
        Right bs -> Right (symbolTableFile . symbolTable $ p, bs)
        Left err -> Left err
    Left err -> Left err

symbolTableFile t = "// Symbol table\n// Scope level 0:\n" ++
                    "//\tSymbol Name       Page Address\n" ++
                    "//\t----------------  ------------\n" ++ go t
    where go = concat . map line . sortBy elComp . M.assocs
          elComp (_, a1) (_, a2) = compare a1 a2
          line (l, a) = "//\t" ++ l ++ spaces l ++ showHex a "" ++ "\n"
          spaces l = replicate (max (18 - length l) 2) ' '

makeStatement nm args = Statement { position = newPos "test" 0 0
                                  , address = 0x3000
                                  , labels = []
                                  , name = nm
                                  , arguments = args
                                  }

