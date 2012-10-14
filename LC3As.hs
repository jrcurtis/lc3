
{-# LANGUAGE ForeignFunctionInterface #-}

module LC3As where

import qualified Data.ByteString as B
import Data.Char (chr, ord)
import Directory
import Foreign.C.String
import System.Environment
import System.Exit

import LC3
import LC3.Types

compile :: CString -> IO CString
compile path = do
    hpath <- peekCAString path
    message <- inspectFile hpath
    newCString message

inspectFile path =
  if not (path `endsWith` ".asm")
    then return ".asm files please!"
    else do exists <- doesFileExist path
            if not exists
              then return "1Couldn't open file."
              else do
                result <- handleFile path
                return ((chr . (+ord '0') . fst $ result) : snd result)

handleFile path = do
    source <- readFile path
    let message = "Starting pass 1:"
    let eprg = parseProgram path source
    case eprg of
      Left err -> return (1, message ++ show err)
      Right prg -> do
        result <- handleProgram path prg
        return (fst result, message ++ snd result)

handleProgram path prg =
  let base = dropExtension path
      objPath = base ++ ".obj"
      symPath = base ++ ".sym"
--      astPath = base ++ ".ast"
  in do
    let message = "No errors in pass 1\nStarting pass 2:"
--    writeFile astPath (show prg)
    let ebits = assembleProgram prg
    case ebits of
      Left err -> return (1, message ++ show err)
      Right bits -> do
        let st = symbolTableFile . symbolTable $ prg
        writeFile symPath st
        B.writeFile objPath bits
        return (0, message ++ "No errors in pass 2\nAssembly successful")

exitUsage = "Usage: lc3as <assembly_file.asm>"

endsWith as bs = take (length bs) (reverse as) == reverse bs
dropExtension = reverse . tail . dropWhile (/='.') . reverse 

foreign export ccall compile :: CString -> IO CString
