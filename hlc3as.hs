
import qualified Data.ByteString as B
import Directory
import System.Environment
import System.Exit

import LC3
import LC3.Types

main = do
  args <- getArgs
  if length args == 0
    then exitUsage
    else inspectFile args

inspectFile args =
  let path = head args in
  if not (path `endsWith` ".asm")
    then putStrLn ".asm files please!" >> exitFailure
    else do exists <- doesFileExist path
            if not exists
              then putStrLn "Couldn't open file." >> exitFailure
              else handleFile path

handleFile path = do
    source <- readFile path
    putStrLn "Starting pass 1:"
    let eprg = parseProgram path source
    case eprg of
      Left err -> print err >> exitFailure
      Right prg -> handleProgram path prg

handleProgram path prg =
  let base = dropExtension path
      objPath = base ++ ".obj"
      symPath = base ++ ".sym"
--      astPath = base ++ ".ast"
  in do
    putStrLn "No errors in pass 1\nStarting pass 2:"
--    writeFile astPath (show prg)
    let ebits = assembleProgram prg
    case ebits of
      Left err -> print err >> exitFailure
      Right bits -> do
        putStrLn "No errors in pass 2\nAssembly successful"
        let st = symbolTableFile . symbolTable $ prg
        writeFile symPath st
        B.writeFile objPath bits

exitUsage = putStrLn "Usage: lc3as <assembly_file.asm>" >> exitFailure

endsWith as bs = take (length bs) (reverse as) == reverse bs
dropExtension = reverse . tail . dropWhile (/='.') . reverse 

