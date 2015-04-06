module DICOM.Gen.Utilities(fix3Args) where

import System.Environment
import System.Exit

fix3Args :: IO (String,String,String)
fix3Args = do
    args <- getArgs
    case length args of
      0 -> return ("-","-","-")
      1 -> return (head args, "-","-")
      2 -> return (head args, args!!1,"-")
      3 -> return (head args, args!!1,args!!2)
      _ -> do prog <- getProgName
              putStrLn ("Usage: "++prog++" [infile1] [infile2] [outfile]")
              exitFailure
