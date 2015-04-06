Module DICOM.Gen.Utilities(fix3Args) where

import System.Environment

fix2Args :: IO (String,String,String)
fix2Args = do
    args <- getArgs
    case length args of
      0 -> return ("-",     "-")
      1 -> return (args!!0, "-")
      2 -> return (args!!0, args!!1,"-")
      3 -> return (args!!0, args!!1,args!!2)
      _ -> do prog <- getProgName
              putStrLn ("Usage: "++prog++" [infile1] [infile2] [outfile]")
              exitFailure