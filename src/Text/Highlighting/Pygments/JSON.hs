{-# LANGUAGE OverloadedStrings #-}
module Text.Highlighting.Pygments.JSON 
    (
      getPygmentsJSON
    ) where

import Control.Monad (when)
import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy.Char8 ()
import System.Exit
import System.Process.ByteString.Lazy

import Paths_hpygments

getJSONDumper :: IO FilePath
getJSONDumper = getDataFileName "pygments_dump_json.py"

getPygmentsJSON :: FromJSON a => String -> IO a
getPygmentsJSON what = do
    jsonDumper <- getJSONDumper
    (exitCode, stdout, _) <- runPython jsonDumper [what] ""
    when (exitCode /= ExitSuccess) $
        fail $ jsonDumper ++ " failed: " ++ show exitCode
    case decode stdout of
        Nothing -> fail $ "failed to decode " ++ what
        Just ls -> return ls
  where
    runPython script args input = do
        readProcessWithExitCode "python" (script : args) input
