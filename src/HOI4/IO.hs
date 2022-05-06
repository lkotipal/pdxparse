{-|
Module      : HOI4.IO
Description : Read Europa Universalis IV scripts and parse to raw AST
-}
module HOI4.IO (
        readHOI4Scripts
    ,   module FileIO
    ) where

import Control.Monad (filterM, forM, when)

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes (Settings (..))

-- | Read all scripts in a directory.
--
-- Return: for each file, its path relative to the game root and the parsed
-- script.
readHOI4Scripts :: Settings -> FilePath -> IO [(FilePath, GenericScript)]
readHOI4Scripts settings category =
    let sourceSubdir = case category of
            "policies" -> "common" </> "policies"
            "ideagroups" -> "common" </> "ideas"
            _          -> category
        sourceDir = buildPath settings sourceSubdir
    in do
        files <- filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                    =<< getDirectoryContents sourceDir
        forM files $ \filename -> do
            let target = sourceSubdir </> filename
            content <- readScript settings target
            when (null content) $
                hPutStrLn stderr $ "Warning: " ++ target ++ " contains no scripts - failed parse? Expected feature type " ++ category
            return (target, content)

