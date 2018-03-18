module Main where

import Control.Lens.Operators hiding ((<.>))
import Control.Monad
import Data.Yaml.Config
import System.Directory
import System.Environment
import System.FilePath

import Blackmail.SMTP.Server

main :: IO ()
main = do
    putStrLn "Launched blackmail v0.1.0"

    cwd <- getCurrentDirectory
    execPath <- getExecutablePath

    let localCfg = cwd </> "blackmail" <.> "yaml"
        installedCfg = (takeDirectory execPath) </> "etc" </> "blackmail" <.> "yaml"

    files <- filterM doesFileExist [localCfg, installedCfg]

    cfg <- loadYamlSettings files defaultSettings useEnv

    runSMTPServer cfg
    return ()
