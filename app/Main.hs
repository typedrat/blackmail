module Main where

import Control.Lens.Operators
import System.Posix.Signals

import Blackmail.Server

main :: IO ()
main = do
    putStrLn "Launched blackmail v0.1.0"
    installHandler sigPIPE Ignore Nothing
    let settings = defaultSettings & ports .~ [25, 587]
    runServer settings
