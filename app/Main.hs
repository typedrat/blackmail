module Main where

import System.Posix.Signals

import Blackmail.Server

main :: IO ()
main = do
    putStrLn "Launched blackmail v0.1.0"
    installHandler sigPIPE Ignore Nothing
    runServer defaultSettings