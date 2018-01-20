{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}

module Main where

import Protolude
import qualified GUI
import Options.Generic
import Data.Aeson(FromJSON, decode)

data CmdLineOpts = GenerateServantApi FilePath
                 | Gui { port :: Int , path :: FilePath , calc :: Maybe FilePath }
    deriving (Generic, Show)

instance ParseRecord CmdLineOpts

main :: IO ()
main = do
    x <- getRecord "Calculus Toolbox 2"
    case (x :: CmdLineOpts) of
        Gui prt pth (Just n) -> GUI.runGUI pth n prt
        Gui prt pth Nothing -> do
            print ("Launching empty gui" :: Text)
            GUI.runEmptyGUI pth prt
        GenerateServantApi p -> GUI.writeJSCode p

data Config = Config {
    workDir :: [FilePath],
    currentCalc :: [Char]
} deriving (Generic, FromJSON)

debugMac :: IO ()
debugMac = do
    f <- readFile "/Users/goodlyrottenapple/Library/Application Support/CalculusToolbox/config.json"
    let Just (Config [wd] cc) = decode (toS f)
    putStrLn $ "Launching from: " ++ wd
    putStrLn $ "Current calculus is: " ++ cc
    GUI.runGUI wd cc 8081
