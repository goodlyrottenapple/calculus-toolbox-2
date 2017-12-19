{-# LANGUAGE DeriveGeneric         #-}

module Main where

import Protolude
import qualified GUI
import Options.Generic

data CmdLineOpts = GenerateServantApi FilePath
                 | Gui { path :: FilePath , name :: Maybe FilePath }
    deriving (Generic, Show)

instance ParseRecord CmdLineOpts

main :: IO ()
main = do
    x <- getRecord "Calculus Toolbox 2"
    case (x :: CmdLineOpts) of
        Gui p (Just n) -> GUI.runGUI p n
        Gui p Nothing -> do
            print ("Launching empty gui" :: Text)
            GUI.runEmptyGUI p
        GenerateServantApi p -> GUI.writeJSCode 8081 p

