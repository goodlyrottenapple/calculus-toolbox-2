{-# LANGUAGE DeriveGeneric         #-}

module Main where

import Protolude
import qualified GUI
import Options.Generic

data CmdLineOpts = GenerateServantApi FilePath
                 | Gui { port :: Int , path :: FilePath , calc :: Maybe FilePath }
    deriving (Generic, Show)

instance ParseRecord CmdLineOpts

main :: IO ()
main = do
    x <- getRecord "Calculus Toolbox 2"
    case (x :: CmdLineOpts) of
        Gui prt p (Just n) -> GUI.runGUI p n prt
        Gui prt p Nothing -> do
            print ("Launching empty gui" :: Text)
            GUI.runEmptyGUI p prt
        GenerateServantApi p -> GUI.writeJSCode p

