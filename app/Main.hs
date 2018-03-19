module Main where

import Protolude
-- import qualified GUI
-- import Options.Generic

-- data CmdLineOpts = GenerateServantApi FilePath
--                  | Gui { port :: Int , path :: FilePath , calc :: Maybe [Char] }
--     deriving (Generic, Show)

-- instance ParseRecord CmdLineOpts

main :: IO ()
main = undefined -- do
    -- x <- getRecord "Calculus Toolbox 2"
    -- case (x :: CmdLineOpts) of
    --     Gui prt pth (Just n) -> GUI.runGUI pth n prt
    --     Gui prt pth Nothing -> do
    --         print ("Launching empty gui" :: Text)
    --         GUI.runEmptyGUI pth prt
    --     GenerateServantApi p -> GUI.writeJSCode p
