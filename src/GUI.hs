{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeApplications      #-}


module GUI where

import Lib.Prelude
import Network.Wai
import Network.Wai.Handler.Warp

import Network.Wai.Middleware.Cors

import Servant
import Data.IORef
import Data.Aeson

import Terms
import Terms.Parsers
import Terms.JSON
import Rules

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except


data GUIError = CalculusDescriptionNotLoaded
              -- | TermParseE TermParseError
              | Custom Text deriving (Generic, ToJSON)
-- type Status e = Either e ()


type API = "parseDSeq" :> QueryParam "val" Text :> Get '[JSON] (LatexDSeq [Rule Text] 'ConcreteK)
         :<|> "getMacros" :> Get '[JSON] Macros
         :<|> "getApplicableRules" :> ReqBody '[JSON] (DSequent 'ConcreteK Text) :> Post '[JSON] [(RuleName , [LatexDSeq [Rule Text] 'ConcreteK])]
         :<|> "getCalcDesc" :> Get '[JSON] CalcDesc
         -- :<|> "loadCalc" :> Capture "val" Text :> Get '[JSON] (Status CalculusDescParseError)


api :: Proxy API
api = Proxy


data CalDescStore c = CalDescStore {
    parsed :: c
  , rawCalc :: Text
  , rawRules :: Text
}

newtype Config r = Config {
    currentCalcDec :: IORef (Maybe (Text, CalDescStore (FinTypeCalculusDescription r)))
}

-- type AppM r = ReaderT (Config r) Handler

type AppM r = ReaderT (Config r) IO


getCaclDesc :: AppM r (FinTypeCalculusDescription r)
getCaclDesc = do
    cRef <- asks currentCalcDec
    cd <- liftIO $ readIORef cRef
    case cd of 
        Just (_, CalDescStore{..}) -> return parsed
        Nothing -> throwIO $ err500 {errBody = encode CalculusDescriptionNotLoaded}




liftCalcErrAppM :: ToJSON e => CalcMErr r e a -> AppM r a
liftCalcErrAppM cmerr = do
    cd <- getCaclDesc
    case runCalcErr cd cmerr of
        Left err -> throwIO $ err300 {errBody = encode err}
        Right res -> return res

parseDSeqH :: Maybe Text -> AppM r (LatexDSeq r 'ConcreteK)
parseDSeqH (Just inStr) = do
    cd <- getCaclDesc
    -- liftIO $ putStrLn $ "got: " <> inStr
    r <- liftCalcErrAppM $ parseCDSeq inStr
    return $ LatexDSeq (cd,r)
parseDSeqH Nothing = throwIO $ err300 {errBody = "No input given"} -- redo this properly??


getMacrosH :: AppM r Macros
getMacrosH = liftCalcErrAppM getMacros
    where
        getMacros :: CalcMErr r () Macros
        getMacros = do
            fconns <- asks formulaConns
            sconns <- asks structureConns
            return $ Macros $ M.fromList $ 
                (map (\(ConnDescription n _ _ _ _ _ l) -> ("\\seq" <> n, l)) fconns) ++
                (map (\(ConnDescription n _ _ _ _ _ l) -> ("\\seq" <> n, l)) sconns)



getApplicableRulesH :: DSequent 'ConcreteK Text -> AppM [Rule Text] [(RuleName , [LatexDSeq [Rule Text] 'ConcreteK])]
getApplicableRulesH dseq = do
    cd <- getCaclDesc
    -- liftIO $ print $ rules cd
    r <- foldrM (fun cd) M.empty (rules cd)
    return $ M.toList r
    where
        fun :: FinTypeCalculusDescription r -> Rule Text -> Map RuleName [LatexDSeq r 'ConcreteK] -> AppM [Rule Text] (Map RuleName [LatexDSeq r 'ConcreteK])
        fun cd r@Rule{..} m = do
            -- liftIO $ print $ name
            -- liftIO $ print $ unifySeq concl dseq
            -- liftIO $ print $ prems
            -- liftIO $ print $ do { udict <- unifySeq concl dseq ; mapM (\(DSeq l _ _) -> sub udict l) prems }
            -- liftIO $ print $ do { udict <- unifySeq concl dseq ; mapM (\(DSeq _ _ r) -> sub udict r) prems }
            -- liftIO $ print $ ("------------" :: Text)
            case isApplicable r dseq of
                Just ps -> return $ M.insert name (map (\s -> LatexDSeq (cd,s)) ps) m
                Nothing -> return m


getCaclDescH :: AppM r CalcDesc
getCaclDescH = do
    cRef <- asks currentCalcDec
    cd <- liftIO $ readIORef cRef
    case cd of 
        Just (name, CalDescStore{..}) -> return $ CalcDesc name rawCalc rawRules
        Nothing -> throwIO $ err500 {errBody = encode CalculusDescriptionNotLoaded}




    -- liftCalcErrAppM @() $ getApplicableRules dseq


-- -- loadCalcH :: Text -> AppM r (Status CalculusDescParseError)
-- -- loadCalcH inStr = do
-- --     case runExcept $ parseFinTypeCalculusDescription inStr of
-- --         Left err -> return $ Left err
-- --         Right calc' -> do
-- --             calcRef <- asks currentCalcDec
-- --             _ <- liftIO $ writeIORef calcRef $ Just calc'
-- --             return $ Right ()


ioToHandler :: Config r -> AppM r :~> Handler
ioToHandler cfg = NT $ \x -> Handler . ExceptT . try $ runReaderT x cfg


readerServer :: Config [Rule Text] -> Server API
readerServer cfg = enter (ioToHandler cfg) server

server :: ServerT API (AppM [Rule Text])
server = parseDSeqH :<|> getMacrosH :<|> getApplicableRulesH :<|> getCaclDescH 

myCors :: Middleware
myCors = cors (const $ Just customPolicy)
    where
        customPolicy = 
            simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "Cache-Control"] }



app :: Config [Rule Text] -> Application
app cfg = myCors (serve api (readerServer cfg))



mkConfig :: Text -> Text -> IO (Config [Rule Text])
mkConfig rawCalc rawRules = do
    let (Right c) = runExcept $ parseFinTypeCalculusDescription rawCalc
        (Right rs) = runCalcErr c (parseRules rawRules)
        parsed = c{rules = rs}
    currentCalcDec <- newIORef $ Just ("Seq", CalDescStore{..})
    return Config{..}


main :: IO ()
main = do
    c <- readFile "Seq.calc"
    r <- readFile "Seq.rules"
    config <- mkConfig c r
    run 8081 $ app config

