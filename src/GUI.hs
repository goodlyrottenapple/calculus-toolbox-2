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
{-# LANGUAGE FlexibleContexts      #-}


module GUI where

import Lib.Prelude
import Network.Wai
import Network.Wai.Handler.Warp

import Network.Wai.Middleware.Cors

import Servant
import Servant.JS
-- import           Servant.Foreign
import           Servant.JS.Internal
import Data.IORef
import Data.Aeson

import Terms
import Terms.Parsers
import Terms.JSON
import Rules

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except

import Text.Earley

data GUIError = CalculusDescriptionNotLoaded
              -- | TermParseE TermParseError
              | Custom Text deriving (Generic, ToJSON)


data Status = Success deriving (Generic, ToJSON)


type API = "parseDSeq" :> QueryParam "val" Text :> Get '[JSON] (LatexDSeq [Rule Text] 'ConcreteK)
         :<|> "macros" :> Get '[JSON] Macros
         :<|> "applicableRules" :> ReqBody '[JSON] (DSequent 'ConcreteK Text) :> Post '[JSON] [(RuleName , [LatexDSeq [Rule Text] 'ConcreteK])]
         :<|> "calcDesc" :> Get '[JSON] CalcDesc
         :<|> "calcDesc" :> ReqBody '[JSON] CalcDesc :> Post '[JSON] Status


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


type AppM r = ReaderT (Config r) IO


getCaclDesc :: AppM r (FinTypeCalculusDescription r)
getCaclDesc = do
    cRef <- asks currentCalcDec
    cd <- liftIO $ readIORef cRef
    case cd of 
        Just (_, CalDescStore{..}) -> return parsed
        Nothing -> throwIO $ err500 {errBody = encode CalculusDescriptionNotLoaded}

putCaclDesc :: Text -> Text -> Text -> FinTypeCalculusDescription r -> AppM r ()
putCaclDesc name rawCalc rawRules parsed = do
    cRef <- asks currentCalcDec
    liftIO $ writeIORef cRef $ Just (name, CalDescStore{..})

liftErrAppM :: ToJSON e => Except e a -> AppM r a
liftErrAppM x = do
    case runExcept x of
        Left err -> throwIO $ err300 {errBody = encode err}
        Right res -> return res
     


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


setCaclDescH :: CalcDesc -> AppM [Rule Text] Status
setCaclDescH CalcDesc{..} = do
    cd <- liftErrAppM $ parseFinTypeCalculusDescription rawCalc
    -- rules <- liftErrAppM $ runReaderT (parseRules rawRules) cd
    rules <- parse cd $ (splitRules . toS) rawRules

    putCaclDesc name rawCalc rawRules cd{rules = rules}
    -- write the calc to a file
    liftIO $ writeFile ("./calculi/" ++ toS name ++ ".calc") rawCalc
    liftIO $ writeFile ("./calculi/" ++ toS name ++ ".rules") rawRules
    return GUI.Success

    where
        parse :: FinTypeCalculusDescription x -> [[Char]] -> ReaderT (Config [Rule Text]) IO [Rule Text]  
        -- parse :: FinTypeCalculusDescription x -> [[Char]] -> ReaderT (Config [Rule Text]) IO ()
        parse _ [] = return []
        parse e (r:rs) = do
            r' <- case fullParses (parser $ runReaderT grammarRule e) $ tokenize $ r of
                ([] , rep) -> throwIO $ err300 {errBody = encode $ TermParserError rep} 
                (prules , rep) -> do
                    -- print prules
                    prulesMetaMap <- liftCalcErrAppM $ mapM typeableRule prules
                    fixed <- liftCalcErrAppM $ mapM (\(m,r) -> fixRule m r)(zip prulesMetaMap prules)
                    case fixed of
                        []  -> throwIO $ err300 {errBody = encode $ RuleUntypeable name}
                        [p] -> do
                            print p
                            return p 
                        ps' -> throwIO $ err300 {errBody = encode $ AmbiguousRuleParse ps'}
            -- case fullParses (parser $ runReaderT grammarRule e) $ tokenize $ r of
            --     ([] , rep) -> throwIO $ err300 {errBody = encode $ TermParserError rep} 
            --     (ps@(Rule{..}:_) , _)   -> do
            --         ps' <-  liftCalcErrAppM @() $ filterTypeable typeableRule ps
            --         print $ length ps'
            --         case sort ps' of
            --             []    -> throwIO $ err300 {errBody = encode $ RuleUntypeable name}
            --             [p]   -> return p
            --             (p:_) -> return p
            rs' <- parse e rs
                -- parse e rs
            return $ r':rs'




ioToHandler :: Config r -> AppM r :~> Handler
ioToHandler cfg = NT $ \x -> Handler . ExceptT . try $ runReaderT x cfg


readerServer :: Config [Rule Text] -> Server API
readerServer cfg = enter (ioToHandler cfg) server

server :: ServerT API (AppM [Rule Text])
server = parseDSeqH :<|> getMacrosH :<|> getApplicableRulesH :<|> getCaclDescH :<|> setCaclDescH 

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
    currentCalcDec <- newIORef $ Just ("Sequent", CalDescStore{..})
    return Config{..}


test = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) api



writeJSCode :: Int -> IO ()
writeJSCode port = writeJSForAPI api (vanillaJSWithNoCache) "./gui/src/ServantApi.js"
    where
        -- adds a Cache-Control header to the function, this results in:
        -- xhr.setRequestHeader("Cache-Control", headerCacheControl);
        -- being added to all the JS functions
        vanillaJSWithNoCache :: [Req NoContent] -> Text
        vanillaJSWithNoCache xs = vanillaJSWith myOptions $ runIdentity $ mapM (reqHeaders (\xs -> Identity $ (HeaderArg (Arg (PathSegment "Cache-Control") NoContent)):xs)) xs
        myOptions = defCommonGeneratorOptions{urlPrefix = "http://localhost:" <> show port, moduleName="exports"}


main :: IO ()
main = do
    c <- readFile "calculi/Sequent.calc"
    r <- readFile "calculi/Sequent.rules"
    config <- mkConfig c r
    writeJSCode 8081
    run 8081 $ app config

