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
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveFunctor      #-}

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
import Control.Monad.Reader
import Text.Earley

data GUIError = CalculusDescriptionNotLoaded
              -- | TermParseE TermParseError
              | Custom Text deriving (Show, Generic, ToJSON)

deriving instance Exception GUIError 

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


newtype AppM r a = AppM { runAppM :: Config r -> IO a } deriving Functor

instance Applicative (AppM r) where
    pure a = AppM $ \_ -> return a
    fab <*> fa = AppM $ \c -> 
        let ioab = runAppM fab c in ioab <*> (runAppM fa c)



instance Monad (AppM r) where
    return a = AppM $ \_ -> return a
    ma >>= amb = AppM $ \c -> do
        a <- runAppM ma c
        runAppM (amb a) c


instance MonadIO (AppM r) where
    liftIO ioa = AppM (\_ -> ioa)

instance MonadThrowJSON (AppM r) where
    throw e = throwIO $ err300 {errBody = encode e }

instance MonadThrowJSON IO where
    throw e = throwIO e


instance MonadState (Text, CalDescStore (FinTypeCalculusDescription r)) (AppM r) where
    put cds = AppM (\Config{..} -> writeIORef currentCalcDec $ Just cds)
    get = AppM (\Config{..} -> do
        r <- readIORef currentCalcDec
        case r of 
            Nothing -> throwIO $ err500 {errBody = encode CalculusDescriptionNotLoaded}
            Just res -> return res)


-- instance MonadReader (FinTypeCalculusDescription r) (AppM r) where
--     ask = AppM (\Config{..} -> do
--         r <- readIORef currentCalcDec
--         case r of 
--             Nothing -> throwIO $ err500 {errBody = encode CalculusDescriptionNotLoaded}
--             Just (_, CalDescStore{..}) -> return parsed)


-- since we use an ioref, the AppM is not really a Reader, because the value of 
-- the calculus description could change arbitrarily mid-computation, especially if there are
-- multiple accesses by different threads?? the following function should ensure that the calculus
-- description at least stays consistent throughout the computation, i.e. ask will always 
-- return the same value inside the freeze block
freeze :: MonadState (Text, CalDescStore r) m => ReaderT r m a -> m a
freeze m = do
    (_, CalDescStore{..}) <- get
    runReaderT m parsed 

instance MonadThrowJSON (ReaderT (FinTypeCalculusDescription r) (AppM r')) where
    throw e = ReaderT $ \_ -> throwIO $ err300 {errBody = encode e }


parseDSeqH :: Maybe Text -> AppM r (LatexDSeq r 'ConcreteK)
parseDSeqH (Just inStr) = freeze $ do
    r <- parseCDSeq inStr
    cd <- ask
    return $ LatexDSeq (cd,r)
parseDSeqH Nothing = throwIO $ err300 {errBody = "No input given"} -- redo this properly??


getMacrosH :: AppM r Macros
getMacrosH = freeze $ do
    fconns <- asks formulaConns
    sconns <- asks structureConns
    return $ Macros $ M.fromList $ 
        (map (\(ConnDescription n _ _ _ _ _ l) -> ("\\seq" <> n, l)) fconns) ++
        (map (\(ConnDescription n _ _ _ _ _ l) -> ("\\seq" <> n, l)) sconns)



getApplicableRulesH :: DSequent 'ConcreteK Text -> AppM [Rule Text] [(RuleName , [LatexDSeq [Rule Text] 'ConcreteK])]
getApplicableRulesH dseq = freeze $ do
    cd <- ask
    -- print cd
    r <- getApplicableRules dseq
    -- print r
    return $ M.toList $ (M.map . map) (\s -> LatexDSeq (cd,s)) r



getCaclDescH :: AppM r CalcDesc
getCaclDescH = do
    (name, CalDescStore{..}) <- get
    return $ CalcDesc name rawCalc rawRules



setCaclDescH :: CalcDesc -> AppM [Rule Text] Status
setCaclDescH CalcDesc{..} = do
    cd <- parseFinTypeCalculusDescription rawCalc
    rules <- runReaderT (parseRules rawRules) cd

    put (name , CalDescStore cd{rules = rules} rawCalc rawRules)
    -- write the calc to a file
    liftIO $ writeFile ("./calculi/" ++ toS name ++ ".calc") rawCalc
    liftIO $ writeFile ("./calculi/" ++ toS name ++ ".rules") rawRules
    return GUI.Success


type ProofSearchAPI = "launchPS" :> ReqBody '[JSON] (DSequent 'ConcreteK Text) :> Post '[JSON] Int
         :<|> "cancelPS" :> ReqBody '[JSON] Int :> Post '[JSON] Status
         :<|> "queryPSResult" :> ReqBody '[JSON] Int :> Post '[JSON] ()





ioToHandler :: Config r -> AppM r :~> Handler
ioToHandler cfg = NT $ \x -> Handler . ExceptT . try $ runAppM x cfg


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




instance MonadThrowJSON (ReaderT x IO) where
    throw = throwIO

runCalcIO :: forall r a. FinTypeCalculusDescription r -> CalcMT r IO a -> IO a
runCalcIO env = \rT -> runReaderT rT env


-- this hack needs to go at some point??
mkConfig :: Text -> Text -> IO (Config [Rule Text])
mkConfig rawCalc rawRules = do
    c <- parseFinTypeCalculusDescription rawCalc
    rs <- runCalcIO c (parseRules rawRules)
    let parsed = c{rules = rs}
    currentCalcDec <- newIORef $ Just ("Sequent", CalDescStore{..})
    return Config{..}


test = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) api



writeJSCode :: Int -> IO ()
writeJSCode port = writeJSForAPI api (vanillaJSWithNoCache) "./gui/src/ServantApi.js"
    where
        -- adds a Cache-Control header to the generator options, which results in
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

