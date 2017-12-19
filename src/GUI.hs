{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GUI where

import           Lib.Prelude
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Network.Wai.Middleware.Cors

import           Servant
import           Servant.JS
-- import           Servant.Foreign
import           Data.Aeson
import           Data.IORef
import           Servant.JS.Internal

import           Rules
import           Terms
import           Terms.JSON
import           Terms.Parsers

import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.Map                    as M

-- import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
-- import Text.Earley
import System.IO (hSetNewlineMode, universalNewlineMode)
import System.FilePath.Posix((</>), (<.>))
import Data.Text.IO (hGetContents)

data GUIError = CalculusDescriptionNotLoaded
              | InvalidName Text
              -- | TermParseE TermParseError
              | Custom Text deriving (Show, Generic, ToJSON)

deriving instance Exception GUIError

data Status = Success deriving (Generic, ToJSON)


type API = "parseDSeq" :> QueryParam "val" Text :> Get '[JSON] (LatexDSeq [Rule Text] 'ConcreteK)
      :<|> "macros" :> Get '[JSON] Macros
      :<|> "applicableRules" :> ReqBody '[JSON] (DSequent 'ConcreteK Text) :> Post '[JSON] [(RuleName , [LatexDSeq [Rule Text] 'ConcreteK])]
      :<|> "calcDesc" :> Get '[JSON] CalcDesc
      :<|> "calcDesc" :> ReqBody '[JSON] CalcDesc :> Post '[JSON] Status
      :<|> ProofSearchAPI


api :: Proxy API
api = Proxy


data CalDescStore c = CalDescStore {
    parsed   :: c
  , rawCalc  :: Text
  , rawRules :: Text
}


type PTDSeq = PT (DSequent 'ConcreteK Text)
type LatexPTDSeq = PT (LatexDSeq [Rule Text] 'ConcreteK)

data Config r = Config {
    currentCalcDec :: IORef (Text, CalDescStore (FinTypeCalculusDescription r))
  , psQueue        :: IORef (IntMap (Either ThreadId (Maybe PTDSeq)))
  , psQueueCounter :: IORef Int
  , calcFolder     :: FilePath
}


getQueue :: AppM r (IORef (IntMap (Either ThreadId (Maybe PTDSeq))))
getQueue = AppM $ \Config{..} -> return psQueue

getIntoQueue :: AppM r Int
getIntoQueue = AppM $ \Config{..} ->
    atomicModifyIORef' psQueueCounter $ \i -> (i+1 , i)

enqueuePS :: Int -> IORef (IntMap (Either ThreadId (Maybe PTDSeq))) -> IO ()
enqueuePS i psQueue = do
    tid <- myThreadId
    atomicModifyIORef' psQueue $ \m ->
        case IntMap.lookup i m of
            Nothing -> (IntMap.insert i (Left tid) m, ())
            Just _  -> (m, ()) -- if i has already been set, then ps finished before we atomically set the threadId


enqueueResult :: IORef (IntMap (Either ThreadId (Maybe PTDSeq))) -> Int -> Maybe PTDSeq -> IO ()
enqueueResult ref i pt = do
    -- print "adding result"
    atomicModifyIORef' ref $ \m -> (IntMap.insert i (Right pt) m, ())


cancelPS :: Int -> AppM r ()
cancelPS i = AppM $ \Config{..} -> do
    v <- atomicModifyIORef' psQueue $ \m -> case IntMap.lookup i m of
      Just (Left tid) -> (IntMap.delete i m, Just tid)
      _               -> (m , Nothing)
    case v of
        Just tid -> do
            killThread tid
            print ("search terminated" :: Text)
            q <- readIORef psQueue
            print q
        -- Nothing -> ?? This has been canceled already... or the thread hadnt been added to the IntMap yet??
        Nothing  -> return () -- thread has finished and stored the result in the map
    -- print $ ("cancelled ps for: " <> show i :: Text)

runPS :: Int -> Set (DSequent 'ConcreteK Text) -> DSequent 'ConcreteK Text -> AppM [Rule Text] Int
runPS maxDepth prems dseq = do
    (_, CalDescStore{..}) <- get
    q <- getQueue
    qi <- getIntoQueue
    _ <- liftIO $ forkFinally (do
        enqueuePS qi q
        r <- liftM (head . take 1) $ runReaderT (findProof 0 maxDepth prems dseq) parsed
        print r
        return r) (handleRes q qi)
    -- enqueuePS qi tid
    -- print ("returning " <> show qi :: Text)
    return qi
    where
        handleRes _ _ (Left e) = throwIO e -- could instead store the excpetion in the intmap??
        handleRes qref qi (Right r) = do
            enqueueResult qref qi r


tryDequeue :: Int -> AppM r (Maybe PTDSeq)
tryDequeue i = AppM $ \Config{..} -> do
    -- print ("trying dequeue for " <> show i :: Text)
    atomicModifyIORef' psQueue $ \m ->
        case IntMap.lookup i m of
            Nothing          -> (m, Nothing)
            Just (Left _)    -> (m, Nothing)
            Just (Right res) -> (IntMap.delete i m, res)
    -- print ("finish dequeue" <> show r :: Text)
    -- case r of
    --     Nothing -> return $ []
    --     Just r -> return $ [r]



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
    put cds = AppM $ \Config{..} -> writeIORef currentCalcDec $ cds
    get = AppM $ \Config{..} -> readIORef currentCalcDec


instance MonadReader (Config r) (AppM r) where
    ask = AppM $ \c -> return c
    local f m = AppM $ \c -> runAppM m (f c)


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
    if name == "" then throw $ InvalidName name else return ()
    cd <- parseFinTypeCalculusDescription rawCalc
    rules <- runReaderT (parseRules rawRules) cd
    -- print rules
    put (name , CalDescStore cd{rules = rules} rawCalc rawRules)
    -- write the calc to a file
    cFolder <- asks calcFolder
    print cFolder

    liftIO $ writeFile (cFolder ++ "/" ++ toS name ++ ".calc") rawCalc
    liftIO $ writeFile (cFolder ++ "/" ++ toS name ++ ".rules") rawRules
    return GUI.Success


type ProofSearchAPI = "launchPS" :> ReqBody '[JSON] (Int, Set (DSequent 'ConcreteK Text) , DSequent 'ConcreteK Text) :> Post '[JSON] Int
                 :<|> "cancelPS" :> ReqBody '[JSON] Int :> Post '[JSON] ()
                 :<|> "queryPSResult" :> ReqBody '[JSON] Int :> Post '[JSON] [LatexPTDSeq]


launchPSH :: (Int, Set (DSequent 'ConcreteK Text), DSequent 'ConcreteK Text) -> AppM [Rule Text] Int
launchPSH (maxDepth, prems, dseq) = runPS maxDepth prems dseq

cancelPSH :: Int -> AppM [Rule Text] ()
cancelPSH = cancelPS

queryPSResultH :: Int -> AppM [Rule Text] [LatexPTDSeq]
queryPSResultH i = do
    r <- tryDequeue i
    case r of
        Nothing -> return []
        Just r' -> do
            (_, CalDescStore{..}) <- get
            return [map (LatexDSeq . (parsed,)) r']


serverPS :: ServerT ProofSearchAPI (AppM [Rule Text])
serverPS = launchPSH :<|> cancelPSH :<|> queryPSResultH


ioToHandler :: Config r -> AppM r :~> Handler
ioToHandler cfg = NT $ \x -> Handler . ExceptT . try $ runAppM x cfg


readerServer :: Config [Rule Text] -> Server API
readerServer cfg = enter (ioToHandler cfg) server

server :: ServerT API (AppM [Rule Text])
server = parseDSeqH :<|> getMacrosH :<|> getApplicableRulesH :<|> getCaclDescH :<|> setCaclDescH
    :<|> serverPS

myCors :: Middleware
myCors = cors $ const $ Just customPolicy
    where
        customPolicy =
            simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "Cache-Control"] }



app :: Config [Rule Text] -> Application
app cfg = myCors (serve api (readerServer cfg))




instance MonadThrowJSON (ReaderT x IO) where
    throw = throwIO

runCalcIO :: forall r a. FinTypeCalculusDescription r -> CalcMT r IO a -> IO a
runCalcIO env = \rT -> runReaderT rT env


-- this hack needs to go at some point?? / rework to be safe...
mkConfig :: Text -> Text -> Text -> FilePath -> IO (Config [Rule Text])
mkConfig name rawCalc rawRules calcFolder = do
    c <- parseFinTypeCalculusDescription rawCalc
    rs <- runCalcIO c (parseRules rawRules)
    let parsed = c{rules = rs}
    psQueue <- newIORef $ IntMap.empty
    currentCalcDec <- newIORef $ (name, CalDescStore{..})
    psQueueCounter <- newIORef 1 -- js is stupid, so we start from 1
    return Config{..}

-- this hack needs to go at some point?? / rework to be safe...
mkConfigEmpty :: FilePath -> IO (Config [Rule Text])
mkConfigEmpty calcFolder = do
    let rawCalc = ""
        rawRules = ""
        parsed = emptyFinTypeCalculusDescription []
    psQueue <- newIORef $ IntMap.empty
    currentCalcDec <- newIORef $ ("", CalDescStore{..})
    psQueueCounter <- newIORef 1 -- js is stupid, so we start from 1
    return Config{..}


writeJSCode :: Int -> [Char] -> IO ()
writeJSCode port jsPath = writeJSForAPI api (vanillaJSWithNoCache) $ jsPath
    where
        -- adds a Cache-Control header to the generator options, which results in
        -- xhr.setRequestHeader("Cache-Control", headerCacheControl);
        -- being added to all the JS functions
        vanillaJSWithNoCache :: [Req NoContent] -> Text
        vanillaJSWithNoCache xs = vanillaJSWith myOptions $ runIdentity $ mapM
            (reqHeaders $ Identity . ((HeaderArg $ Arg (PathSegment "Cache-Control") NoContent):)) xs
        myOptions = defCommonGeneratorOptions{urlPrefix = "http://localhost:" <> show port, Servant.JS.Internal.moduleName="exports"}


-- converts line endings to \n to ensure consistent decoding...
-- should fix issues with parsing?
readFile' :: FilePath -> IO Text
readFile' name = do
    h <- openFile name ReadMode
    hSetNewlineMode h universalNewlineMode 
    hGetContents h



runGUI :: FilePath -> FilePath -> IO ()
runGUI calcFolder calcName = do
    -- [calcFolder, jsFolder] <- getArgs
    c <- readFile' $ calcFolder </> (calcName System.FilePath.Posix.<.> "calc")
    r <- readFile' $ calcFolder </> (calcName System.FilePath.Posix.<.> "rules")
    config <- mkConfig (toS calcName) c r calcFolder
    -- writeJSCode 8081 jsFolder
    run 8081 $ app config

runEmptyGUI :: FilePath -> IO ()
runEmptyGUI calcFolder = do
    config <- mkConfigEmpty calcFolder
    run 8081 $ app config

