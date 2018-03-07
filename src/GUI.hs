{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module GUI where

import           Lib.Prelude
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Network.Wai.Middleware.Cors

import           Servant
import           Servant.JS
import           Servant.JS.Custom
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
import qualified Data.Set                    as S

-- import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
-- import Text.Earley
import System.FilePath.Posix((</>), dropExtension, takeExtension)
import qualified System.FilePath.Posix as FP
import System.Directory(listDirectory, removeFile, doesFileExist)

data GUIError = CalculusDescriptionNotLoaded
              | InvalidName Text
              | FileDoesNotExist FilePath
              | CircularImport (Set Text)
              -- | TermParseE TermParseError
              | Custom Text deriving (Show, Generic, ToJSON)

deriving instance Exception GUIError



data ParseTerm o = ParseTerm {
    text :: Text,
    opts :: o
} deriving (Generic, FromJSON)

type API = "parseDSeq" :> ReqBody '[JSON] (ParseTerm AbbrevsMapInternal) :>  Post '[JSON] (LatexDSeq (FinTypeCalculusDescription [Rule Text]) 'ConcreteK)
      :<|> "parseFormula" :> ReqBody '[JSON] (ParseTerm (CalcType, AbbrevsMapInternal)) :> Post '[JSON] (LatexTerm (FinTypeCalculusDescription [Rule Text]) (Term 'FormulaL 'ConcreteK Text))
      :<|> "macros" :> Get '[JSON] Macros
      :<|> "applicableRules" :> ReqBody '[JSON] (DSequent 'ConcreteK Text) :> Post '[JSON] [(RuleName , [LatexDSeq (FinTypeCalculusDescription [Rule Text]) 'ConcreteK])]
      :<|> "applyCut" :> ReqBody '[JSON] (DSequent 'ConcreteK Text, Term 'FormulaL 'ConcreteK Text) :> Post '[JSON] [(RuleName , [LatexDSeq (FinTypeCalculusDescription [Rule Text]) 'ConcreteK])]
      :<|> "exportPTtoLatex" :> ReqBody '[JSON] (PT (LatexDSeq () 'ConcreteK)) :> Post '[JSON] Text
      :<|> CalculusDescriptionAPI
      :<|> ProofSearchAPI


api :: Proxy API
api = Proxy


data CalDescStore c = CalDescStore {
    parsed   :: c
  , rawCalc  :: Text
  , rawRules :: Text
}


type PTDSeq = PT (DSequent 'ConcreteK Text)
type PTLatexDSeq = PT (LatexDSeq (FinTypeCalculusDescription [Rule Text]) 'ConcreteK)


type AbbrevsMapLatex = AbbrevsMap (LatexTerm [Rule Text] (Term 'FormulaL 'ConcreteK Text)) (LatexTerm [Rule Text] (Term 'StructureL 'ConcreteK Text))


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

-- instance MonadThrowJSON (ReaderT (FinTypeCalculusDescription r) (AppM r')) where
--     throw e = ReaderT $ \_ -> throwIO $ err300 {errBody = encode e }


parseDSeqH :: ParseTerm AbbrevsMapInternal -> AppM r (LatexDSeq (FinTypeCalculusDescription r) 'ConcreteK)
parseDSeqH ParseTerm{..} = freeze $ do
    -- print $ tokenize $ toS text
    r <- parseCDSeq opts text
    cd <- ask
    return $ LatexTerm (cd,r)
-- parseDSeqH Nothing = throwIO $ err300 {errBody = "No input given"} -- redo this properly??


parseFormulaH :: ParseTerm (CalcType, AbbrevsMapInternal) -> AppM r (LatexTerm (FinTypeCalculusDescription  r) (Term 'FormulaL 'ConcreteK Text))
parseFormulaH ParseTerm{..} = freeze $ do
    r <- parseFormula opts text
    cd <- ask
    return $ LatexTerm (cd,r)


getMacrosH :: AppM r Macros
getMacrosH = freeze $ do
    fconns <- asks formulaConns
    sconns <- asks structureConns
    ms <- asks macros
    return $ Macros $ (M.map snd ms) `M.union` (M.fromList $
        (map (\(ConnDescription n _ _ _ _ _ l) -> ("\\seq" <> n, l)) fconns) ++
        (map (\(ConnDescription n _ _ _ _ _ l) -> ("\\seq" <> n, l)) sconns))



getApplicableRulesH :: DSequent 'ConcreteK Text -> AppM [Rule Text] [(RuleName , [LatexDSeq (FinTypeCalculusDescription [Rule Text]) 'ConcreteK])]
getApplicableRulesH dseq = freeze $ do
    cd <- ask
    -- print cd
    r <- getApplicableRules dseq
    -- print r
    return $ M.toList $ (M.map . map) (\s -> LatexTerm (cd,s)) r


applyCutH :: (DSequent 'ConcreteK Text , Term 'FormulaL 'ConcreteK Text) -> AppM [Rule Text] [(RuleName , [LatexDSeq (FinTypeCalculusDescription [Rule Text]) 'ConcreteK])]
applyCutH (dseq@(DSeq _ typ _) , cutFormula) = freeze $ do
    cd <- ask
    let (Rule _ (Just latexName) premises conclusion) = cutRule typ
    let appR = do {
            udict <- unifySeq conclusion dseq ;
            mapM (subSeq (M.insert "A" (Lift cutFormula) udict)) premises
        }
    case appR of 
        Just [l,r] -> return [(latexName , [LatexTerm (cd,l), LatexTerm (cd,r)])]
        _ -> return []
    -- -- print r
    -- return $ M.toList $ (M.map . map) (\s -> LatexDSeq (cd,s)) r



exportPTtoLatexH :: PT (LatexDSeq () 'ConcreteK) -> AppM r Text
exportPTtoLatexH pt = freeze $ do
    pretty <- pprint (map (snd . unMk) pt)
    print pretty
    return pretty


type CalculusDescriptionAPI = "calcDesc"    :> Get '[JSON] CalcDesc
                         :<|> "getTypes" :> Get '[JSON] (Set CalcType)
                         :<|> "modifyCalc"  :> ReqBody '[JSON] CalcDesc :> Post '[JSON] ()
                         :<|> "deleteCalc"  :> ReqBody '[JSON] FilePath :> Post '[JSON] ()
                         :<|> "loadCalc"    :> ReqBody '[JSON] FilePath :> Post '[JSON] ()
                         :<|> "listCalculi" :> Get '[JSON] [FilePath]


caclDescH :: AppM r CalcDesc
caclDescH = do
    (name, CalDescStore{..}) <- get
    return $ CalcDesc name rawCalc rawRules

getTypesH :: AppM r (Set CalcType)
getTypesH = do
    (_, CalDescStore{..}) <- get
    return $ types parsed


modifyCalcH :: CalcDesc -> AppM [Rule Text] ()
modifyCalcH CalcDesc{..} = do
    when (name == "") $ throw $ InvalidName name
    cFolder <- asks calcFolder
    cd <- loadCalcRec cFolder (S.singleton name) rawCalc rawRules
    -- rules <- runReaderT (parseRules rawRules) cd
    print cd
    put (name , CalDescStore cd rawCalc rawRules)
    -- write the calc to a file
    -- cFolder <- asks calcFolder
    -- print cFolder

    liftIO $ writeFile (cFolder </> toS name FP.<.> ".calc") rawCalc
    liftIO $ writeFile (cFolder </> toS name FP.<.> ".rules") rawRules


deleteCalcH :: FilePath -> AppM r ()
deleteCalcH f = do
    workDir <- asks calcFolder
    deleteFile $ workDir </> f FP.<.> "calc"
    deleteFile $ workDir </> f FP.<.> "rules"

    where
        deleteFile file = do
            exists <- liftIO $ doesFileExist $ file
            unless exists $ throw $ FileDoesNotExist file
            liftIO $ removeFile file


loadCalcRec ::
    FilePath -> Set Text -> Text -> Text -> AppM [Rule Text] (FinTypeCalculusDescription [Rule Text])
loadCalcRec workDir previousDeps rawCalc rawRules = do
    -- rawCalc  <- readF $ workDir </> (toS f) FP.<.> "calc"
    -- rawRules <- readF $ workDir </> (toS f) FP.<.> "rules"

    imports <- parseFinTypeCalculusDescriptionImports rawCalc
    print imports

    when ((previousDeps `S.intersection` imports) /= S.empty) $ 
        throw $ CircularImport (previousDeps `S.intersection` imports)

    cdImports <- foldM foldImports (emptyFinTypeCalculusDescription []) imports

    cd       <- parseFinTypeCalculusDescription rawCalc
    rules    <- runReaderT (parseRules rawRules) cd
    return $ cdImports <> cd{rules = rules}

    where
        readF file = do
            exists <- liftIO $ doesFileExist $ file
            unless exists $ throw $ FileDoesNotExist file
            liftIO $ readFile' file

        foldImports :: 
            FinTypeCalculusDescription [Rule Text] -> Text -> AppM [Rule Text] (FinTypeCalculusDescription [Rule Text])
        foldImports cd i = do
            rawCalc'  <- readF $ workDir </> (toS i) FP.<.> "calc"
            rawRules' <- readF $ workDir </> (toS i) FP.<.> "rules"
            cd' <- loadCalcRec workDir (S.insert i previousDeps) rawCalc' rawRules'
            return $ cd <> cd'


loadCalcH :: FilePath -> AppM [Rule Text] ()
loadCalcH f = do
    workDir  <- asks calcFolder
    rawCalc  <- readF $ workDir </> (toS f) FP.<.> "calc"
    rawRules <- readF $ workDir </> (toS f) FP.<.> "rules"
    cd <- loadCalcRec workDir (S.singleton $ toS f) rawCalc rawRules
    -- print rules
    put (toS f , CalDescStore cd rawCalc rawRules)

    where
        readF file = do
            exists <- liftIO $ doesFileExist $ file
            unless exists $ throw $ FileDoesNotExist file
            liftIO $ readFile' file


listCalculiH :: AppM r [FilePath]
listCalculiH = do
    workDir <- asks calcFolder
    cs <- liftIO $ listDirectory workDir
    let cs' = filter (\x -> takeExtension x  == ".calc" || takeExtension x  == ".rules") cs
    return $ (filterCalc . sort . map dropExtension) cs'

    where
        filterCalc [] = []
        filterCalc [_] = []
        filterCalc (x:y:xs) | x == y = x : filterCalc xs
        filterCalc (_:y:xs) | otherwise = filterCalc (y:xs)

serverCalcDesc :: ServerT CalculusDescriptionAPI (AppM [Rule Text])
serverCalcDesc = caclDescH :<|> getTypesH :<|> modifyCalcH :<|> deleteCalcH :<|> loadCalcH :<|> listCalculiH


type ProofSearchAPI = "launchPS"      :> ReqBody '[JSON] (Int, Set (DSequent 'ConcreteK Text) , DSequent 'ConcreteK Text) :> Post '[JSON] Int
                 :<|> "cancelPS"      :> ReqBody '[JSON] Int :> Post '[JSON] ()
                 :<|> "queryPSResult" :> ReqBody '[JSON] Int :> Post '[JSON] [PTLatexDSeq]


launchPSH :: (Int, Set (DSequent 'ConcreteK Text), DSequent 'ConcreteK Text) -> AppM [Rule Text] Int
launchPSH (maxDepth, assms, dseq) = runPS maxDepth assms dseq

cancelPSH :: Int -> AppM [Rule Text] ()
cancelPSH = cancelPS

queryPSResultH :: Int -> AppM [Rule Text] [PTLatexDSeq]
queryPSResultH i = do
    r <- tryDequeue i
    case r of
        Nothing -> return []
        Just r' -> do
            (_, CalDescStore{..}) <- get
            return [map (LatexTerm . (parsed,)) r']


serverPS :: ServerT ProofSearchAPI (AppM [Rule Text])
serverPS = launchPSH :<|> cancelPSH :<|> queryPSResultH


ioToHandler :: Config r -> AppM r :~> Handler
ioToHandler cfg = NT $ \x -> Handler . ExceptT . try $ runAppM x cfg


readerServer :: Config [Rule Text] -> Server API
readerServer cfg = enter (ioToHandler cfg) server

server :: ServerT API (AppM [Rule Text])
server = parseDSeqH :<|> parseFormulaH :<|> getMacrosH :<|> getApplicableRulesH :<|> applyCutH :<|> exportPTtoLatexH :<|> serverCalcDesc :<|> serverPS

myCors :: Middleware
myCors = cors $ const $ Just customPolicy
    where
        customPolicy =
            simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "Cache-Control"] }



app :: Config [Rule Text] -> Application
app cfg = myCors (serve api (readerServer cfg))




-- instance MonadThrowJSON (ReaderT x IO) where
--     throw = throwIO

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


writeJSCode :: FilePath -> IO ()
writeJSCode jsPath = writeJSForAPI api (customJSWith myOptions) $ jsPath
    where
        -- adds a Cache-Control header to the generator options, which results in
        -- xhr.setRequestHeader("Cache-Control", headerCacheControl);
        -- being added to all the JS functions
        -- customJSWithNoCache :: [Req NoContent] -> Text
        -- customJSWithNoCache xs = customJSWith myOptions $ runIdentity $ mapM
        --     (reqHeaders $ Identity . ((HeaderArg $ Arg (PathSegment "Cache-Control") NoContent):)) xs
        myOptions = defCommonGeneratorOptions{
            urlPrefix = "http://localhost:${port}", 
            Servant.JS.Internal.moduleName="exports"}


-- checks that the calculus+rule files exist and launches runEmptyGUI if the don't, 
    -- otherwise it reads+parses the file and launches gui with the given calc
runGUI :: FilePath -> FilePath -> Int -> IO ()
runGUI calcFolder calcName port = do
    cFEx <- doesFileExist calcFile
    rFEx <- doesFileExist ruleFile
    if (cFEx && rFEx) then do
        c <- readFile' $ calcFile
        r <- readFile' $ ruleFile
        config <- mkConfig (toS calcName) c r calcFolder
        -- writeJSCode 8081 jsFolder
        run port $ app config
    else runEmptyGUI calcFolder port

    where
        calcFile = calcFolder </> calcName FP.<.> "calc"
        ruleFile = calcFolder </> calcName FP.<.> "rules"

runEmptyGUI :: FilePath -> Int -> IO ()
runEmptyGUI calcFolder port = do
    config <- mkConfigEmpty calcFolder
    run port $ app config


data DebugConfig = DebugConfig {
    workDir :: [FilePath],
    currentCalc :: [Char]
} deriving (Generic, FromJSON)

debugMac :: IO ()
debugMac = do
    f <- readFile "/Users/goodlyrottenapple/Library/Application Support/CalculusToolbox/config.json"
    let Just (DebugConfig [wd] cc) = decode (toS f)
    putStrLn $ "Launching from: " ++ wd
    putStrLn $ "Current calculus is: " ++ cc
    GUI.runGUI wd cc 8081

