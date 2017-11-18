{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE RecordWildCards    #-}

module Terms.JSON where

import Terms
import Lib.Prelude
import Data.Aeson
import qualified Prelude as P
import Data.Singletons.TH
import GHC.TypeLits



instance ToJSON a => ToJSON (Vec n a) where
    toJSON = toJSON . toList



-- instance (KnownNat n, SingI f, KnownSymbol p, KnownNat b) => ToJSON (HomCon v c n f p b l) where
--     toJSON c = object [
--             -- "name" .= ("" :: Text) , --(show c :: Text),
--             "noOfArgs" .= getNoOfArgs c,
--             "fixity" .= getFixity c,
--             "parserSymbol" .= getParserSymbol c,
--             "binding" .= getBinding c
--         ]


-- instance ToJSON a => ToJSON (Term v l m a) where
--     toJSON t@(MetaV x) = object [
--             "type" .= ("Meta" :: Text),
--             -- "level" .= levelT t,
--             "value" .= toJSON x
--         ]
--     toJSON t@(Base x) = object [
--             "type" .= ("Base" :: Text),
--             -- "level" .= levelT t,
--             "value" .= x
--         ]
--     toJSON t@(Lift x) = object [
--             "type" .= ("Lift" :: Text),
--             "level" .= getLevelT t,
--             "value" .= x
--         ]
--     toJSON t@(HomC c ts) = object [
--             "type" .= ("HomC" :: Text),
--             -- "level" .= levelT t,
--             "con" .= c,
--             "value" .= ts
--         ]




-- -- instance (KnownNat n, FromJSON a) => FromJSON (Vec n a) where
-- --      parseJSON j = do
-- --         l <- parseJSON j
-- --         case toVec Proxy l of
-- --             Just v -> return v
-- --             Nothing -> P.fail "The vector length does not match"


-- instance FromJSON a => FromJSON (MetaTerm v 'AtomL a) where
--     parseJSON (Object o) = do
--         (t :: Text) <- o .: "type"
--         case t of
--             "Meta" -> do
--                 v <- o .: "value"
--                 return $ MetaV v
--             -- "Lift" -> do
--             --     v  <- o .: "value"
--             --     return $ Lift v
--             _ -> P.fail "not a valid meta term"
--     parseJSON _ = P.fail "was expecting an object"

-- instance (FromJSON a, MkHomCon v 'FormulaL) => FromJSON (MetaTerm v 'FormulaL a) where
--     parseJSON (Object o) = do
--         (t :: Text) <- o .: "type"
--         case t of
--             "Meta" -> do
--                 v <- o .: "value"
--                 return $ MetaV v
--             "Lift" -> do
--                 v  <- o .: "value"
--                 return $ Lift v
--             "HomC" -> do
--                 v  <- o .: "value"
--                 s <- do
--                     c <- o .: "con"
--                     c .: "parserSymbol"
--                 case mkHomCon s v of
--                     (Just r) -> return r
--                     Nothing -> P.fail "not a valid HomC"
--             _ -> P.fail "not a valid meta term"
--     parseJSON _ = P.fail "was expecting an object"


-- instance (FromJSON a, MkHomCon v 'FormulaL, MkHomCon v 'StructureL) => FromJSON (MetaTerm v 'StructureL a) where
--     parseJSON (Object o) = do
--         (t :: Text) <- o .: "type"
--         case t of
--             "Meta" -> do
--                 v <- o .: "value"
--                 return $ MetaV v
--             "Lift" -> do
--                 v  <- o .: "value"
--                 return $ Lift v
--             "HomC" -> do
--                 v  <- o .: "value"
--                 s <- do
--                     c <- o .: "con"
--                     c .: "parserSymbol"
--                 case mkHomCon s v of
--                     (Just r) -> return r
--                     Nothing -> P.fail "not a valid HomC"
--             _ -> P.fail "not a valid meta term"
--     parseJSON _ = P.fail "was expecting an object"

-- instance FromJSON a => FromJSON (ConcreteTerm v 'AtomL a) where
--     parseJSON (Object o) = do
--         (t :: Text) <- o .: "type"
--         case t of
--             "Base" -> do
--                 v <- o .: "value"
--                 return $ Base v
--             -- "Lift" -> do
--             --     v  <- o .: "value"
--             --     return $ Lift v
--             _ -> P.fail "not a valid meta term"
--     parseJSON _ = P.fail "was expecting an object"

-- instance (FromJSON a, MkHomCon v 'FormulaL) => FromJSON (ConcreteTerm v 'FormulaL a) where
--     parseJSON (Object o) = do
--         (t :: Text) <- o .: "type"
--         case t of
--             "Lift" -> do
--                 v  <- o .: "value"
--                 return $ Lift v
--             "HomC" -> do
--                 v  <- o .: "value"
--                 s <- do
--                     c <- o .: "con"
--                     c .: "parserSymbol"
--                 case mkHomCon s v of
--                     (Just r) -> return r
--                     Nothing -> P.fail "not a valid HomC"
--             _ -> P.fail "not a valid meta term"
--     parseJSON _ = P.fail "was expecting an object"


-- instance (FromJSON a, MkHomCon v 'FormulaL, MkHomCon v 'StructureL) => FromJSON (ConcreteTerm v 'StructureL a) where
--     parseJSON (Object o) = do
--         (t :: Text) <- o .: "type"
--         case t of
--             "Lift" -> do
--                 v  <- o .: "value"
--                 return $ Lift v
--             "HomC" -> do
--                 v  <- o .: "value"
--                 s <- do
--                     c <- o .: "con"
--                     c .: "parserSymbol"
--                 case mkHomCon s v of
--                     (Just r) -> return r
--                     Nothing -> P.fail "not a valid HomC"
--             _ -> P.fail "not a valid meta term"
--     parseJSON _ = P.fail "was expecting an object"
