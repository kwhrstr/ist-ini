{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Data.Ini.Config where

import RIO
import Data.Ini.Config.Raw
import qualified RIO.Seq as Seq 
import qualified RIO.Text as T 
import GHC.Exts (IsList(..))
import Text.Read (readMaybe)
import qualified Data.Foldable as F
import Control.Monad.Except
import RIO.State
import RIO.Partial (fromJust)
import Data.Typeable (typeRep, Proxy(..))
import qualified RIO.Map as Map


lkp :: NormalizedText -> Seq (NormalizedText, a) -> Maybe a
lkp name = fmap snd . F.find ((== name) . fst)

lkpI :: NormalizedText -> Seq (NormalizedText, a) -> Maybe Int
lkpI name = Seq.findIndexL ((== name) . fst)

addLineInformation :: Int -> Text -> (Text -> String)
addLineInformation lineNo sec  = T.unpack . err
  where
    err e = "Line " <> T.pack (show lineNo)
         <> ", in section " <> sec <> ": " <> e

type StParserR s a = ExceptT String (Reader s) a

type StParserS s a = ExceptT String (State s) a

newtype IniParser a = IniParser (StParserR RawIni a)
  deriving (Functor, Applicative, Alternative, Monad)
  
newtype SectionParser a = SectionParser (StParserR IniSection a)
  deriving (Functor, Applicative, Monad)

newtype SectionParserS a = SectionParserS (StParserS IniSection a)
  deriving (Functor, Applicative, Monad)
  
parseIniFile :: Text -> IniParser a -> Either String a
parseIniFile text (IniParser mote) = do
  ini <- parseRawIni text
  runReader (runExceptT mote) ini
  

sectionWithConsume :: Text -> SectionParserS a -> IniParser a
sectionWithConsume secName (SectionParserS thunk) = IniParser $ do
  RawIni ini <- ask
  let noDefinedSecErr = "No top-level section named " <> show secName
  flip (maybe $ throwError noDefinedSecErr) (lkp (normalize secName) ini) $ \sec -> do
    (a, iniSec) <- pure $ runState (runExceptT thunk) sec
    case Seq.viewl $ isVals iniSec of
      ((key, val) Seq.:< _) ->
          let addMsg = addLineInformation (ivLineNo val) secName
          in throwError $ addMsg $ "unused parameter \"" <> actualText key <> "\""
      _ ->  ExceptT $ pure a

section :: Text -> SectionParser a -> IniParser a
section name (SectionParser thunk) = IniParser $  do
  RawIni ini <- ask
  case lkp (normalize name) ini of
    Nothing -> throwError $ "No top-level section named " <> show name
    Just sec -> ExceptT $ pure $ runReader (runExceptT thunk) sec

sectionOf :: (Text -> Maybe b) -> (b -> SectionParser a) -> IniParser (Seq a)
sectionOf fn sectionParser = IniParser $ do
  RawIni ini <- ask 
  let go Seq.EmptyL = pure Seq.empty
      go ((t, sec) Seq.:< rs) = flip (maybe (go $ Seq.viewl rs)) (fn $ actualText t) $ \v -> do
        let SectionParser thunk = sectionParser v
        x <- runReader (runExceptT thunk) sec
        xs <- go $ Seq.viewl rs
        pure $ x Seq.<| xs
  ExceptT $ pure $ go (Seq.viewl ini)

sectionMb :: Text -> SectionParser a -> IniParser (Maybe a)
sectionMb name (SectionParser thunk) = IniParser $  do
  RawIni ini <- ask
  case lkp (normalize name) ini of
    Nothing -> pure Nothing
    Just sec -> ExceptT $ pure $ Just <$> runReader (runExceptT thunk) sec

sectionDef :: Text -> a -> SectionParser a -> IniParser a
sectionDef name def (SectionParser thunk) = IniParser $  do
  RawIni ini <- ask
  case lkp (normalize name) ini of
    Nothing  -> pure def
    Just sec -> ExceptT $ pure $ runReader (runExceptT thunk) sec
    
getSectionName :: StParserR IniSection Text
getSectionName = ExceptT $ pure . isName <$> ask

getSectionName' :: StParserS IniSection Text
getSectionName' = ExceptT $ pure . isName <$> get

rawFiledMb :: Text -> StParserR IniSection (Maybe IniValue)
rawFiledMb name = ExceptT $ pure . lkp (normalize name) . isVals <$> ask

rawFiledIndexMb' :: Text -> StParserS IniSection (Maybe Int)
rawFiledIndexMb' name = ExceptT $ pure . lkpI (normalize name) . isVals <$> get

rawFiled :: Text -> StParserR IniSection IniValue
rawFiled name = do
  sec <- getSectionName
  valMb <- rawFiledMb name
  case valMb of
    Nothing -> throwError . T.unpack $  "Missing field " <> name
                    <> " in section " <> sec
    Just x -> pure x

rawFiled' :: Text -> StParserS IniSection IniValue
rawFiled' key = do
  sec <- get
  secName <- getSectionName'
  indexMb <- rawFiledIndexMb' key
  case indexMb of
    Nothing -> throwError . T.unpack $  "Missing field " <> key
                    <> " in section " <> secName
    Just x -> do
      let !val =  Seq.index (isVals sec)  x
      modify (\s -> s {isVals = Seq.deleteAt x $ isVals s})
      pure $ snd val


getVal :: IniValue -> Text
getVal  = T.strip . ivValue

field :: Text -> SectionParser Text
field key = SectionParser $ getVal <$> rawFiled key

fieldOf :: Text -> (Text -> Either Text a) -> SectionParser a
fieldOf key parse = SectionParser $ do
  sec <- getSectionName
  val <- rawFiled key
  let withErr = addLineInformation (ivLineNo val) sec
  either (throwError . withErr) pure $ parse $ getVal val

fieldMb :: Text -> SectionParser (Maybe Text)
fieldMb key = SectionParser $ fmap getVal <$> rawFiledMb key

fieldDef :: Text -> Text -> SectionParser Text
fieldDef key def = SectionParser $ do
  m <- ask
  case lkp (normalize key) (isVals m) of
    Nothing -> pure def
    Just x -> pure $ getVal x

fieldDefOf :: Text -> (Text -> Either Text a) -> a -> SectionParser a
fieldDefOf key parse def = SectionParser $ do
  sec <- getSectionName
  mb <- rawFiledMb key
  flip (maybe $ pure def) mb $ \v ->
    let withErr = addLineInformation (ivLineNo v) sec
    in either (throwError . withErr) pure $ parse $ getVal v


fieldFlag :: Text -> SectionParser Bool
fieldFlag name = fieldOf name flag

flag :: Text -> Either Text Bool
flag s = case T.toLower s of
  "true"  -> Right True
  "yes"   -> Right True
  "t"     -> Right True
  "y"     -> Right True
  "+"     -> Right True
  "1"     -> Right True
  "false" -> Right False
  "no"    -> Right False
  "f"     -> Right False
  "n"     -> Right False
  "-"     -> Right False
  "0"     -> Right False
  _       -> Left $ "Unable to parse " <> s <> " as a boolean"

readable :: forall a. (Read a, Typeable a) => Text -> Either Text a 
readable t = case readMaybe $ T.unpack t of
  Just v -> Right v
  Nothing -> Left $ "Unable to parse" <> t
                 <> " as a value of type " 
                 <> T.pack (show typ)
  where
    typ = typeRep prx
    prx = Proxy @a 

number :: (Num a, Read a, Typeable a) => Text -> Either Text a
number = readable

string :: (IsString a) => Text -> Either Text a
string = pure . fromString . T.unpack

