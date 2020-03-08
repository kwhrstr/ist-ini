{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ini.Config.Raw where

import Text.Megaparsec
import Text.Megaparsec.Char
import RIO hiding (some, many)
import qualified RIO.Text as T
import RIO.Seq
import qualified RIO.Seq as Seq

type Parser  = Parsec Void Text

data NormalizedText = NormalizedText
  { actualText :: Text
  , normalizedText :: Text
  } deriving Show

normalize :: Text -> NormalizedText
normalize t = NormalizedText t . T.toLower . T.strip $ t

instance Eq NormalizedText where
  NormalizedText _ x == NormalizedText _ y =
    x == y

instance Ord NormalizedText where
  NormalizedText _ x `compare` NormalizedText _ y =
    x `compare` y
    

newtype RawIni = RawIni { fromRawIni :: Seq (NormalizedText, IniSection) }
  deriving (Eq, Show)
  
data IniSection = IniSection
  { isName :: Text
  , isVals :: Seq (NormalizedText, IniValue)
  , isStartLine :: Int
  , isEndLine :: Int
  } deriving (Show, Eq)
  
data IniValue = IniValue
  { ivLineNo :: Int
  , ivName :: Text
  , ivValue :: Text
  }  deriving (Show, Eq)

  
type RawVals = Map Text Text


parseRawIni :: Text -> Either String RawIni
parseRawIni t = case runParser pIni "ini file" t of
  Left err -> Left (errorBundlePretty err)
  Right v -> Right v

pIni :: Parser RawIni
pIni = sBlanks >> pSections Seq.empty

sBlanks :: Parser ()
sBlanks = skipMany $ void eol <|> sComment

sComment :: Parser ()
sComment = do
  void $ oneOf ";#"
  void $ skipManyTill anySingle eolOrf
  
pSections :: Seq (NormalizedText, IniSection) -> Parser RawIni
pSections prevs = pSection prevs <|> RawIni prevs <$ eof

pSection :: Seq (NormalizedText, IniSection) -> Parser RawIni
pSection prevs = do
  start <- getCurrentLine
  name <- fmap T.pack $ between (char '[') (char ']') $ some $ notFollowedBy eol *> noneOf "[]"
  eolOrf
  pPairs name start prevs Seq.empty
  
pPairs :: Text
       -> Int
       -> Seq (NormalizedText, IniSection)
       -> Seq (NormalizedText, IniValue)
       -> Parser RawIni
pPairs sectionName start prevs iniVals = newPairs <|> finishedSections
  where
    newPairs = do
      sBlanks
      p <- pPair
      pPairs sectionName start prevs $ iniVals Seq.|> p
    finishedSections = do
      end <- getCurrentLine
      let newSection = IniSection
            { isName = sectionName
            , isVals = iniVals
            , isStartLine = start
            , isEndLine = end
            }
      pSections $ prevs Seq.|> (normalize sectionName,  newSection)

p = parseTest (pSections Seq.empty) t
t = T.pack "[sec]\nt=d\n[sec2]"

pPair :: Parser (NormalizedText, IniValue)
pPair = do
  pos <- getCurrentLine
  notFollowedBy $ oneOf "[]"
  key <- fmap T.pack $ some $ notFollowedBy eol *> noneOf "=:"
  void $ oneOf "=:"
  val <- T.pack <$> manyTill anySingle eolOrf
  pure ( normalize key
       , IniValue
          { ivLineNo = pos
          , ivName = key
          , ivValue = val
          }
       )

getCurrentLine :: Parser Int
getCurrentLine = fromIntegral . unPos . sourceLine <$> getSourcePos

eolOrf :: Parser ()
eolOrf = void eol <|> eof

lookupInSection :: Text
                -- ^ The section name.
                -> Text
                -- ^ The key
                -> RawIni
                -- ^ The Ini
                -> Seq Text
lookupInSection sec opt ini = 
  ivValue <$> asum (lookupValue opt <$> lookupSection sec ini)

lookupSection :: Text
              -> RawIni
              -> Seq IniSection
lookupSection name ini =
  snd <$> Seq.filter ((== normalize name). fst) (fromRawIni ini)

lookupValue :: Text
            -> IniSection
            -> Seq IniValue
lookupValue name section =
  snd <$> Seq.filter ((== normalize name) . fst) (isVals section)