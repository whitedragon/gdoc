{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module GDoc (GCoreType(..), GType(..), Script(..), Doc(..), DocParam(..), DocReturn(..), parseDoc) where

import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator

import Control.Applicative
import Data.Maybe
import Prelude hiding (takeWhile)
import Data.List hiding (takeWhile)
import Control.Monad.Identity
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as S
import qualified Data.ByteString.Char8 as B
import Debug.Trace

type DocParsec a = Parser a

data GCoreType = GString | GInt | GFloat | GObject !(Maybe B.ByteString) | GArray !GType | GNull deriving Show
data GType = Core ![GCoreType] | Mixed | Unknown deriving Show

data Script = Script { sFilename :: !B.ByteString
                     , sDoc :: !(Maybe Doc)
                     , sFunctions :: ![Doc]
                     }
data Doc = Doc { dFunctionName :: !B.ByteString
               , dPublic :: !Bool
               , dTrigger :: !Bool
               , dParams :: !(Seq DocParam)
               , dReturn :: !DocReturn
               , dDescription :: ![B.ByteString]
               , dAuthor :: !(Maybe B.ByteString)
               , dDeprecated :: !Bool
               , dLineNumber :: !Int
               , dScript :: !Bool
               } deriving Show
data DocParam = DocParam { pName :: !B.ByteString
                         , pType :: !GType
                         , pDescription :: !B.ByteString
                         } deriving Show
data DocReturn = DocReturn { rType :: !GType
                           , rDescription :: !B.ByteString
                           } deriving Show

emptyParam :: DocParam
emptyParam = DocParam "" Unknown ""

emptyReturn :: DocReturn
emptyReturn = DocReturn Unknown ""

emptyDoc :: Doc
emptyDoc = Doc "" False False S.empty emptyReturn [] Nothing False 0 False

emptyScript :: Script
emptyScript = Script "" Nothing []

gCoreType :: DocParsec [GCoreType]
gCoreType = do
  d <-     do string "string"; return GString
       <|> do string "int"; return GInt
       <|> do string "float"; return GFloat
       <|> do string "null"; return GNull
       <|> do string "object"
              objectType <- optionMaybe (between (char '(') (char ')') (takeWhile $ \a -> isAlpha_ascii a || isDigit a))
              return (GObject $ objectType)
       <|> do string "array"
              objectType <- optionMaybe (between (char '(') (char ')') gType)
              return (GArray $ fromMaybe Mixed objectType)
  n <- optionMaybe (char '|' >> gCoreType)
  case n of
    Nothing -> return ([d])
    Just t -> return (d : t)

gType :: DocParsec GType
gType = fmap Core gCoreType
        <|> return Mixed

docScript :: Doc -> DocParsec Doc
docScript r = do
  docLineFlag "@script"
  return $ r { dScript = True }

docParam :: Doc -> DocParsec Doc
docParam r = do
  string "@param "
  paramType <- gType
  char ' '
  desc <- takeWhile (/= '\n')
  char '\n'
  return $ r { dParams = dParams r |> (DocParam "" paramType desc) }

docReturn :: Doc -> DocParsec Doc
docReturn r = do 
  string "@return "
  returnType <- gType
  optional $ char ' '
  desc <- takeWhile (/= '\n')
  char '\n'
  return $ r { dReturn = DocReturn returnType desc }

docAuthor :: Doc -> DocParsec Doc
docAuthor r = do
  string "@author "
  author <- takeWhile (/= '\n')
  char '\n'
  return $ r { dAuthor = Just author }
  
docDeprecated :: Doc -> DocParsec Doc
docDeprecated r = do
  docLineFlag "@deprecated"
  return $ r { dDeprecated = True }

docLineFlag :: B.ByteString -> DocParsec ()
docLineFlag f = do
  string f
  skipMany (char ' ')
  char '\n'
  return ()

docLine :: Doc -> DocParsec Doc
docLine r = do
  skipMany (char ' ')
  string "*"
  skipMany (char ' ')
  
  (try (docParam r)
   <|> try (docReturn r)
   <|> try (docAuthor r)
   <|> try (docDeprecated r)
   <|> try (docScript r)
   <|> do d <- manyTill anyChar (try (string "\n") <|> try (string "*/"))
          return $ r { dDescription = dDescription r ++ [B.pack d] }
   <|> do char '\n'; return $ r)

gFunction :: Doc -> DocParsec Doc
gFunction r = do
  public <- optionMaybe (string "public ")
  let a = r { dPublic = isJust public }
  
  string "function "

  -- !pos <- getPosition
  -- let b = a { dLineNumber = sourceLine pos }
  
  !fName <- takeWhile (\a -> isAlpha_ascii a || a == '_' || a == '.')
  let c = a { dFunctionName = fName
            , dTrigger = "on" `B.isPrefixOf` (fName)
            }
             
  skipWhile (==' ')

  !paramNames <- between (char '(') (char ')') ((between (skipWhile (==' ')) (skipWhile (==' ')) (takeWhile $ \a -> isAlpha_ascii a || a == '_' || a == '.'))
                                                `sepBy`
                                                char ','
                                               )
  -- paramNames <- takeWhile (\a -> isAlpha_ascii a || a == '_' || a == '.' || a == ',')
  -- let paramNames = "lol"

  let d = let fParamNames = S.filter (/= "") $ S.fromList $ paramNames;
              docParams = dParams r;
              extParams = docParams >< S.replicate (S.length fParamNames - S.length docParams) emptyParam 
           in c { dParams = S.zipWith (\p n -> p { pName = n }) extParams fParamNames }
  
  return $ d

comment :: DocParsec Doc
comment = do
  string "/**\n"
  
  r <- foreverUntil docLine (try $ do skipMany space; string "*/") emptyDoc
  
  let trim = reverse . dropWhile (==B.empty) . reverse . dropWhile (==B.empty) 
  let a = r { dDescription = trim (dDescription r) }
  
  skipWhile isSpace
  
  if (not . dScript $ r) then gFunction r else return r

foreverUntil :: (a -> Parser a) -> Parser c -> a -> Parser a
foreverUntil m sep a = do x <- m a; o <- optionMaybe sep; case o of { Just _ -> return $! x; Nothing -> foreverUntil m sep x }

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe a = option Nothing (fmap Just a)

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = do open; x <- p; close; return x

functions :: DocParsec [Doc]
functions = catMaybes <$> manyTill l endOfInput
  where l =     fmap Just comment
            <|> (try $ fmap Just (gFunction emptyDoc))
            <|> (do anyChar; return Nothing)

scriptDoc :: DocParsec (Maybe Doc)
scriptDoc = do
  mc <- optionMaybe comment
  return $ mc >>= (\c -> if (dScript $ c) then Just c else Nothing)

script :: DocParsec Script
script = do
  !script <- scriptDoc >>= \c -> return $! Script "" c []
             -- (return $! emptyScript)
  !fs <- functions
  return $! script {sFunctions = fs}

parseDoc :: B.ByteString -> Result Script
parseDoc = parse script 