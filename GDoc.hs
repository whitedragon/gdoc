module GDoc (GCoreType(..), GType(..), Script(..), Doc(..), DocParam(..), DocReturn(..), parseDoc) where

import Text.Parsec
import Control.Applicative (pure, (<$>))
import Data.Maybe
import Data.List
import Control.Monad.Identity
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as S

type DocParsec a = ParsecT String Doc Identity a

data GCoreType = GString | GInt | GFloat | GObject (Maybe String) | GArray GType | GNull
data GType = Core [GCoreType] | Mixed | Unknown

data Script = Script { sDoc :: Maybe Doc,
                       sFunctions :: [Doc]
                     }
data Doc = Doc { dFunctionName :: String
               , dPublic :: Bool
               , dTrigger :: Bool
               , dParams :: Seq DocParam
               , dReturn :: DocReturn
               , dDescription :: [String]
               , dAuthor :: Maybe String
               , dDeprecated :: Bool
               , dLineNumber :: Int
               , dScript :: Bool
               }
data DocParam = DocParam { pName :: String
                         , pType :: GType
                         , pDescription :: String
                         }
data DocReturn = DocReturn { rType :: GType
                           , rDescription :: String
                           }

emptyParam :: DocParam
emptyParam = DocParam "" Unknown ""

emptyReturn :: DocReturn
emptyReturn = DocReturn Unknown ""

emptyDoc :: Doc
emptyDoc = Doc "" False False S.empty emptyReturn [] Nothing False 0 False

emptyScript :: Script
emptyScript = Script Nothing []

gCoreType :: DocParsec [GCoreType]
gCoreType = do
  d <-     do string "string"; return GString
       <|> do string "int"; return GInt
       <|> do string "float"; return GFloat
       <|> do string "null"; return GNull
       <|> do string "object"
              objectType <- optionMaybe (between (string "(") (string ")") (many alphaNum))
              return (GObject objectType)
       <|> do string "array"
              objectType <- optionMaybe (between (string "(") (string ")") gType)
              return (GArray $ fromMaybe Mixed objectType)
  n <- optionMaybe (string "|" >> gCoreType)
  case n of
    Nothing -> return ([d])
    Just t -> return (d : t)

gType :: DocParsec GType
gType = fmap Core gCoreType
        <|> return Mixed

docScript :: DocParsec ()
docScript = do
  docLineFlag "@script"
  modifyState (\r -> r { dScript = True })

docParam :: DocParsec ()
docParam = do
  string "@param "
  paramType <- gType
  string " "
  desc <- manyTill anyChar (string "\n")
  modifyState (\r -> r { dParams = dParams r |> (DocParam "" paramType desc) })
  return ()

docReturn :: DocParsec ()
docReturn = do 
  string "@return "
  returnType <- gType
  optional $ string " "
  desc <- manyTill anyChar (string "\n")
  modifyState (\r -> r { dReturn = DocReturn returnType desc } )
  return ()

docAuthor :: DocParsec ()
docAuthor = do
  string "@author "
  author <- manyTill anyChar (string "\n")
  modifyState (\r -> r { dAuthor = Just author })
  
docDeprecated :: DocParsec ()
docDeprecated = do
  docLineFlag "@deprecated"
  modifyState (\r -> r { dDeprecated = True })

docLineFlag :: String -> DocParsec ()
docLineFlag f = do
  string f
  skipMany (char ' ')
  char '\n'
  return ()

docLine :: DocParsec ()
docLine = do
  skipMany (char ' ')
  string "*"
  skipMany (char ' ')
  
  (    try docParam
   <|> try docReturn
   <|> try docAuthor
   <|> try docDeprecated
   <|> try docScript
   <|> do d <- manyTill anyChar (try (string "\n") <|> try (string "*/"))
          modifyState (\r -> r { dDescription = dDescription r ++ [d] })
          return ()
   <|> do char '\n'; return ())
  
  return ()

gFunction :: DocParsec ()
gFunction = do
  public <- optionMaybe (string "public ")
  modifyState (\r -> r { dPublic = isJust public })
  
  string "function "
  
  pos <- getPosition
  modifyState (\r -> r { dLineNumber = sourceLine pos })
  
  fName <- manyTill anyChar (string " " <|> lookAhead (string "("))
  modifyState (\r -> r { dFunctionName = fName
                       , dTrigger = "on" `isPrefixOf` fName
                       }
              )
             
  skipMany (char ' ')
  paramNames <- between (string "(") (string ")") ((do skipMany (char ' '); s <- many (alphaNum <|> char '.' <|> char '_'); skipMany (char ' '); return s) `sepBy` (do string ","; optional spaces))
  let fParamNames' = filter (/= "") paramNames
  modifyState (\r -> let fParamNames = S.fromList $ filter (/= "") paramNames;
                         docParams = dParams r;
                         extParams = docParams >< S.replicate (S.length fParamNames - S.length docParams) emptyParam 
                     in r { dParams = S.zipWith (\p n -> p { pName = n }) extParams fParamNames }
              )

comment :: DocParsec Doc
comment = do
  try $ do
    string "/**"
    skipMany (char ' ')
    char '\n'
  putState emptyDoc
             
  manyTill docLine (try $ do skipMany space; string "*/")
  
  (let trim = reverse . dropWhile (==[]) . reverse . dropWhile (==[]) 
   in modifyState (\r -> r { dDescription = trim (dDescription r) }))
  spaces
  
  s <- getState
  when (not . dScript $ s) gFunction

  getState

functions :: DocParsec [Doc]
functions = catMaybes <$> manyTill l eof
  where l =     fmap Just comment
            <|> try (fmap Just (do putState emptyDoc; gFunction; getState))
            <|> (do anyChar; return Nothing)

scriptDoc :: DocParsec Doc
scriptDoc = do
  c <- comment
  when (not $ dScript c) (fail "Script-level docblock.")
  return c

script :: DocParsec Script
script = do
  script <- try (do c <- scriptDoc; return $ Script (Just c) [])
            <|> (return $ Script Nothing [])
  fs <- functions
  return $ script {sFunctions = fs}

parseDoc :: String -> Either ParseError Script
parseDoc = runParser script emptyDoc ""