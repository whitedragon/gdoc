module GDoc where

import Text.Parsec
import Control.Applicative (pure)
import Data.Maybe
import Data.List
import Control.Monad.Identity

type DocParsec a = ParsecT String Doc Identity a

data GCoreType = GString | GInt | GFloat | GObject (Maybe String) | GArray GType | GNull deriving Show
data GType = Core [GCoreType] | Mixed | Unknown
instance Show GType where
  show (Core t) = intercalate "|" (map show t)
  show Mixed = "Mixed"
  show Unknown = "_"

data Doc = Doc { dFunctionName :: String
               , dPublic :: Bool
               , dTrigger :: Bool
               , dParams :: [DocParam]
               , dReturn :: DocReturn
               , dDescription :: [String]
               , dAuthor :: Maybe String
               , dDeprecated :: Bool
               , dLineNumber :: Int
               }
instance Show Doc where
  show d = "\n" ++ dFunctionName d ++ (if dDeprecated d then " (Deprecated)" else "") ++ ": \n"  
           ++ (if not . null $ (dParams d) then "  Params:\n" ++  (unlines . map (("    " ++) . show) $ dParams d) else "")
           ++ (\x -> "  Return:\n    " ++ (show x) ++ "\n") (dReturn d)
           ++ maybe "" (\x -> "  Author: " ++ x ++ "\n") (dAuthor d)
           ++ "  Description:\n" ++ (unlines . map ("    " ++) $ dDescription d)


data DocParam = DocParam { pName :: String
                         , pType :: GType
                         , pDescription :: String
                         }
instance Show DocParam where
  show p = pName p ++ " - " ++ show (pType p) ++ " - " ++ pDescription p

data DocReturn = DocReturn { rType :: GType
                           , rDescription :: String
                           }
instance Show DocReturn where
  show p = show (rType p) ++ " - " ++ rDescription p

emptyParam :: DocParam
emptyParam = DocParam "" Unknown ""

emptyReturn :: DocReturn
emptyReturn = DocReturn Unknown ""

emptyDoc :: Doc
emptyDoc = Doc "" False False [] emptyReturn [] Nothing False undefined

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

docParam :: DocParsec ()
docParam = do
  string "@param "
  paramType <- gType
  string " "
  desc <- manyTill anyChar (string "\n")
  modifyState (\r -> r { dParams = dParams r ++ [(DocParam "" paramType desc)] })
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
  string "@deprecated"
  skipMany (char ' ')
  char '\n'
  modifyState (\r -> r { dDeprecated = True })

docLine :: DocParsec ()
docLine = do
  skipMany (char ' ')
  string "*"
  skipMany (char ' ')
  
  (try docParam
   <|> try docReturn
   <|> try docAuthor
   <|> try docDeprecated
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
  paramNames <- between (string "(") (string ")") ((many (alphaNum <|> char '.')) `sepBy` (do string ","; optional spaces))
  let fParamNames' = filter (/= "") paramNames
  modifyState (\r -> let fParamNames = filter (/= "") paramNames;
                         docParams = dParams r;
                         extParams = docParams ++ replicate (length fParamNames - length docParams) emptyParam 
                     in r { dParams = zipWith (\p n -> p { pName = n }) extParams fParamNames }
              )

comment :: DocParsec Doc
comment = do
  try $ string "/**"
  skipMany (char ' ')
  char '\n'
  putState emptyDoc
             
  manyTill docLine (try $ do skipMany space; string "*/")
  
  (let trim = reverse . dropWhile (==[]) . reverse . dropWhile (==[]) 
   in modifyState (\r -> r { dDescription = trim (dDescription r) }))
  spaces
  
  gFunction

  getState

expr :: DocParsec [Doc]
expr = fmap (fmap fromJust . filter isJust) $
       manyTill (fmap Just (comment)
                 <|> try (fmap Just (do putState emptyDoc; gFunction; getState))
                 <|> (anyChar >> pure Nothing)
                ) eof
       
parseDoc :: String -> Either ParseError [Doc]
parseDoc = runParser expr emptyDoc ""