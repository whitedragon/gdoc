{-# LANGUAGE OverloadedStrings #-}
module GDoc.Renderer.Html (renderScript) where

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title, span)
import Text.Blaze.Renderer.Utf8 (renderHtml)

import GDoc
import Control.Monad
import Data.Monoid
import Data.List hiding (head, span)
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

infixr 6 <>
(<>) = mappend

asciiText :: B.ByteString -> Html
asciiText = text . E.decodeASCII

asciiTextValue :: B.ByteString -> AttributeValue
asciiTextValue = textValue . E.decodeASCII

prettyGCoreType :: GCoreType -> Html
prettyGCoreType = f
  where
    f GString = "string"
    f GInt = "int"
    f GFloat = "float"
    f (GObject Nothing) = "object"
    f (GObject (Just o)) = do "object "; span ! class_ "objectType" $ asciiText o
    f (GArray t) = do "array("; prettyGType t; ")"
    f GNull = "null"

prettyGType :: GType -> Html
prettyGType (Core ts) = (span ! class_ "type") . sequence_ $ intersperse "|" (map prettyGCoreType ts)
prettyGType Mixed = span ! class_ "type" $ "mixed"
prettyGType Unknown = span ! class_ "unknownType" $ "_"

docClass :: Doc -> AttributeValue
docClass d = stringValue . unwords $ (if dPublic d then [] else ["private"]) ++ (if dTrigger d then ["trigger"] else [])

docSynopsisHtml :: Doc -> Html
docSynopsisHtml d =
  li ! class_ (docClass d) $ do
    a ! href ("#" <> asciiTextValue (dFunctionName d)) $ asciiText (dFunctionName d)
    " : " 
    sequence_ . intersperse " → " . F.toList $ fmap (prettyGType . pType) (dParams d) |> (prettyGType . rType) (dReturn d)



docHtml :: B.ByteString -> Doc -> Html
docHtml filename d =
  li ! class_ (docClass d) ! id (asciiTextValue (dFunctionName d)) $ do
    div ! class_ "functionTitle" $ do
      h3 $ asciiText (dFunctionName d)
    
    table . sequence_ . F.toList $ fmap docParamRow (dParams d) |> docReturnRow (dReturn d)

    p . unlinesBr $ dDescription d

unlinesBr :: [B.ByteString] -> Html
unlinesBr = sequence_ . intersperse br . map asciiText

docParamRow :: DocParam -> Html
docParamRow param =
  tr $ do
    (td ! class_ "paramType") . prettyGType . pType $ param
    (td ! class_ "paramName") . asciiText . pName $ param
    td . asciiText . pDescription $ param

docReturnRow :: DocReturn -> Html
docReturnRow ret =
  tr $ do
    (td ! class_ "paramType") . prettyGType . rType $ ret
    (td ! class_ "paramName returnName") . asciiText $ "(return)"
    td . asciiText . rDescription $ ret

scriptDocHtml :: Doc -> Html
scriptDocHtml d = do
  div ! id "script" $ do
    maybe (return ()) ((p ! class_ "author") . asciiText . ("Author: " `mappend`)) (dAuthor d)
    p . unlinesBr $ dDescription d

scriptHtml :: Script -> Html
scriptHtml s =
  docTypeHtml $ do
    head $ do
      meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"      
      title "Documentation"
      link ! rel "stylesheet" ! type_ "text/css" ! href "../screen.css"
    body $ do
      h1 $ do
        "Documentation: "
        (a ! href ((asciiTextValue . sFilename $ s) <> "_source.html")) . asciiText $ sFilename s
      maybe (return ()) scriptDocHtml (sDoc s)
      let ds = sFunctions s
      div ! id "synopsisWrapper" $ do
        h2 "Synopsis"
        ul ! class_ "synopsisFunctions" $ mapM_ docSynopsisHtml ds
      div ! id "functionsWrapper" $ do
        h2 "Functions"
        ul ! class_ "functions" $ mapM_ (docHtml (sFilename s)) ds

renderScript :: Script -> LB.ByteString
renderScript = renderHtml . scriptHtml