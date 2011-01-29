{-# LANGUAGE OverloadedStrings #-}
module GDoc.Renderer.Html where

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title, span)
import Text.Blaze.Renderer.String (renderHtml)

import GDoc
import Control.Monad
import Data.Monoid
import Data.List hiding (head, span)

infixr 6 <>
(<>) = mappend

prettyGCoreType :: GCoreType -> Html
prettyGCoreType = f
  where
    f GString = "string"
    f GInt = "int"
    f GFloat = "float"
    f (GObject Nothing) = "object"
    f (GObject (Just o)) = do "object "; span ! class_ "objectType" $ string o
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
    a ! href ("#" <> stringValue (dFunctionName d)) $ string (dFunctionName d)
    " : " 
    sequence_ $ intersperse " → " (map (prettyGType . pType) (dParams d) ++ [(prettyGType . rType) (dReturn d)])



docHtml :: Doc -> Html
docHtml d =
  li ! class_ (docClass d) ! id (stringValue (dFunctionName d)) $ do
    div ! class_ "functionTitle" $ do
      h3 $ string (dFunctionName d)
      a ! class_ "functionSource" ! href ("source.html#" `mappend` (stringValue . show $ dLineNumber d)) $ "Source"
    
    table . sequence_ $ map docParamRow (dParams d) ++ [docReturnRow (dReturn d)]

    p . sequence_ . intersperse br . map string $ dDescription d


docParamRow :: DocParam -> Html
docParamRow param =
  tr $ do
    (td ! class_ "paramType") . prettyGType . pType $ param
    (td ! class_ "paramName") . string . pName $ param
    td . string . pDescription $ param

docReturnRow :: DocReturn -> Html
docReturnRow ret =
  tr $ do
    (td ! class_ "paramType") . prettyGType . rType $ ret
    (td ! class_ "paramName returnName") . string $ "(return)"
    td . string . rDescription $ ret

docsHtml :: [Doc] -> Html
docsHtml ds =
  docTypeHtml $ do
    head $ do
      meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"      
      title "Documentation"
      link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
      h1 $ "Documentation"
      div ! id "synopsisWrapper" $ do
        h2 "Synopsis"
        ul ! class_ "synopsisFunctions" $ mapM_ docSynopsisHtml ds
      div ! id "functionsWrapper" $ do
        h2 "Functions"
        ul ! class_ "functions" $ mapM_ docHtml ds

renderDocs :: [Doc] -> String
renderDocs = renderHtml . docsHtml