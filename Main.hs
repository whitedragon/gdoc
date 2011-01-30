module Main where

import GDoc
import GDoc.Renderer.Html
import Text.Highlighting.Kate
import Text.XHtml.Strict (showHtml, (+++), Html, body, header, thelink, thetype, src, (!), noHtml, rel, href)

out :: IO ()
out = putStrLn . generate =<< readFile "examples/chat_system"

main :: IO ()
main = do
  code <- readFile "examples/chat_system"
  writeFile "gdoc.html" (generate code)
  writeFile "source.html" $ either (error . show) (showHtml . formatSource) $ highlightAs "javascript" code

formatSource :: [SourceLine] -> Html
formatSource d = (header $ thelink ! [rel "stylesheet", thetype "text/css", href "screen.css"] $ noHtml) +++ (body . formatAsXHtml [OptNumberLines, OptLineAnchors] "javascript" $ d)

generate :: String -> String
generate = renderScript . either (error . show) id . parseDoc