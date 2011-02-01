module Main where

import GDoc
import GDoc.Renderer.Html
import Text.Highlighting.Kate
import Text.XHtml.Strict (showHtml, (+++), Html, body, header, thelink, thetype, src, (!), noHtml, rel, href)
import System.Directory
import System.Environment
import Control.Monad

out :: IO ()
out = putStrLn . generate =<< readFile "examples/chat_system"

main :: IO ()
main = do
  args <- getArgs
  when (length args < 0) (fail "Please provide a directory as the first argument.") -- containing weapons/, npcs/, and scripts/ in it.")
  let dir = head args
  
  npcPaths <- fmap cleanPaths $ getDirectoryContents (dir++"/npcs")
  scriptPaths <- fmap cleanPaths $ getDirectoryContents (dir++"/scripts")
  weaponPaths <- fmap cleanPaths $ getDirectoryContents (dir++"/weapons")
  
  createDirectory "docs"
  forM_ npcPaths (f dir "npcs")
  forM_ scriptPaths (f dir "scripts")
  forM_ weaponPaths (f dir "weapons")
  
  where
    f dir d s = do
      putStrLn ("Parsing: " ++ d ++ "/" ++ s)
      code <- readFile (dir ++ "/" ++ d ++ "/" ++ s)
      writeFile ("docs/" ++ d ++ "_" ++ s ++ ".html") (generate code)
      writeFile ("docs/" ++ d ++ "_" ++ s ++ "_source.html") $ either (error . show) (showHtml . formatSource) $ highlightAs "javascript" code

formatSource :: [SourceLine] -> Html
formatSource d = (header $ thelink ! [rel "stylesheet", thetype "text/css", href "screen.css"] $ noHtml) +++ (body . formatAsXHtml [OptNumberLines, OptLineAnchors] "javascript" $ d)

cleanPaths :: [FilePath] -> [FilePath]
cleanPaths = filter (\s -> s /= "." && s /= "..")
  
generate :: String -> String
generate = renderScript . either (error . show) id . parseDoc