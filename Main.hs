{-# LANGUAGE OverloadedStrings #-}
module Main (main, {-out-}) where

import GDoc
import GDoc.Renderer.Html
import Text.Highlighting.Kate
import Text.XHtml.Strict (showHtml, (+++), Html, body, header, thelink, thetype, src, (!), noHtml, rel, href)
import System.Directory
import System.Environment
import Data.Functor
import Data.List
import Data.Monoid
import Control.Monad
import Data.Attoparsec (Result(..), eitherResult, feed)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

infixr 6 <>
(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

main :: IO ()
main = do
  args <- getArgs
  when (length args <= 0) (fail "Please provide an input directory as the first argument.") -- containing weapons/, npcs/, and scripts/ in it.")
  when (length args <= 1) (fail "Please provide an output directory as the second argument.")
  let inDir = args !! 0
  let outDir = args !! 1
  
  exists <- mapM (doesDirectoryExist . ((inDir++"/")++)) ["npcs", "scripts", "weapons"]
  when (not . and $ exists) (fail $ "Please ensure the input directory " ++ inDir ++ "/ has sub-directories: weapons/, npcs/, and scripts/") 
  
  [npcs, scripts, weapons] <- mapM (cleanDirectoryContents . ((inDir++"/")++)) ["npcs", "scripts", "weapons"]
  
  createDirectory outDir
  
  mapM_ (\p -> B.readFile (filePath [inDir, "npcs", p]) >>= f inDir outDir "npcs" p . cleanNPC) npcs
  mapM_ (\p -> B.readFile (filePath [inDir, "scripts", p]) >>= f inDir outDir "scripts" p) scripts
  mapM_ (\p -> B.readFile (filePath [inDir, "weapons", p]) >>= f inDir outDir "weapons" p) weapons
 
  where
    f inDir outDir subDir file code = do
      B.putStrLn $ "Parsing: " <> (B.pack $ filePath [subDir, file])
      LB.writeFile (filePath [outDir,  subDir ++ "_" ++ file ++ ".html"]) $ generate (B.pack $ subDir ++ "_" ++ file) code
      {-LB.writeFile (filePath [outDir,  subDir ++ "_" ++ file ++ "_source.html"]) $ either (error . show) (\f -> renderHtml . docTypeHtml $ do
                                                                                                          Text.Blaze.Html5.head $ link ! rel "stylesheet" ! type_ "text/css" ! href "../source.css"
                                                                                                          Text.Blaze.Html5.body $ format True f
                                                                                                         ) $ runLexer lexer code-}
      writeFile (filePath [outDir, subDir ++ "_" ++ file ++ "_source.html"]) $ either (error . show) (showHtml . formatSource) $ highlightAs "javascript" (B.unpack code)


filePath :: [String] -> FilePath
filePath = intercalate "/"

formatSource :: [SourceLine] -> Html
formatSource d = (header $ thelink ! [rel "stylesheet", thetype "text/css", href "../screen.css"] $ noHtml) +++ (body . formatAsXHtml [OptNumberLines, OptLineAnchors] "javascript" $ d)

cleanNPC :: B.ByteString -> B.ByteString
cleanNPC = fst . B.breakSubstring "NPCSCRIPTEND" . B.drop (B.length "NPCSCRIPT\n") . snd . B.breakSubstring "NPCSCRIPT\n"

cleanPaths :: [FilePath] -> [FilePath]
cleanPaths = filter (\s -> s /= "." && s /= "..")

cleanDirectoryContents :: FilePath -> IO [FilePath]
cleanDirectoryContents = fmap cleanPaths . getDirectoryContents

generate :: B.ByteString -> B.ByteString -> LB.ByteString
generate f c = renderScript . handleResult $ fmap (\x -> x { sFilename = f }) (parseDoc c)

handleResult :: Result a -> a
handleResult (Fail a b c) = error . show $ c
handleResult a@(Partial p) = handleResult $ feed a B.empty
handleResult (Done a r) = r
