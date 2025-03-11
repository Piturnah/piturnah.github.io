{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath
import Text.Pandoc.Options

emptyField :: String -> Context String
emptyField name = field name (const . pure $ "")

mdRoute :: Routes
mdRoute = customRoute $ mdRoute' . toFilePath
  where
    mdRoute' :: FilePath -> FilePath
    mdRoute' path =
        let (dir, name) = splitFileName . dropExtension $ path
         in dir </> name </> "index.html"

compileWith :: Compiler (Item String) -> Rules ()
compileWith compiler = compile $ do
    identifier <- getUnderlying
    layout <- getMetadataField identifier "layout"
    compiler >>= case layout of
        Just "none" -> pure
        Just path -> loadAndApplyTemplate (fromFilePath path) defaultContext
        Nothing -> loadAndApplyTemplate "_templates/default.html" defaultContext

main :: IO ()
main = hakyll $ do
    match "_templates/*" . compile $ templateCompiler

    create ["index.html"] $ do
        route idRoute
        compile . loadAndApplyTemplate "_templates/index.html" defaultContext $
            Item "index.html" ""

    match "blog/**.md" $ do
        route mdRoute
        compile $
            pandocCompilerWith
                defaultHakyllReaderOptions
                defaultHakyllWriterOptions{writerHTMLMathMethod = MathJax ""}
                >>= loadAndApplyTemplate
                    "_templates/default.html"
                    (defaultContext <> emptyField "blog")

    match "**.md" $ route mdRoute >> compileWith pandocCompiler
    match "**.html" $ route idRoute >> compileWith getResourceBody
    match "**" $ route idRoute >> compile copyFileCompiler
