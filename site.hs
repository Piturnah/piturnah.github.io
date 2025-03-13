{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath
import Text.Pandoc.Options

emptyField :: String -> Context a
emptyField name = boolField name $ const True

mdRoute :: Routes
mdRoute = customRoute $ (</> "index.html") . dropExtension . toFilePath

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
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate
                    "_templates/default.html"
                    (defaultContext <> emptyField "blog")

    create ["atom.xml"] $ do
        route idRoute
        compile $
            loadAll "blog/*"
                >>= recentFirst
                >>= renderAtom
                    FeedConfiguration
                        { feedTitle = "piturnah.xyz"
                        , feedDescription = "Pit's blog"
                        , feedAuthorName = "Peter Hebden"
                        , feedAuthorEmail = ""
                        , feedRoot = "https://piturnah.xyz"
                        }
                    defaultContext

    match "**.md" $ route mdRoute >> compileWith pandocCompiler
    match "**.html" $ route idRoute >> compileWith getResourceBody
    match "**" $ route idRoute >> compile copyFileCompiler
