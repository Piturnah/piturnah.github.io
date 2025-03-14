{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath
import Text.Pandoc.Options

emptyField :: String -> Context a
emptyField name = boolField name $ const True

emptyItem :: Identifier -> Item String
emptyItem = flip Item ""

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
            emptyItem "index.html"

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
            loadAll ("blog/*" .&&. complement "blog/index.html")
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

    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            posts <-
                recentFirst
                    =<< loadAll ("blog/*" .&&. complement "blog/index.html")
            let lastPost = last posts
            let postCtx =
                    defaultContext
                        <> field
                            "bullet"
                            ( \item ->
                                pure $
                                    if itemBody item == itemBody lastPost
                                        then "&#x2514;&#x2500;"
                                        else "&#x251c;&#x2500;"
                            )
            loadAndApplyTemplate
                "_templates/blog.html"
                (listField "posts" postCtx $ pure posts)
                (emptyItem "blog/index.html")
                >>= loadAndApplyTemplate
                    "_templates/default.html"
                    defaultContext

    match "**.md" $ route mdRoute >> compileWith pandocCompiler
    match "**.html" $ route idRoute >> compileWith getResourceBody
    match "**" $ route idRoute >> compile copyFileCompiler
