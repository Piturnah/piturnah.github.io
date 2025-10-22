{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Data.Maybe (isNothing)
import Hakyll
import System.FilePath
import Text.Pandoc.Options

emptyField :: String -> Context a
emptyField name = boolField name $ const True

stringField :: String -> (Item a -> String) -> Context a
stringField name f = field name $ pure . f

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

filterUnlisted :: (MonadMetadata m) => [Item String] -> m [Item String]
filterUnlisted = filterM $ \item ->
    isNothing <$> getMetadataField (itemIdentifier item) "unlisted"

loadPosts :: Compiler ([Item String], Context String)
loadPosts = do
    posts <-
        recentFirst
            =<< filterUnlisted
            =<< loadAll ("blog/*" .&&. complement "blog/index.html")
    let lastPost = last posts
    let postCtx =
            mapContextBy (== "url") dropFileName defaultContext
                <> stringField
                    "bullet"
                    ( \item ->
                        if itemBody item == itemBody lastPost
                            then "&#x2514;&#x2500;"
                            else "&#x251c;&#x2500;"
                    )
    pure (posts, postCtx)

main :: IO ()
main = hakyll $ do
    match "_templates/*" . compile $ templateCompiler

    create ["index.html"] $ do
        route idRoute
        compile $ do
            (posts, postCtx) <- loadPosts
            let ctx = defaultContext <> listField "posts" postCtx (pure posts)
            loadAndApplyTemplate "_templates/index.html" ctx $
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
                >>= filterUnlisted
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
            (posts, postCtx) <- loadPosts
            loadAndApplyTemplate
                "_templates/blog.html"
                (listField "posts" postCtx $ pure posts)
                (emptyItem "blog/index.html")
                >>= loadAndApplyTemplate
                    "_templates/default.html"
                    defaultContext

    match "site.hs" $ do
        route (constRoute "site.hs/index.html")
        compile $
            getResourceBody
                >>= applyTemplate "<pre>$body$</pre>" defaultContext
                    . fmap escapeHtml

    match "**.md" $ route mdRoute >> compileWith pandocCompiler
    match "**.html" $ route idRoute >> compileWith getResourceBody
    match "**" $ route idRoute >> compile copyFileCompiler
