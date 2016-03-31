--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import          Data.Monoid (mappend)
import          Data.List (isInfixOf, intercalate, findIndex)
import          Data.List.Split (splitWhen)
import          Data.Char (isSpace)
import          Hakyll
import          Hakyll.Web.Sass (sassCompiler)
import          System.FilePath.Posix (
                    takeBaseName,takeDirectory,
                    (</>),(<.>),splitFileName)

import GHC.IO.Encoding

--------------------------------------------------------------------------------

(<>) = mappend

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    hakyll $ do

        match "assets/**" $ do
            route   idRoute
            compile copyFileCompiler

        match "img/**" $ do
            route   $ idRoute
            compile $ copyFileCompiler

        match "css/*.scss" $ do
            route   $ setExtension "css"
            compile $ sassCompiler

        match "css/*.svg" $ do
            route   $ idRoute
            compile $ copyFileCompiler

        match "projects/*.md" $ do
            compile $ pandocCompiler

        --------------------
        -- POSTS + OEMBED --
        ------------------

        create ["index.html"] $ do 
            route $ idRoute
            compile $ do 
                -- load all posts
                projects <- recentFirst =<< loadAll "projects/*.md"

                let mainCtx =
                        listField "projects" defaultContext (return projects) <>
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/index.html" mainCtx
                    >>= relativizeUrls  

        ---------------
        -- TEMPLATES --
        ---------------

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute 
    where createIndexRoute ident = 
            takeDirectory p </> takeBaseName p </> "index.html"
            where p=toFilePath ident

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url
        where isLocal uri = not (isInfixOf "://" uri)
