--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import           Data.List                      ( isInfixOf
                                                , intercalate
                                                , findIndex
                                                )
import           Data.List.Split                ( splitWhen )
import           Data.Char                      ( isSpace )
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           Hakyll.Web.CompressCss         ( compressCss )
import           System.FilePath.Posix          ( takeBaseName
                                                , takeDirectory
                                                , (</>)
                                                , (<.>)
                                                , splitFileName
                                                )
import           Data.Text                      ( pack
                                                , unpack
                                                , replace
                                                )

import           Text.Regex                     ( subRegex
                                                , mkRegex
                                                )

import           GHC.IO.Encoding
import           GHC.IO.Encoding
import           System.FilePath.Posix          ( takeBaseName )
--------------------------------------------------------------------------------

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    hakyll $ do

        match ("js/**" .||. "img/**" .||. "assets/**" .||. "css/*.svg") $ do
            route idRoute
            compile copyFileCompiler

            match "css/*.scss" $ do
                route $ setExtension "css"
                compile $ (fmap compressCss) <$> sassCompiler

        match "icons/*.svg" $ do
            compile $ getResourceBody

        match "projects/*.md" $ do
            compile $ pandocCompiler

        --------------------
        -- POSTS + OEMBED --
        ------------------

        -- create ["img/icon-library.svg"] $ do
        --     route $ idRoute
        --     compile $ do

        --         makeItem ""
        --             >>= loadAndApplyTemplate "templates/icon-library.svg" svgCtx
        --             >>= relativizeUrls

        create ["index.html"] $ do
            route $ idRoute
            compile $ do
                -- load all icons and compile to field
                icons <- loadAll "icons/*.svg"
                let svgCtx = listField "icons" svgSymbolContext (return icons)

                -- load all posts
                projects <- loadAll "projects/*.md"
                let projectsCtx = listField
                        "projects"
                        (defaultContext <> projectIconContext)
                        (return projects)

                let mainCtx = projectsCtx <> svgCtx <> defaultContext

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
  where
    createIndexRoute ident =
        takeDirectory p </> takeBaseName p </> "index.html"
        where p = toFilePath ident

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _ -> url
        where isLocal uri = not (isInfixOf "://" uri)


svgToSymbol :: String -> String -> String
svgToSymbol svgString symbolId =
    let symbolHeader = pack $ "<symbol id=\"" ++ symbolId ++ "\""
        xmlSubRegex  = mkRegex "<\\?[^\\?]*\\?>"
        xmlIdRegex   = mkRegex " id=\"[^\"]*\""
    in  unpack
            $ replace "<svg"  symbolHeader
            $ replace "xmlns=\"http://www.w3.org/2000/svg\"" ""
            $ replace "</svg" "</symbol"
            $ pack
            $ subRegex xmlIdRegex (subRegex xmlSubRegex svgString "") " "

getSymbolBody item =
    let body    = itemBody item
        svgName = takeBaseName $ toFilePath $ itemIdentifier item
    in  return (svgToSymbol body svgName)

svgSymbolContext :: Context String
svgSymbolContext = field "svgSymbol" $ getSymbolBody

baseNameIfIconExists basename = basename

projectIconContext :: Context String
projectIconContext =
    (mapContext baseNameIfIconExists . titleField) "projectIcon"
