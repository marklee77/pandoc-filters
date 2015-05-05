-- pandoc-plantuml-filter.hs, a pandoc json filter to create inline diagrams 
-- with plantuml
--
-- Copyright (C) 2015 Mark Lee Stillwell
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program. If not, see <http://www.gnu.org/licenses/>.

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.String.Utils (join, split, strip)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getEnv)
import System.FilePath (addExtension, combine)
import System.IO (hClose, hPutStr)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process (system)
import Text.Pandoc.JSON (Block, Block(CodeBlock, Para), Inline(Image, Str), 
                         toJSONFilter)


withPreloadedFile :: String -> (FilePath -> IO a) -> IO a
withPreloadedFile content action =
    withSystemTempFile "plantuml.txt" callback
  where callback path handle = do 
            hPutStr handle content
            hClose handle
            action path


uniqueFilePrefix :: String -> String
uniqueFilePrefix = showDigest . sha1 . fromString


renderFigure :: String -> String-> String -> String -> IO String
renderFigure infile figfmt figdir prefix = do
    createDirectoryIfMissing True figdir
    let outfile = combine figdir $ addExtension prefix figfmt
    if figfmt == "pdf"
        then do
            epsfile <- renderFigure infile "eps" figdir prefix
            system $ join " " ["epspdf", epsfile, outfile]
        else
            system $ join " " ["plantuml", "-pipe", join "" ["-t", figfmt], 
                               "<", infile, ">", outfile] 
    return outfile


processDocument :: String -> String -> Block -> IO Block
processDocument figdir docfmt (CodeBlock (id, "plantuml":opts, attrs) contents) =
    withPreloadedFile content $ \infile -> do
        figfmt <- case docfmt of
            "plain"    -> return "txt"
            "html"     -> return "svg"
            "html5"    -> return "svg"
            "slidy"    -> return "svg"
            "slideous" -> return "svg"
            "dzslides" -> return "svg"
            "revealjs" -> return "svg"
            "latex"    -> return "pdf"
            "beamer"   -> return "pdf"
            "pdf"      -> return "pdf"
            _          -> return ""
        if length(figfmt) > 0
            then do
                outfile <- renderFigure infile figfmt figdir prefix
                return (Para [Image [Str caption] (outfile, "")])
            else return (CodeBlock (id, "plantuml":opts, attrs) contents)
  where contentslist = split "Caption:" contents
        content      = head contentslist
        caption      = strip $ head $ tail $ contentslist ++ [""]
        prefix       = uniqueFilePrefix content
processDocument _ _ x = return x


main :: IO ()
main = do
    args <- getArgs
    figdir <- getEnv "PANDOC_PLANTUML_FIGDIR" 
                `catchIOError` \e -> return "plantuml-figures"
    let docfmt = head $ args ++ ["html"]
    toJSONFilter $ processDocument figdir docfmt
