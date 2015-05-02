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
import System.Environment (getArgs, getEnv)
import System.FilePath (addExtension, combine)
import System.IO (hClose, hPutStr)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import Text.Pandoc.JSON (toJSONFilter)

withPreloadedFile :: String -> (FilePath -> IO a) -> IO a
withPreloadedFile content action =
    withSystemTempFile "plantuml.txt" callback
  where callback path handle = do
        hPutStr handle content
        hClose handle
        action path

getOutputFileName :: String -> String -> String -> IO String
getOutputFileName dir content ext = 
    return $ combine dir (addExtension 
                             (showDigest . sha1 . fromString $ content) ext)

renderDiagram

renderDiagrams?

main :: IO ()
main = do
    args <- getArgs
    figdir <- getEnv "PANDOC_PLANTUML_FIGDIR" 
                `catchIOError` \e -> return "plantuml-figures"
    let format = head $ args ++ ["svg"]
    toJSONFilter $ renderFigures subdir format
