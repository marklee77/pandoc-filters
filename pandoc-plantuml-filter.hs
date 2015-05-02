-- ditaa.hs, a pandoc json filter to create inline diagrams with ditaa
-- Copyright (C) 2013 Mark Lee Stillwell
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
--
-- ditaa-json.hs
-- need to --    make sure ditaa is in path

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.IORef
import Data.String.Utils
import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.IO
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process
import Text.Pandoc.JSON


withPreloadedFile :: String -> (FilePath -> IO a) -> IO a
withPreloadedFile content action = 
    withSystemTempFile "plantuml.txt" callback
  where callback path handle = do
            hPutStr handle content 
            hClose handle 
            action path


getOutputFileName :: String -> String -> String -> IO String
getOutputFileName subdir content format = do
    createDirectoryIfMissing True subdir
    return $ combine subdir (addExtension (showDigest . sha1 .fromString $ content) format)


renderDiagrams :: String-> String -> Block -> IO Block
renderDiagrams subdir format (CodeBlock (id, "plantuml":opts, attrs) contents) =
    withPreloadedFile content $ \infile -> do
        outfile <- getOutputFileName subdir content "svg"
        case format of 
            "html" -> do system $ join " " ["plantuml", "-pipe", "-tsvg", optstring, "<", infile, ">", outfile ]
                      where optstring = join " --" ([""] ++ opts)
        return (Para [Image [Str caption] (outfile, "")])
  where contentslist = split "Caption:" contents
        content      = head contentslist
        caption      = strip $ head $ tail $ contentslist ++ [""]
renderDiagrams _ _ x = return x


main = do
    args <- getArgs
    subdirenv <- getEnv "PANDOC_PLANTUML_SUBDIR"
    let subdir = head $ [subdirenv] ++ ["plantuml-figures"]
        format = head $ args ++ ["svg"]
    putStrLn subdir
    toJSONFilter $ renderDiagrams subdir format
