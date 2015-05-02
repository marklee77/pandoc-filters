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
import System.Environment
import System.FilePath
import System.IO
import System.IO.Temp(withSystemTempFile)
import System.Process
import Text.Pandoc.JSON


withPreloadedFile :: String -> String -> (FilePath -> IO a) -> IO a
withPreloadedFile content template action = 
    withSystemTempFile template callback
  where callback path handle = do
            hPutStr handle content 
            hClose handle 
            action path


getOutputFileName :: String -> String -> IO String
getOutputFileName content format = do
    createDirectoryIfMissing True directory
    return $ combine directory (addExtension (showDigest . sha1 .fromString $ content) format)
  where directory = "plantuml-figures"


renderDiagrams :: String -> Block -> IO Block
renderDiagrams format (CodeBlock (id, "plantuml":opts, attrs) contents) =
    withPreloadedFile diagram "plantuml.txt" $ \infile -> do
        outfile <- getOutputFileName diagram "svg"
        case format of 
            "html" -> do system $ join " " ["plantuml", "-pipe", "-tsvg", optstring, "<", infile, ">", outfile ]
                      where optstring = join " --" ([""] ++ opts)
        return (Para [Image [Str caption] (outfile, "")])
  where contentslist = split "Caption:" contents
        diagram      = head contentslist
        caption      = strip $ head $ tail $ contentslist ++ [""]
renderDiagrams _ x = return x


main = do
    args <- getArgs
    cref <- newIORef 0
    let format = head $ args ++ ["svg"]
    toJSONFilter $ renderDiagrams format
