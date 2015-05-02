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


getOutputFileName :: FilePath -> IO Int -> IO String
getOutputFileName format counter = do
    createDirectoryIfMissing True directory
    cval <- counter
    return $ combine directory (addExtension ("fig-" ++ show cval) format)
  where directory = "figures"


renderDiagrams :: String -> IO Int -> Block -> IO Block
renderDiagrams format counter (CodeBlock (id, "plantuml":opts, attrs) contents) =
    withPreloadedFile diagram "plantuml.txt" $ \infile -> do
        outfile <- getOutputFileName "svg" counter
        case format of 
            "html" -> do system $ join " " ["plantuml", "-pipe", "-tsvg", optstring, "<", infile, ">", outfile ]
                      where optstring = join " --" ([""] ++ opts)
        return (Para [Image [Str caption] (outfile, "")])
  where contentslist = split "Caption:" contents
        diagram      = head contentslist
        caption      = strip $ head $ tail $ contentslist ++ [""]
renderDiagrams _ _ x = return x


main = do
    args <- getArgs
    cref <- newIORef 0
    let format = head $ args ++ ["svg"]
        counter = modifyIORef cref (+1) >> readIORef cref
    toJSONFilter $ renderDiagrams format counter
