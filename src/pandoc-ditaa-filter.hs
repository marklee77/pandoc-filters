-- pandoc-ditaa-filter.hs, a pandoc json filter to create inline diagrams with 
-- ditaa
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
getOutputFileName template counter = do
    createDirectoryIfMissing True directory
    cval <- counter
    return (addExtension (prefix ++ show cval) extension)
  where directory = takeDirectory template
        (prefix, extension) = splitExtension template


renderDiagrams :: String -> IO Int -> Block -> IO Block
renderDiagrams template counter (CodeBlock (id, "ditaa":opts, attrs) contents) =
    withPreloadedFile diagram "ditaa.md" $ \infile -> do
        outfile <- getOutputFileName template counter
        case takeExtension template of 
            ".png" -> do system $ join " " ["ditaa", optstring, infile, outfile,
                                            ">/dev/null"]
                       where optstring = join " --" ([""] ++ opts)
            ".eps" -> do system $ join " " ["ditaaeps", optstring, infile, 
                                            outfile, ">/dev/null"]
                       where optstring = join " --" ([""] ++ opts)
            ".pdf" -> do withSystemTempFile "ditaa.eps" callback
                       where callback epsfile handle = do
                                 hClose handle
                                 system $ join " " ["ditaaeps", optstring, 
                                                    infile, epsfile, 
                                                    ">/dev/null"]
                                 system $ 
                                     "epstopdf " ++ epsfile ++ " -o=" ++ outfile
                               where optstring = join " --" ([""] ++ opts)
        return (Para [Image [Str caption] (outfile, "")])
  where contentslist = split "Caption:" contents
        diagram      = head contentslist
        caption      = strip $ head $ tail $ contentslist ++ [""]
renderDiagrams _ _ x = return x


main = do
    args <- getArgs
    cref <- newIORef 0
    let template = head $ args ++ ["image.png"]
        counter  = modifyIORef cref (+1) >> readIORef cref
    toJSONFilter $ renderDiagrams template counter
