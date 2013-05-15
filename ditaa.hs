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
-- need to
--    1 make sure ditaa is in path
--    3 add format support...
--    4 add getting filename from namevals support
--    5 title and caption info...
--    6 other opts

import Data.IORef
import Data.String.Utils
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Temp(withSystemTempFile)
import System.Process
import Text.Pandoc


type Counter = Int -> IO Int
makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> modifyIORef r (+i) >> readIORef r)


withPreloadedFile :: String -> String -> (FilePath -> IO a) -> IO a
withPreloadedFile content template action = 
    withSystemTempFile template callback
  where callback path handle = do
            hPutStr handle content 
            hClose handle 
            action path


getOutputFileName :: FilePath -> Counter -> IO String
getOutputFileName template counter = do
    createDirectoryIfMissing True directory
    cval <- counter 1
    return (prefix ++ show cval ++ extension)
  where directory = takeDirectory template
        (prefix, extension) = splitExtension template


renderDiagrams :: String -> Counter -> Block -> IO Block
renderDiagrams template counter (CodeBlock (id, "ditaa":opts, attrs) diagram) = 
    withPreloadedFile diagram "ditaa.md" $ \infile -> do
        outfile <- getOutputFileName template counter
        system ("ditaa" ++ optionstr ++ " " ++ infile ++ " " ++ outfile ++ " >/dev/null")
        return (Para [Image [] (outfile, "")])
      where optionstr = join " --" ([""] ++ opts)
renderDiagrams _ _ x = return x


main = do
    counter <- makeCounter
    args <- getArgs
-- TODO: add options for ditaa command / arguments
    let template = head $ args ++ ["image.png"] in
        toJsonFilter $ renderDiagrams template counter
