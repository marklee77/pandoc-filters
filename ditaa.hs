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
--    0 parse arguments (optional output template) (optional format)
--    1 make sure ditaa is in path
--    2 create output dir if needed
--    3 add format support...
--    4 add getting filename from namevals support
--    5 title and caption info...
--    6 other opts

import Data.IORef
import System.Environment
import System.IO
import System.IO.Temp(withSystemTempFile)
import System.Process
import Text.Pandoc


doDitaa :: Counter -> String -> Block -> IO Block
doDitaa counter template (CodeBlock (id, "ditaa":opts, attrs) diagram) = 
    withPreloadedFile diagram $ \infile -> do
        cval <- counter 1
        let outfile = template ++ show cval ++ ".png" in do
            system ("ditaa " ++ infile ++ " " ++ outfile ++ " >/dev/null")
            return (Para [Image [] (outfile, "")])
doDitaa _ _ x = return x


type Counter = Int -> IO Int
makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> modifyIORef r (+i) >> readIORef r)


withPreloadedFile :: String -> (FilePath -> IO a) -> IO a
withPreloadedFile content action = 
    withSystemTempFile "ditaa.md" callback
  where callback path handle = do
            hPutStr handle content 
            hClose handle 
            action path


main = do
  counter <- makeCounter
  argv <- getArgs
  toJsonFilter $ doDitaa counter (argv !! 0)
