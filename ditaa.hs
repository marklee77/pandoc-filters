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
--    0 parse arguments (optional output dir) (optional format)
--    1 get tmp file
--    2 write data to tmp
--    3 create output dir if needed
--    4 figure output file name...first available ditaafig-N.(fmt)
--    5 invoke ditaa

import Data.IORef
import System.IO
import System.IO.Temp(withSystemTempFile)
import System.Process
import Text.Pandoc

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return (\i -> do modifyIORef r (+i)
                   readIORef r)

doDitaa :: Counter -> Block -> IO Block
doDitaa counter cb@(CodeBlock (id, classes, namevals) contents) =
    if elem "ditaa" classes
        then withPreloadedFile contents $ \infile -> do
          cval <- counter 1
          let outfile = "image-" ++ show cval ++ ".png" in do
          system ("/usr/bin/ditaa " ++ infile ++ " " ++ outfile ++ " >/dev/null")
          return (Para [Image [] (outfile, "")])
        else return cb
doDitaa c x = return x

withPreloadedFile :: [Char] -> (FilePath -> IO a) -> IO a
withPreloadedFile content action = withSystemTempFile filenameTemplate callback
  where filenameTemplate     = "ditaa.md"
        callback path handle = hPutStr handle content >> hClose handle >> action path

main = do
  counter <- makeCounter
  toJsonFilter $ doDitaa counter
