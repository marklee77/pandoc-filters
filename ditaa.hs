-- ditaa-json.hs
-- need to
--    0 parse arguments (optional output dir) (optional format)
--    1 get tmp file
--    2 write data to tmp
--    3 create output dir if needed
--    4 figure output file name...first available ditaafig-N.(fmt)
--    5 invoke ditaa
import Text.Pandoc

import System.IO
import System.IO.Temp(withSystemTempFile)
import System.Process

doDitaa :: Block -> IO Block
doDitaa cb@(CodeBlock (id, classes, namevals) contents) =
    if elem "ditaa" classes
-- text, url, title
        then withPreloadedFile contents $ \path -> 
          system ("/usr/bin/ditaa " ++ path ++ " >/dev/null") >>
          return (Para [Image [] ("image-1.png", "")])
        else return cb
doDitaa x = return x

withPreloadedFile :: [Char] -> (FilePath -> IO a) -> IO a
withPreloadedFile content action = withSystemTempFile filenameTemplate callback
  where filenameTemplate     = "ditaa.md"
        callback path handle = hPutStr handle content >> hClose handle >> action path

main = toJsonFilter doDitaa
