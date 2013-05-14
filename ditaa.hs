-- ditaa-json.hs
-- need to
--    0 parse arguments (optional output dir) (optional format)
--    1 get tmp file
--    2 write data to tmp
--    3 create output dir if needed
--    4 figure output file name...first available ditaafig-N.(fmt)
--    5 invoke ditaa
import Text.Pandoc

doDitaa :: Block -> IO Block
doDitaa cb@(CodeBlock (id, classes, namevals) contents) =
    if elem "ditaa" classes
-- text, url, title
        then return (Para [Image [] ("image-1.png", "")])
        else return cb
doDitaa x = return x

main = toJsonFilter doDitaa
