Pandoc Filters
==============

Some filters that do transformations on the pandoc JSON AST representation. 
Mostly written using the 
[pandoc scripting](http://johnmacfarlane.net/pandoc/scripting.html) library.

ditaa.hs: use Text.Pandoc to parse out CodeBlocks annotated with ".ditaa" and 
pass them through [ditaa](http://ditaa.sourceforge.net/) to create inline 
graphics. Inspired by [mddia](https://github.com/nichtich/ditaa-markdown).

This is a work in progress, but the intended usage is:

    pandoc -t json input.md | runghc ditaa.hs [template] | pandoc -f json ...

Where *template* is an optional parameter of the form *directory/prefix.format*

And this is an annotated code block:

~~~~~ {.ditaa .no-separation}

+--------+   +-------+    +-------+
|        | --+ ditaa +--> |       |
|  Text  |   +-------+    |diagram|
|Document|   |!magic!|    |       |
|     {d}|   |       |    |       |
+---+----+   +-------+    +-------+
    :                         ^
    |       Lots of work      |
    +-------------------------+

~~~~~

