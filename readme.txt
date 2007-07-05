System.FilePath
===============

I have written a System.FilePath module in part based on the one in
Yhc, and in part based on the one in Cabal (thanks to Lemmih). The aim
is to try and get this module into the base package, as FilePath's are
something many programs use, but its all too easy to hack up a little
function that gets it right most of the time on most platforms, and
there lies a source of bugs.

This module is Posix (Linux) and Windows capable - just import
System.FilePath and it will pick the right one. Of course, if you
demand Windows paths on all OS's, then System.FilePath.Windows will
give you that (same with Posix). Written in Haskell 98 + Heirarchical
Modules.

Haddock: http://www-users.cs.york.ac.uk/~ndm/projects/filepath/System-FilePath.html
Darcs: darcs get http://www.cs.york.ac.uk/fp/darcs/filepath
Source: http://www.cs.york.ac.uk/fp/darcs/filepath/System/FilePath.hs
Homepage: http://www-users.cs.york.ac.uk/~ndm/projects/libraries.php

If you go to the haddock page there are a few little examples at the
top of the file.


Acknowledgements
----------------

Thanks to Marc Webber, shapr, David House, Lemmih, others...


Competitors
-----------

System.FilePath from Cabal, by Lemmih
FilePath.hs and NameManip.hs from MissingH

The one from Cabal and FilePath.hs in MissingH are both very similar, I
stole lots of good ideas from those two.

NameManip seems to be more unix specific, but all functions in that module
have equivalents in this new System.FilePath module.

Hopefully this new module can be used without noticing any lost functions,
and certainly adds new features/functions to the table.


Should FilePath by an abstract data type?
-----------------------------------------

The answer for this library is no. This is a deliberate design decision.

In Haskell 98 the definition is 'type FilePath = String', and all functions
operating on FilePath's, i.e. readFile/writeFile etc take FilePath's. The
only way to introduce an abstract type is to provide wrappers for these
functions or casts between String's and FilePathAbstract's.

There are also additional questions as to what constitutes a FilePath, and
what is just a pure String. For example, "/path/file.ext" is a FilePath. Is
"/" ? "/path" ? "path" ? "file.ext" ? ".ext" ? "file" ?

With that being accepted, it should be trivial to write
System.FilePath.ByteString which has the same interface as System.FilePath
yet operates on ByteString's.
