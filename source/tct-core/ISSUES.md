This file collects on-going issues and enhancements for `tct`.


### * interactive mode #bug #minor

Originally the `dyre` package has been used to provide customisation. For
`dyre` tct had to be in the `ghc-pkg` path. Now we mainly, but not necessarily,
rely on `stack` and `cabal sandbox`. So there is no easy way to provide
invocation of the interpreter. The easiest way now is to load the package in
`ghci` (when stack is used `stack ghic tct-<module>` and import the
`*Interactive` modules. Alternatively one may want to provide a dedicated
executable which imports the modules. 

The interactive flag should be removed from the command line flag and the
README accordingly updated.



### * certification type #enhancement #major

Commit (843c6f2) introduces YES/NO certificate. Unfortunately it relies on
partial functions. Though it shows a bigger problem, namely that the type of
certificate is fixed. For `tct-its` we rely on a hack, reading the bound from
the proof state rather than the proof certificate, to obtain more precise
bounds. 

One possibility it is to generalise the certificate type to more complex
expressions. Then one has to provide the corresponding combinators. This is far
from optimal, but easy.

Alternatively one may generalise the framework over the certificate type,
similar as it is done for the problem type. This is a major update and requires
updating all the other modules.



### * which processor #enhancement #minor

Provide means to check availability of used tools using system `which`. For
example `which "minismt"` should fail if error code of `which minismt` is
non-null. Integrate in competition and web strategies.



### * logging information #enhancement #minor

Intermediate nodes in the proof tree only store successful applications of
processor. Leafs may contain information why a processor failed. Though is
usually disregarded for more complex strategies using `try`. Sometimes it would
be nice to obtain more information. It is possible but cumbersome in the
interactive mode. An alternative would be to optionally log the result of an
processor application. This should be fairly easy: one could write (atomically)
to a log file when applying a processor depending on the runtime options.



###  * error handling #bug

System/user errors are catched when executing evaluate and return
a `ErroneousProcessor`. Due to the semantics of the strategy language errors
are usually silently ignored.



### * HTML output #enhancement

Provide an (interactive) HTML output. HTML output could be similar generated as
the Pretty and XML output is generated at the moment. First provide a skeleton
depending on the proof tree. If one uses the Pretty instance of processors as
a default then processors can overwrite the behaviour individually.



### * extend default proof output #enhancement #minor

Print certificate and time after proof.



### * spawn process #bug #minor

The core module uses an adapted version of `System.Process.spawn`, see
`Tct.Core.Common.Concurrent`. There have been a couple of bug-fixes over time
for the `System.Process` library. Ideally we could now rely on the library.



