# enscript-print

An Emacs Lisp package implementing two interactively called functions,
`enscript-print-buffer` and `enscript-print-region`, which are fancy
wrappers that pipe the appropriate contents to [GNU
Enscript](https://www.gnu.org/software/enscript/), a program that
converts text to PostScript and (by default) sends the results to a
PostScript printer.

There are plenty of customizable options in the `enscript-print`
customize group you can use to activate GNU Enscript's options.  There
will be more.

This should work with [GNU Emacs](https://www.gnu.org/software/emacs/)
25.2 and up.  May work with earlier versions, but I can make no
guarantees on this.
