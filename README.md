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

## Alternatives

- [enscript.el](https://www.emacswiki.org/emacs/EnscriptPrint)
  from the [Emacs Wiki](https://www.emacswiki.org/emacs/SiteMap).

- The
  [`ps-print`](https://www.gnu.org/software/emacs/manual/html_node/emacs/PostScript.html#PostScript)
  package that comes with Emacs for PostScript printing.  Implements
  `ps-print-buffer`, `ps-print-region`, and other functions.


- The
  [`lpr`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Printing.html)
  package that comes with Emacs.  Implements the `print-buffer`,
  `lpr-buffer`, `print-region`, and `lpr-region` functions.
