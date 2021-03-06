;;; enscript-print.el --- Printing with GNU Enscript  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Darren Embry

;; Author: Darren Embry <dse@webonastick.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Two interactively called functions, `enscript-print-buffer' and
;; `enscript-print-region', which are fancy wrappers that pipe the
;; appropriate contents to GNU Enscript.  There are plenty of
;; customizable options in the `enscript-print' customize group.
;; There will be more.

;; TODO: also support recode if it is installed and iconv is not.
;; TODO: support built-in recode/iconv?

;;; Code:

(defgroup enscript-print nil
  "Printing with GNU Enscript"
  :group 'printing)

(defcustom enscript-print-executable "enscript"
  "Name of GNU Enscript executable."
  :group 'enscript-print
  :safe #'stringp
  :type '(string))

;; negation of --no-header
(defcustom enscript-print-header t
  "Print page headers."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --escapes
;; TODO: support --escapes=<char>
(defcustom enscript-print-escapes nil
  "Enable special escapes interpretation."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --silent
(defcustom enscript-print-silent nil
  "Suppress all output except for fatal error messages."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --borders
(defcustom enscript-print-borders nil
  "Print borders around columns."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --page-prefeed
(defcustom enscript-print-enable-page-prefeed nil
  "Enable page prefeed."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --media
(defcustom enscript-print-media nil
  "Select an output media (paper size).

Media for common paper sizes include `A4' and `Letter'.

Run `enscript --list-media' for a list of all known output media."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (stringp x)))
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (string :tags "Media name (enscript --list-media)" "A4")))

;; --font=<font>
(defcustom enscript-print-font-name nil
  "Select a font for the body text.

If value is nil, either \"Courier\" or whatever is specified in
~/.enscriptrc is used."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (stringp x)))
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (const :tags "Use Courier" "Courier")
                 (string :tags "Custom Font" "Courier")))

;; --font=<font>
(defcustom enscript-print-font-size nil
  "Specify a font size for the body text, in PostScript points.

If value is nil, either 10 points or whatever is specified in
~/.enscriptrc is used.

If landscape multi-column printing is done, the default font size
changes from 10 points to 7 points.

If a value for `enscript-print-font-height' is specified, this
specifies the font's width and `enscript-print-font-height'
specifies the font's height.  Otherwise, this specifies both the
font's width and height."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (numberp x)
                               (>= x 0))))
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (const :tags "10pt" 10)
                 (const :tags "7pt" 7)
                 (number :tags "Custom font size in points" 10)))

;; --font=<font>
(defcustom enscript-print-font-height nil
  "Specify a font **height** for the body text, in PostScript points.

If the value is nil, maintain the font's original aspect ratio."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (numberp x)
                               (>= x 0))))
  :type '(choice (const :tags "Maintain font's original aspect ratio" nil)
                 (number :tags "Custom font height in points" 10)))

;; --header-font=<font>
(defcustom enscript-print-header-font-name nil
  "Select a font for the header.

If value is nil, either \"Courier\" or whatever is specified in
~/.enscriptrc is used."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (stringp x)))
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (const :tags "Use Courier" "Courier")
                 (string :tags "Custom Font" "Courier")))

;; --header-font=<font>
(defcustom enscript-print-header-font-size nil
  "Specify a font size for the header, in PostScript points.

If value is nil, either 10 points or whatever is specified in
~/.enscriptrc is used.

If landscape multi-column printing is done, the default font size
changes from 10 points to 7 points.

If a value for `enscript-print-header-font-height' is specified, this
specifies the font's width and `enscript-print-header-font-height'
specifies the font's height.  Otherwise, this specifies both the
font's width and height."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (numberp x)
                               (>= x 0))))
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (const :tags "10pt" 10)
                 (const :tags "7pt" 7)
                 (number :tags "Custom font size in points" 10)))

;; --header-font=<font>
(defcustom enscript-print-header-font-height nil
  "Specify a font **height** for the header, in PostScript points.

If the value is nil, maintain the font's original aspect ratio."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (numberp x)
                               (>= x 0))))
  :type '(choice (const :tags "Maintain font's original aspect ratio" nil)
                 (number :tags "Custom font height in points" 10)))

;; --landscape
(defcustom enscript-print-landscape nil
  "Print in the landscape mode; rotate page 90 degrees."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --columns=<num>
(defcustom enscript-print-columns 1
  "Specify how many columns each page has."
  :group 'enscript-print
  :safe #'(lambda (x) (and (integerp x)
                           (>= x 1)))
  :type '(integer))

;; --highlight-bars[=<num>]
(defcustom enscript-print-highlight-bars nil
  "Print highlight bars, and optionally specify how high they are in lines."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (integerp x)
                               (>= x 1))))
  :type '(choice (const :tags "Off" nil)
                 (const :tags "On" t)
                 (integer :tags "Height of highlight bars in lines" 2)))

;; --highlight-bar-gray=<gray>
(defcustom enscript-print-highlight-bars-gray-level 0.9
  "Specify gray level used in printing highlight bars (0 = black; 1 = white)."
  :group 'enscript-print
  :safe #'numberp
  :type '(number))

(define-obsolete-variable-alias
  'enscript-print-highlight-bar-gray-level
  'enscript-print-highlight-bars-gray-level "25.1")

;; --baselineskip=<num>
(defcustom enscript-print-baseline-skip nil
  "Specify the baseline skip, in PostScript points."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (numberp x)
                               (> x 0))))
  :type '(choice (const :tags "Default" nil)
                 (number :tags "Number of PostScript points" 1)))

(defcustom enscript-print-exclude-emacs-local-variables nil
  "Exclude the Emacs \"Local Variables\" section at the end of the file."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --tabsize=<num>
(defcustom enscript-print-tab-size nil
  "Set the tabulator size, in number of characters."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (integerp x)
                               (>= x 1))))
  :type '(choice (const :tags "Default" nil)
                 (integer :tags "Number of characters" 8)))

;; --verbose[=<level>]
(defcustom enscript-print-verbose-level nil
  "Tell what enscript is doing."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (integerp x)
                               (>= x 0))))
  :type '(choice (const :tags "No" nil)
                 (integer :tags "Verbose level (1 or more)" 1)))

;; --copies=<num>
(defcustom enscript-print-number-of-copies nil
  "Number of copies to print."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (and (integerp x)
                               (>= x 0))))
  :type '(choice (const :tags "Default" nil)
                 (integer :tags "Number of copies" 2)))

;; --margins=<left>:<right>:<top>:<bottom>
(defcustom enscript-print-left-margin nil
  "Left page marginal, in PostScript points."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (numberp x)))
  :type '(choice (const :tags "Default" nil)
                 (integer :tags "Number of PostScript points" 10)))

;; --margins=<left>:<right>:<top>:<bottom>
(defcustom enscript-print-right-margin nil
  "Right page marginal, in PostScript points."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (numberp x)))
  :type '(choice (const :tags "Default" nil)
                 (integer :tags "Number of PostScript points" 10)))

;; --margins=<left>:<right>:<top>:<bottom>
(defcustom enscript-print-top-margin nil
  "Top page marginal, in PostScript points."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (numberp x)))
  :type '(choice (const :tags "Default" nil)
                 (integer :tags "Number of PostScript points" 10)))

;; --margins=<left>:<right>:<top>:<bottom>
(defcustom enscript-print-bottom-margin nil
  "Bottom page marginal, in PostScript points."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (numberp x)))
  :type '(choice (const :tags "Default" nil)
                 (integer :tags "Number of PostScript points" 10)))

;; --truncate-lines
(defcustom enscript-print-truncate-lines nil
  "Cut lines that are too long for the page."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --mark-wrapped-lines[=<style>]
(defcustom enscript-print-mark-wrapped-lines nil
  "Mark wrapped lines in output, optionally with a specified style."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (eq x 'plus)
                          (eq x 'box)
                          (eq x 'arrow)))
  :type '(choice (const :tags "do not print (default)"                    nil)
                 (const :tags "plus (+) sign to end of each wrapped line" 'plus)
                 (const :tags "black box to end of each wrapped line"     'box)
                 (const :tags "small arrow to end of each wrapped line"   'arrow)))

;; --printer=<name>
(defcustom enscript-print-printer-name nil
  "Sppol the output to the specified printer.

Leave nil to use the value of `printer-name'."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (stringp x)))
  :type '(choice (const :tags "Use `printer-name' or default printer." nil)
                 (string :tags "Specify a printer name" "printer")))

;; --line-numbers[=<start-line>]
(defcustom enscript-print-line-numbers nil
  "Precede each line with its line number."
  :safe #'(lambda (x) (or (booleanp x)
                          (integerp x)))
  :type '(choice (const :tags "No (default)" nil)
                 (const :tags "Yes" 1)
                 (integer :tags "Specify the line number of the first line." 1)))

;; --no-job-header
(defcustom enscript-print-no-job-header nil
  "Suppress printing of the job header page."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --indent=<indent>
(defcustom enscript-print-indent nil
  "Indent every line this many characters (or `enscript-print-indent-units')."
  :group 'enscript-print
  :safe #'numberp
  :type '(choice (const :tags "No indentation (default)" nil)
                 (number :tags "Number of characters or units" 4)))

;; --indent=<indent>
(defcustom enscript-print-indent-units nil
  "Units to use instead of characters for `enscript-print-indent'."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (eq x 'centimeters)
                          (eq x 'inches)
                          (eq x 'points)))
  :type '(choice (const :tags "characters (default)" nil)
                 (const :tags "centimeters" 'centimeters)
                 (const :tags "inches" 'inches)
                 (const :tags "PostScript points" 'points)))

;; --missing-characters
(defcustom enscript-print-missing-characters nil
  "Print a listing of character codes which couldn't be printed."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

;; --non-printable-format=<format>
(defcustom enscript-print-non-printable-format nil
  "Specify how non-printable characters are printed."
  :group 'enscript-print
  :safe #'(lambda (x) (or (not x)
                          (eq x 'caret)
                          (eq x 'questionmark)
                          (eq x 'space)))
  :type '(choice (const :tags "Octal (default)" nil)
                 (const :tags "Caret" 'caret)
                 (const :tags "Question mark" 'questionmark)
                 (const :tags "Space" 'space)))

(defcustom enscript-print-coding-system-for-read 'utf-8-unix
  "Value for `coding-system-for-read' when piping to iconv and enscript.

You *should* never have to change this."
  :group 'enscript-print
  :safe #'symbolp
  :type '(symbol))

(defcustom enscript-print-coding-system-for-write 'utf-8-unix
  "Value for `coding-system-for-write' when piping to iconv and enscript.

You *should* never have to change this."
  :group 'enscript-print
  :safe #'symbolp
  :type '(symbol))

(defcustom enscript-print-iconv-source-encoding "utf-8"
  "Encoding to pass to iconv via its `-f' option for reading.

You *should* never have to change this.

Run `iconv --list' to see a list of supported encodings."
  :group 'enscript-print
  :safe #'stringp
  :type '(string))

(defcustom enscript-print-iconv-destination-encoding "iso-8859-1"
  "Encoding to pass to iconv via its `-t' option for piping to enscript.

If NIL, iconv defaults to the current locale.

Corresponds to the value of
`enscript-print-input-encoding', but does not
necessarily have the same name.

Run `iconv --list' to see a list of supported encodings as
specified to the iconv program.

You need not append `//TRANSLIT' or `//IGNORE' to this variable;
see `enscript-print-iconv-transliterate-nonrepresentable-characters'
and `enscript-print-iconv-ignore-nonrepresentable-characters'."
  :group 'enscript-print
  :safe #'stringp
  :type '(string))

(defcustom enscript-print-iconv-transliterate-nonrepresentable-characters t
  "Translate nonrepresentable characters to approximations.

If a character is not representable in
`enscript-print-iconv-destination-encoding', it is converted to
the closest matching character in that encoding, or to a
replacement character, often a question mark.

This appends //TRANSLIT to that variable's value when specifying
it as the destination encoding for iconv."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

(defcustom enscript-print-iconv-ignore-nonrepresentable-characters t
  "Silently ignore invalid sequences and nonrepresentable characters.

If a character is not representable in
`enscript-print-iconv-destination-encoding', or an invalid byte
sequence is encountered, it is silently ignored.

This appends //IGNORE to that variable's value when specifying
it as the destination encoding for iconv."
  :group 'enscript-print
  :safe #'booleanp
  :type '(boolean))

(defcustom enscript-print-input-encoding "latin1"
  "Encoding to pass to enscript via its `--encoding' option.

If NIL, enscript defaults to \"latin1\".

Corresponds to the value of
`enscript-print-iconv-destination-encoding', but does not
necessarily have the same name.

See the enscript man page for a list of supported encodings as
specified to the enscript program."
  :group 'enscript-print
  :safe #'stringp
  :type '(string))

(define-obsolete-variable-alias
  'enscript-print/coding-system-for-read
  'enscript-print-coding-system-for-read)
(define-obsolete-variable-alias
  'enscript-print/coding-system-for-write
  'enscript-print-coding-system-for-write)
(define-obsolete-variable-alias
  'enscript-print/iconv-source-encoding
  'enscript-print-iconv-source-encoding)
(define-obsolete-variable-alias
  'enscript-print/iconv-destination-encoding
  'enscript-print-iconv-destination-encoding)
(define-obsolete-variable-alias
  'enscript-print/enscript-input-encoding
  'enscript-print-input-encoding)
  
(defun enscript-print-printer-name ()
  "Return the default printer name used for enscript-print.

Returns the value of `enscript-print-printer-name' if non-nil.
Otherwise returns the value of `printer-name'.
`printer-name'."
  (or enscript-print-printer-name
      printer-name))

;; Stolen from https://stackoverflow.com/questions/969067/name-of-this-function-in-built-in-emacs-lisp-library
(defun enscript-print/flatten (LIST)
  "Return a new flattened list from LIST."
  (cond
   ((atom LIST) (list LIST))
   ((null (cdr LIST)) (enscript-print/flatten (car LIST)))
   (t (append (enscript-print/flatten (car LIST))
              (enscript-print/flatten (cdr LIST))))))

(defun enscript-print/shell-concat (command-line &rest arguments)
  "Take COMMAND-LINE; append ARGUMENTS; return new command line."
  (let ((command-line command-line))
    (dolist (argument (enscript-print/flatten arguments) command-line)
      (if argument
          (setq command-line
                (concat command-line " "
                        (shell-quote-argument argument)))))))

(defun enscript-print/point-before-local-variables-line ()
  "Return location after last non-blank line before \"Local Variables:\" line.

Return nil if \"Local Variables:\" line is not found."
  ;; partly based on files-x.el
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'noerror)
    (when (let ((case-fold-search t))
            (search-forward (concat "Local Variables" ":") nil t))
      (forward-line 0) ;go to beginning of line, ignoring field boundaries
      (when (search-backward-regexp "[^[:space:]\r\n]" nil 'noerror)
        ;; at this point, point is now on last non-whitespace
        ;; character.
        (forward-line 1)                ;go to beginning of next line
        (point)))))                     ;return location

(defun enscript-print/point-max ()
  "Return end of buffer for enscript printing purposes.

If `enscript-print-exclude-emacs-local-variables' is non-nil,
return either the location after the end of the line containing
the last non-whitespace character before the \"Local Variables:\"
line, or the value of `(point-max)'.

If `enscript-print-exclude-emacs-local-variables' is nil,
return `(point-max)'."
  (if enscript-print-exclude-emacs-local-variables
      (or (enscript-print/point-before-local-variables-line)
          (point-max))
    (point-max)))

(defun enscript-print/font-string (font-name font-size font-height)
  "Given FONT-NAME, FONT-SIZE, and FONT-HEIGHT, return a font spec.

The font spec is used as the value of the `--font' and
`--header-font' options."
  (concat (format "%s@%g" (or font-name "Courier") (or font-size 10))
          (if font-height (format "/%g" font-height) "")))

(defun enscript-print/buffer-name-to-file-name (buffer-name)
  "Convert BUFFER-NAME to a filename without extension.

Removes trailing and leading non-alphanumeric characters, and
replaces each remaining contiguous sequence of non-alphanumeric
chsracters with a hyphen.

Returns NIL if BUFFER-NAME is not a string, or if it contains
no alphanumeric characters."
  (if (and buffer-name (stringp buffer-name))
      (let ((result buffer-name))
        (setq result (replace-regexp-in-string "\\`[^[:alnum:]]+" ""  result))
        (setq result (replace-regexp-in-string "[^[:alnum:]]+\\'" ""  result))
        (setq result (replace-regexp-in-string "[^[:alnum:]]+"    "-" result))
        (if (equal result "") nil result))
    nil))

(defun enscript-print/iconv-destination-encoding ()
  "Return the full string specified as iconv's -t option."
  (concat enscript-print-iconv-destination-encoding
          (if enscript-print-iconv-transliterate-nonrepresentable-characters "//TRANSLIT" "")
          (if enscript-print-iconv-ignore-nonrepresentable-characters        "//IGNORE"   "")))

(defun enscript-print/iconv-command-line ()
  "Return the command line for running iconv for piping to enscript."
  (enscript-print/shell-concat
   "iconv"
   (if enscript-print-iconv-source-encoding
       (list "-f" enscript-print-iconv-source-encoding))
   (if enscript-print-iconv-destination-encoding
       (list "-t" (enscript-print/iconv-destination-encoding)))))

(defun enscript-print/enscript-command-line (&optional print-to-postscript-file)
  "Return the command line for running enscript, piped from iconv.

Optional argument PRINT-TO-POSTSCRIPT-FILE, if non-nil, instructs
enscript to output to a PostScript file instead of printing.

The name of the PostScript file is the buffer's filename, with
`.ps' appended.

If PRINT-TO-POSTSCRIPT-FILE is equal to the string \"-\", command
sends PostScript to standard output."
  (enscript-print/shell-concat
   enscript-print-executable
   (if print-to-postscript-file
       (if (equal print-to-postscript-file "-")
           "--output=-"
         (format "--output=%s"
                 (enscript-print/postscript-file-name-for-buffer))))
   (format "--encoding=%s" enscript-print-input-encoding)
   (if enscript-print-escapes "--escapes")
   (if (not enscript-print-header) "--no-header")
   (if enscript-print-borders "--borders")
   (if enscript-print-media (format "--media=%s" enscript-print-media))
   (if enscript-print-enable-page-prefeed "--page-prefeed")
   (if (or enscript-print-font-name enscript-print-font-size)
       (format "--font=%s" (enscript-print/font-string
                            enscript-print-font-name
                            enscript-print-font-size
                            enscript-print-font-height)))
   (if (or enscript-print-header-font-name enscript-print-header-font-size)
       (format "--header-font=%s" (enscript-print/font-string
                                   enscript-print-header-font-name
                                   enscript-print-header-font-size
                                   enscript-print-header-font-height)))
   (if (or enscript-print-left-margin
           enscript-print-right-margin
           enscript-print-top-margin
           enscript-print-bottom-margin)
       (format "--margins=%s:%s:%s:%s"
               (if enscript-print-left-margin   (format "%f" enscript-print-left-margin)   "")
               (if enscript-print-right-margin  (format "%f" enscript-print-right-margin)  "")
               (if enscript-print-top-margin    (format "%f" enscript-print-top-margin)    "")
               (if enscript-print-bottom-margin (format "%f" enscript-print-bottom-margin) "")))
   (if enscript-print-baseline-skip
       (format "--baselineskip=%f" enscript-print-baseline-skip))
   (if enscript-print-tab-size
       (format "--tabsize=%d" enscript-print-tab-size))
   (if enscript-print-verbose-level
       (format "--verbose=%d" enscript-print-verbose-level))
   (if enscript-print-landscape "--landscape")
   (if enscript-print-columns
       (format "--columns=%d" enscript-print-columns))
   (if enscript-print-highlight-bars
       (if (numberp enscript-print-highlight-bars)
           (list (format "--highlight-bars=%d"
                         enscript-print-highlight-bars)
                 (format "--highlight-bar-gray=%g"
                         enscript-print-highlight-bars-gray-level))
         (list "--highlight-bars"
               (format "--highlight-bar-gray=%g"
                       enscript-print-highlight-bars-gray-level))))
   (if enscript-print-number-of-copies
       (format "--copies=%d" enscript-print-number-of-copies))
   (if enscript-print-truncate-lines "--truncate-lines")
   (if enscript-print-mark-wrapped-lines
       (cond ((eq enscript-print-mark-wrapped-lines 'plus)
              "--mark-wrapped-lines=plus")
             ((eq enscript-print-mark-wrapped-lines 'box)
              "--mark-wrapped-lines=box")
             ((eq enscript-print-mark-wrapped-lines 'arrow)
              "--mark-wrapped-lines=arrow")))
   (let ((the-printer-name (enscript-print-printer-name)))
     (if the-printer-name
         (format "--printer=%s" the-printer-name)))
   (if enscript-print-no-job-header "--no-job-header")
   (if enscript-print-indent
       (cond ((not enscript-print-indent-units)
              (format "--indent=%f" enscript-print-indent))
             ((eq enscript-print-indent-units 'centimeters)
              (format "--indent=%fc" enscript-print-indent))
             ((eq enscript-print-indent-units 'inches)
              (format "--indent=%fi" enscript-print-indent))
             ((eq enscript-print-indent-units 'points)
              (format "--indent=%fp" enscript-print-indent))))
   (if enscript-print-missing-characters "--missing-characters")
   (if enscript-print-non-printable-format
       (cond ((not enscript-print-non-printable-format)
              "--non-printable-format=octal")
             ((eq enscript-print-non-printable-format 'caret)
              "--non-printable-format=caret")
             ((eq enscript-print-non-printable-format 'questionmark)
              "--non-printable-format=questionmark")
             ((eq enscript-print-non-printable-format 'space)
              "--non-printable-format=space")))
   (if enscript-print-silent "--silent")
   (if enscript-print-line-numbers
       (cond ((booleanp enscript-print-line-numbers)
              "--line-numbers")
             ((integerp enscript-print-line-numbers)
              (format "--line-numbers=%d" enscript-print-line-numbers))))))

(defun enscript-print/postscript-file-name-for-buffer (&optional ext)
  "Return PostScript file name for buffer.  Optional EXT overrides \".ps\"."
  (let ((ext (or ext ".ps")))
    (cond ((buffer-file-name) (concat (buffer-file-name) ext))
          ((buffer-name)
           (concat (enscript-print/buffer-name-to-file-name
                    (buffer-name)) ext))
          (t (concat "emacs-enscript" ext)))))

(defun enscript-print/pdf-file-name-for-buffer ()
  "Return PDF file name for buffer."
  (enscript-print/postscript-file-name-for-buffer ".pdf"))

;; return X if X and X is not "-"
(defun enscript-print/but-not-equal (a b)
  "Convenience function to return A if A is equal to B."
  (cond ((equal a b) nil)
        (t a)))

(defun enscript-print/ps2pdf-command-line (postscript-file-name pdf-file-name)
  "Return command to convert PostScript to PDF using ps2pdf.

String arguments POSTSCRIPT-FILE-NAME and PDF-FILE-NAME specify
the input and output filenames.

If POSTSCRIPT-FILE-NAME is NIL or \"-\", command reads from
standard input.

If ps2pdf utility is not found, return NIL.

See also `enscript-print/pstopdf-command-line'."
  (let ((ps2pdf-location (executable-find "ps2pdf")))
    (if ps2pdf-location
        (enscript-print/shell-concat ps2pdf-location
                                     (or (enscript-print/but-not-equal
                                          postscript-file-name "-")
                                         "-")
                                     pdf-file-name))))

(defun enscript-print/pstopdf-command-line (postscript-file-name pdf-file-name)
  "Return command to convert PostScript to PDF using pstopdf.

String arguments POSTSCRIPT-FILE-NAME and PDF-FILE-NAME specify
the input and output filenames.

If POSTSCRIPT-FILE-NAME is NIL or \"-\", command reads from
standard input.

If pstopdf utility is not found, return NIL.

See also `enscript-print/ps2pdf-command-line'."
  (let ((pstopdf-location (executable-find "pstopdf")))
    (if pstopdf-location
        (enscript-print/shell-concat pstopdf-location
                                     (or (enscript-print/but-not-equal
                                          postscript-file-name "-")
                                         nil)
                                     "-o" pdf-file-name))))

(defun enscript-print/postscript-to-pdf-command-line (postscript-file-name pdf-file-name)
  "Return command to convert POSTSCRIPT-FILE-NAME to PDF-FILE-NAME.

If POSTSCRIPT-FILE-NAME is NIL or \"-\", command reads from
standard input.

If ps2pdf is found, command will use that utility.
Otherwise, if pstopdf is found, command will use that utility.
Otherwise, this function throws an error."
  (or (enscript-print/ps2pdf-command-line postscript-file-name pdf-file-name)
      (enscript-print/pstopdf-command-line postscript-file-name pdf-file-name)
      (error "%s" "No `ps2pdf' or `pstopdf' utility found")))

(defun enscript-print-command-line (&optional print-to-postscript-file
                                              print-to-pdf-file)
  "Return the command line for enscript printing.

Optional argument PRINT-TO-POSTSCRIPT-FILE has the same meaning
as in the function `enscript-command-line'.

Optional argument PRINT-TO-PDF-FILE, if non-nil, instructs us to
output a .pdf file."
  (if print-to-pdf-file
      (if print-to-postscript-file
          (format "%s | %s && %s"
                  (enscript-print/iconv-command-line)
                  (enscript-print/enscript-command-line
                   print-to-postscript-file)
                  (enscript-print/postscript-to-pdf-command-line
                   (enscript-print/postscript-file-name-for-buffer)
                   (enscript-print/pdf-file-name-for-buffer)))
        (format "%s | %s | %s"
                (enscript-print/iconv-command-line)
                (enscript-print/enscript-command-line "-")
                (enscript-print/postscript-to-pdf-command-line
                 "-"
                 (enscript-print/pdf-file-name-for-buffer))))
    (format "%s | %s"
            (enscript-print/iconv-command-line)
            (enscript-print/enscript-command-line
             print-to-postscript-file))))

;;;###autoload
(defun enscript-print-buffer (&optional arg)
  "Print the contents of the buffer using enscript.

Supply a negative optional argument -1 to ARG to output a
PostScript file.  (M-- M-x ...)

Supply negative argument -2 to output a PDF file.  (M-- M-2 M-x ...)

Supply negative argument -3 to output PostScript and PDF files."
  (interactive "p")
  (let ((coding-system-for-read enscript-print-coding-system-for-read)
        (coding-system-for-write enscript-print-coding-system-for-write)
        (print-to-postscript-file (and (numberp arg) (or (= arg -1) (= arg -3))))
        (print-to-pdf-file        (and (numberp arg) (or (= arg -2) (= arg -3)))))
    (shell-command-on-region (point-min) (enscript-print/point-max)
                             (enscript-print-command-line
                              print-to-postscript-file
                              print-to-pdf-file))))

;;;###autoload
(defun enscript-print-region (&optional arg)
  "Print the contents of the region using enscript.

Supply a negative optional argument -1 to ARG to output a
PostScript file.  (M-- M-x ...)

Supply negative argument -2 to output a PDF file.  (M-- M-2 M-x ...)

Supply negative argument -3 to output PostScript and PDF files."
  (interactive "p")
  (let ((coding-system-for-read enscript-print-coding-system-for-read)
        (coding-system-for-write enscript-print-coding-system-for-write)
        (print-to-postscript-file (and (numberp arg) (< arg 0))))
    (shell-command-on-region (point) (mark)
                             (enscript-print-command-line
                              print-to-postscript-file))))

(provide 'enscript-print)
;;; enscript-print.el ends here
