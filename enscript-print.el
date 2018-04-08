;;; enscript-print.el --- Printing with GNU Enscript  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Darren Embry

;; Author: Darren Embry <dse@rectangle>
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

;; Nothing necessary at the moment.  :-)

;;; Code:

(defgroup enscript-print nil
  "Printing with GNU Enscript"
  :group 'printing)

(defcustom enscript-print-header-p t
  "Print using the enscript header."
  :group 'enscript-print
  :tag "Print The Header?"
  :type '(boolean))

(defcustom enscript-print-font-name nil
  "The PostScript font name to use when printing.

If value is nil, either \"Courier\" or whatever is specified in
~/.enscriptrc is used."
  :group 'enscript-print
  :tag "Font Name"
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (string :tags "Use Courier" "Courier")
                 (string :tags "Custom Font")))

(defcustom enscript-print-font-size nil
  "The PostScript font size (in units of 1/72 inch) to use when printing.

If value is nil, either 10 points or whatever is specified in
~/.enscriptrc is used.

If landscape multi-column printing is done, the default font size
changes from 10 points to 7 points.

If a value for `enscript-print-font-height' is specified, this
specifies the font's width and `enscript-print-font-height'
specifies the font's height.  Otherwise, this specifies both the
font's width and height."
  :group 'enscript-print
  :tag "Font Size"
  :type '(choice (const :tags "Use Enscript's default" nil)
                 (number :tags "10pt" 10)
                 (number :tags "7pt" 7)
                 (number :tags "Custom font size in points")))

(defcustom enscript-print-font-height nil
  "The PostScript font height (in units of 1/72 inch) to use when printing.

If the value is nil, maintain the font's original aspect ratio."
  :group 'enscript-print
  :tag "Font Height"
  :type '(choice (const :tags "Maintain font's original aspect ratio" nil)
                 (number :tags "Custom font height in points")))

(defcustom enscript-print-landscape-p nil
  "Whether to print in landscape orientation."
  :group 'enscript-print
  :tag "Landscape?"
  :type '(boolean))

(defcustom enscript-print-columns 1
  "Number of columns to print."
  :group 'enscript-print
  :tag "Number of Columns"
  :type '(integer))

(defcustom enscript-print-highlight-bars nil
  "Use highlight bars."
  :group 'enscript-print
  :tag "Highlight Bars?"
  :type '(choice (const :tags "Off" nil)
                 (const :tags "On" t)
                 (integer :tags "Height of highlight bars in lines")))

(defcustom enscript-print-highlight-bar-gray-level 0.1
  "Highlight bar gray level (0 to 1)."
  :group 'enscript-print
  :tag "Highlight Bar Gray Level"
  :type '(number))

(defun enscript-print-shell-concat (command-line &rest arguments)
  "Take COMMAND-LINE; append ARGUMENTS; return new command line."
  (let ((command-line command-line))
    (dolist (argument arguments command-line)
      (if argument
          (setq command-line (concat command-line " " (shell-quote-argument argument)))))))

(defun enscript-print-command-line ()
  "Return the command line for enscript printing."
  (let ((command-line "enscript"))
    (if (not enscript-print-header-p)
        (setq command-line (enscript-print-shell-concat
                            command-line
                            "--no-header")))
    (if (or enscript-print-font-name enscript-print-font-size)
        (setq command-line (enscript-print-shell-concat
                            command-line
                            (concat "--font="
                                    (or enscript-print-font-name "Courier")
                                    (if enscript-print-font-size
                                        (concat "@" (number-to-string enscript-print-font-size)
                                                (if enscript-print-font-height
                                                    (concat "/" (number-to-string enscript-print-font-height)))))))))
    (if enscript-print-landscape-p
        (setq command-line (enscript-print-shell-concat
                            command-line
                            "--landscape")))
    (if enscript-print-columns
        (setq command-line (enscript-print-shell-concat
                            command-line
                            (concat "--columns=" (number-to-string enscript-print-columns)))))
    (if enscript-print-highlight-bars
        (if (numberp enscript-print-highlight-bars)
            (setq command-line (enscript-print-shell-concat
                                command-line
                                (format "--highlight-bars=%d" enscript-print-highlight-bars)
                                (format "--highlight-bar-gray=%f" enscript-print-highlight-bar-gray-level)))
          (setq command-line (enscript-print-shell-concat
                              command-line
                              "--highlight-bars"
                              (format "--highlight-bar-gray=%f" enscript-print-highlight-bar-gray-level)))))

    command-line))

;;;###autoload
(defun enscript-print-buffer ()
  "Print the contents of the buffer using enscript."
  (interactive)
  (shell-command-on-region (point-min) (point-max) (enscript-print-command-line)))

;;;###autoload
(defun enscript-print-region ()
  "Print the contents of the region using enscript."
  (interactive)
  (shell-command-on-region (point) (mark) (enscript-print-command-line)))

(provide 'enscript-print)
;;; enscript-print.el ends here
