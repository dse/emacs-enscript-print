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

(defcustom enscript-print-footer-p t
  "Print using the enscript footer."
  :group 'enscript-print
  :tag "Print The Footer?"
  :type '(boolean))

(defcustom enscript-print-font-name "Courier"
  "The PostScript font name to use when printing."
  :group 'enscript-print
  :tag "Font Name"
  :type '(string))

(defcustom enscript-print-font-size 10
  "The PostScript font size (in units of 1/72 inch) to use when printing."
  :group 'enscript-print
  :tag "Font Size"
  :type '(integer))

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

(defun enscript-print-shell-concat (command-line &rest arguments)
  "Take COMMAND-LINE; append ARGUMENTS; return new command line."
  (let ((command-line command-line))
    (dolist (argument arguments command-line)
      (setq command-line (concat command-line " " (shell-quote-argument argument))))))

(defun enscript-print-command-line ()
  "Return the command line for enscript printing."
  (let ((command-line "enscript"))
    (if (not enscript-print-header-p)
        (setq command-line (enscript-print-shell-concat
                            command-line "--no-header")))
    (if (not enscript-print-footer-p)
        (setq command-line (enscript-print-shell-concat
                            command-line "--no-footer")))
    (setq command-line (enscript-print-shell-concat
                        command-line (concat "--font="
                                             enscript-print-font-name
                                             enscript-print-font-size)))
    (if enscript-print-landscape-p
        (setq command-line (enscript-print-shell-concat
                            command-line "--landscape")))
    (if enscript-print-columns
        (setq command-line (enscript-print-shell-concat
                            command-line (concat "--columns="
                                                 enscript-print-columns))))
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
