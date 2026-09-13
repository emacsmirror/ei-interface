;;; ei-edit-buffer.el --- Interactive edit buffers for the ei interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: lisp, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generic interactive edit buffers: buffers that present a
;; collection of editable lines inside a recursive edit, so the
;; user can edit many entries of a container at once with normal
;; editing commands.
;;
;; `ei-edit-buffer' opens a buffer, inserts a title and
;; instructions as comment lines, calls a populate function for
;; the editable content and runs `recursive-edit'.  C-c C-c
;; finishes the edit and leaves the recursive edit; C-c C-k (or
;; C-g) aborts it with the `quit' signal, which the caller may
;; catch.  The window configuration is restored afterwards by
;; `ei-with-restored-windows'.
;;
;; Parsing helpers read the content lines back: all lines that
;; are neither empty nor comment lines.  `ei-edit-buffer-fields'
;; splits them at a separator for KEY<TAB>VALUE styles, and
;; `ei-escape-string'/`ei-unescape-string' make one-line strings
;; out of values containing newlines or tabs.

;;; Code:

(require 'cl-lib)
(require 'ei-generics)

(define-derived-mode ei-edit-buffer-mode text-mode "EI-Edit"
  "Major mode for interactive edit buffers of `ei-interface'.
\\{ei-edit-buffer-mode-map}"
  (setq-local comment-start "# ")
  (setq-local comment-use-syntax t))

(keymap-set ei-edit-buffer-mode-map "C-c C-c" #'ei-edit-buffer-finish)
(keymap-set ei-edit-buffer-mode-map "C-c C-k" #'ei-edit-buffer-cancel)

(defun ei-edit-buffer-finish ()
  "Finish editing the current edit buffer.
Leave the recursive edit so `ei-edit-buffer' returns."
  (interactive)
  (exit-recursive-edit))

(defun ei-edit-buffer-cancel ()
  "Cancel editing the current edit buffer.
Abort the recursive edit; `ei-edit-buffer' signals `quit'."
  (interactive)
  (abort-recursive-edit))

(cl-defun ei-edit-buffer (buffer-name title instructions
                                      &key populate finish-help
                                      (mode 'ei-edit-buffer-mode))
  "Open an edit buffer named BUFFER-NAME and edit it recursively.
TITLE and INSTRUCTIONS (a string or a list of strings) are
inserted as comment lines at the top.  POPULATE, when given, is
called with the buffer current to insert the editable lines.
FINISH-HELP, when given, is shown in the echo area while the
edit runs.  MODE is the major mode to install; it defaults to
`ei-edit-buffer-mode'.

The user edits the buffer with normal commands; C-c C-c finishes
and C-c C-k or C-g aborts.  Return the buffer on finish; on
abort, signal `quit' (as thrown by `abort-recursive-edit').  The
window configuration is restored by `ei-with-restored-windows',
but the buffer stays current afterwards, so callers can parse it
with `ei-edit-buffer-lines' directly."
  (ei-with-restored-windows
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (funcall mode)
          (goto-char (point-max))
          (insert (format "# %s\n" title))
          (dolist (line (if (stringp instructions)
                            (split-string instructions "\n" t)
                          instructions))
            (insert (format "# %s\n" line)))
          (insert "\n")
          (when populate
            (funcall populate))))
      (pop-to-buffer buf)
      (when finish-help
        (message "%s" finish-help))
      (recursive-edit)
      buf)))

(defun ei-edit-buffer-lines (&optional buffer)
  "Return the content lines of BUFFER, default the current buffer.
Content lines are all lines that are neither empty nor comment
lines (starting with `#' or `;')."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (not (eobp))
               for line = (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))
               unless (or (string-empty-p line)
                          (string-prefix-p "#" line)
                          (string-prefix-p ";" line))
               collect line
               do (forward-line 1)))))

(defun ei-edit-buffer-fields (separator &optional buffer)
  "Return the fields of the content lines of BUFFER as alists.
Each content line is split at the first occurrence of SEPARATOR
\(a string); the result is a list of (KEY . VALUE) conses of the
trimmed parts.  A line without the separator becomes (LINE)."
  (mapcar (lambda (line)
            (if (string-match (regexp-quote separator) line)
                (let ((start (match-beginning 0)))
                  (cons (string-trim (substring line 0 start))
                        (string-trim
                         (substring line (+ start (length separator))))))
              (list line)))
          (ei-edit-buffer-lines buffer)))

(defun ei-edit-buffer-put-field (key value &optional separator)
  "Insert a field line KEY SEPARATOR VALUE at point.
SEPARATOR defaults to a tab."
  (insert key (or separator "\t") value "\n"))

(defun ei-escape-string (string)
  "Return STRING with newlines, tabs, returns and backslashes escaped.
A value prepared this way fits on one line of an edit buffer and
is read back by `ei-unescape-string'."
  (mapconcat (lambda (ch)
               (pcase ch
                 (?\n "\\n") (?\r "\\r") (?\t "\\t") (?\\ "\\\\")
                 (_ (char-to-string ch))))
             string ""))

(defun ei-unescape-string (string)
  "Return STRING with the escapes of `ei-escape-string' undone."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\\\\[nrt\\\\]" nil t)
      (replace-match (pcase (match-string 0)
                       ("\\n" "\n") ("\\r" "\r")
                       ("\\t" "\t") ("\\\\" "\\"))
                     t t))
    (buffer-string)))

(provide 'ei-edit-buffer)

;;; ei-edit-buffer.el ends here
