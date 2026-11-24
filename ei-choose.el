;;; ei-choose.el --- Completion engine of the ei choice interface  -*- lexical-binding: t; -*-

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

;; This file contains the choice interface used by `ei-interface':
;; the actions menu, object/candidate info and annotation methods
;; for builtin types, and the completion read machinery.
;;
;; The interface reads one entry of a container through the
;; minibuffer, with these keys active (their stock bindings in
;; `ei-choose-minibuffer-map'; a keyboard-layout system such as
;; Daselt adds its own bindings to that map, and the prompt hint
;; and the action menu then advertise those instead, see
;; `ei-choose-prompt-hint'):
;;
;; - M-a (also M-\\) opens the action menu, a help screen built
;;   with `make-help-screen' from the entries of `ei-actions'.
;; - M-h describes the entry named in the minibuffer through
;;   `ei-candidate-info', including slot documentation.
;; - M-, goes back to the previous container of the descent, see
;;   `ei-choose-back'; the parent read resumes with its previous
;;   input.
;;
;; Candidates are annotated through `ei-candidate-annotation' and
;; every prompt carries `ei-choose-prompt-hint'.  The actual
;; completing read goes through `ei-completing-read-function', so
;; that a caller can supply a history-aware reader.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'help-macro)

(defgroup ei nil
  "Completion-based interface for EIEIO objects and other containers."
  :group 'extensions)

(declare-function ei-choose "ei-generics")
(declare-function ei-add "ei-generics")
(declare-function ei-delete "ei-generics")
(declare-function ei-edit "ei-generics")

(defvar ei-choose-minibuffer-history nil
  "Minibuffer history of `ei-choose--default-completing-read'.")

(defvar ei-current-choice-history nil
  "History list of the choice read in progress.
This is the value threaded through the reads of a choice run as
their `:history-list': what it holds is up to the reader in
`ei-completing-read-function', which may keep it current, for
instance as the list of the choices made so far.  The actions of
the menu pass it on as the `:history-list' of the operation they
start.  The default reader leaves it alone.")

(defvar ei-current-object nil
  "The object currently being chosen in.")

(defvar ei-current-path nil
  "Choice path leading to `ei-current-object'.
Bound while a completion of the choice interface is active.  The
value is an alist of (CONTAINER . KEY) steps, innermost first:
each element records one container that was chosen into and the
entry that was selected in it.  Context-dependent actions read
this to find, for example, the object owning the container
currently chosen in.")

(defvar ei-choose--tag-stack nil
  "Catch tags of the active choice reads, innermost first.
`ei-read' pushes one tag per active read level;
`ei-choose-back' throws to the second entry, the tag of the
parent read, to resume the descent one level up.")

(defvar ei-edit--store-back nil
  "Set by `ei-edit', `ei-add' and `ei-delete' methods to hand a new
value to their caller.

Methods on in-place containers (EIEIO objects, obarrays, hash
tables, symbols) write their changes themselves and leave this
alone.  Methods on value containers (lists) compute a fresh list,
store it in this variable and return it; a caller that holds the
edited list in a place of its own rebinds this variable to a
non-list marker around the recursive edit, checks it afterwards
and installs the list found there, see `ei-edit--value'.  The
marker rather than nil stands for \"nothing handed over\", because
nil is the empty list, a legitimate new value.  This protocol
keeps every write at the level that owns the edited place.")

(defvar ei-choose-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-a") #'ei-choose-actions)
    (define-key map (kbd "M-\\") #'ei-choose-actions)
    (define-key map (kbd "M-h") #'ei-choose-describe-candidate)
    (define-key map (kbd "M-,") #'ei-choose-back)
    map)
  "Keymap active in the minibuffer of every choice completion.

M-a opens the action menu of the object currently chosen in (a
help screen built with `make-help-screen' listing the
context-dependent actions of `ei-actions'); M-h describes the
entry currently named in the minibuffer via `ei-candidate-info';
M-, goes back to the previous container of the descent via
`ei-choose-back'.  Candidates are annotated with type information
through `ei-candidate-annotation'.

These are the stock bindings; a keyboard-layout system such as
Daselt adds its own bindings to this map (through its bindlists),
and the prompt hint of `ei-choose-prompt-hint' and the action
menu then advertise those instead.  The map is composed over
whatever local map the minibuffer read installs, so bindings in
it are active during every choice completion without being
affected by the minibuffer maps themselves.")

(defun ei-choose-back ()
  "Go back to the previous container of the choice descent.
Throw to the read loop of the parent container, which resumes
with its previous input, so one level of the descent is undone.
At the top level of a descent there is no parent; then ring the
bell and stay in the current read."
  (interactive)
  (if (cadr ei-choose--tag-stack)
      (throw (cadr ei-choose--tag-stack) t)
    (ding)))

(defcustom ei-completing-read-function nil
  "Function reading one entry for the choice interface.
When nil, `ei-choose--default-completing-read' is used.  The
function is called with (PROMPT TABLE &key history-list
require-match initial-input) and must return a cons (CHOICE .
NEW-HISTORY): CHOICE is the chosen entry as a string and
NEW-HISTORY the history to thread to the next read.  Bind this to
a history-aware reader to give the interface a persistent
per-decision-point history."
  :type '(choice (const :tag "Default reader" nil)
                 (function :tag "Custom reader"))
  :group 'ei)

(cl-defun ei-choose--default-completing-read (prompt table
                                                    &key history-list
                                                    require-match
                                                    initial-input)
  "Read one entry of TABLE through `completing-read'.
Return (CHOICE . HISTORY-LIST) with the chosen entry as a
string."
  (cons (substring-no-properties
         (completing-read prompt table nil require-match
                          nil 'ei-choose-minibuffer-history
                          initial-input))
        history-list))

(defvar ei-choose--actions-keymap nil
  "Keymap of the action menu built by `ei-choose-actions'.
Dynamically rebound from `ei-actions' every time the menu opens;
consumed by `ei-choose--action-screen'.")

(defvar ei-choose--help-text nil
  "Help text of the action menu built by `ei-choose-actions'.
Dynamically rebound from `ei-actions' every time the menu opens;
consumed by `ei-choose--action-screen'.")

(make-help-screen ei-choose--action-screen
  "Action: (SPC/DEL to scroll, q to quit)"
  ei-choose--help-text
  ei-choose--actions-keymap
  "*ei-choose-actions*")

(defun ei-choose-actions ()
  "Open the action menu of the object currently chosen in.
The menu is a help screen in the style of `make-help-screen' that
lists all actions with their keys right away: press one of the
action keys to run that action, scroll the list with SPC and
DEL, or press q to return to the completion.  The actions are
the context-dependent entries of `ei-actions' for
`ei-current-object'."
  (interactive)
  (when ei-current-object
    (let* ((obj ei-current-object)
           (path ei-current-path)
           (menu (ei-choose--actions-menu (ei-actions obj) obj path)))
      (setq ei-choose--actions-keymap (car menu)
            ei-choose--help-text (cdr menu))
      ;; A help screen only lists its commands once the help key is
      ;; pressed; press it for the user, so the actions are visible
      ;; from the start.
      (push ?? unread-command-events)
      (ei-choose--action-screen))))

(defun ei-choose--actions-menu (actions object path)
  "Return a cons (KEYMAP . HELP-TEXT) for ACTIONS on OBJECT.
ACTIONS is a list of (KEY LABEL FUNCTION) entries as returned by
`ei-actions'; PATH is the choice path leading to OBJECT.  Keys
already taken are reassigned to the next free character; q and
C-g always quit the menu and resume the completion.  An action
ends the choice and its result becomes the result of the read,
except for entries marked `:resume', which run inside the
minibuffer and leave the completion active."
  (let* ((map (make-sparse-keymap))
         (taken (make-hash-table))
         (rows nil))
    (puthash ?q t taken)
    (puthash ?\C-g t taken)
    (dolist (action actions)
      (pcase-let ((`(,key ,label ,fn ,resume) action))
        (when (and (characterp key) (functionp fn))
          (let ((ch key))
            (while (gethash ch taken) (setq ch (1+ ch)))
            (puthash ch t taken)
            (define-key map (vector ch)
                        (if (eq resume :resume)
                            (lambda ()
                              (interactive)
                              (funcall fn object))
                          (lambda ()
                            (interactive)
                            (throw 'ei-choose-action
                                   (list :action fn object path)))))
            (push (cons ch label) rows)))))
    (dolist (ch '(?q ?\C-g))
      (define-key map (vector ch)
                  (lambda ()
                      (interactive)
                      (throw 'ei-choose--action-abort
                           (minibuffer-contents)))))
    (cons map (ei-choose--format-help-text (nreverse rows) object path))))

(defun ei-choose--format-help-text (rows object path)
  "Format the help screen listing ROWS, the actions on OBJECT.
PATH is the choice path leading to OBJECT; it is shown so that
the user can see the context the actions apply to."
  (let ((actions-key (ei-choose--command-key 'ei-choose-actions))
        (info-key (ei-choose--command-key 'ei-choose-describe-candidate))
        (back-key (ei-choose--command-key 'ei-choose-back)))
    (with-temp-buffer
      (insert "ei choice interface\n\n")
      (insert (format "RET descends into the entry chosen from the current
object; %s opens this action menu; %s describes the entry
currently named in the minibuffer; %s goes back to the previous
container; C-g aborts the choice.\n\n"
                      (or actions-key "the actions key")
                      (or info-key "the info key")
                      (or back-key "the back key")))
      (insert (format "Current object: %s\n"
                     (ei-choose--object-summary object)))
      (when path
      (insert (format "Reached through: %s\n"
                      (mapconcat
                       (lambda (step)
                           (concat (ei-choose--object-summary (car step))
                                 (if (cdr step)
                                         (format " [%s]" (cdr step))
                                     "")))
                       path " <- "))))
      (insert "\nActions:\n")
      (if rows
            (dolist (row rows)
          (insert (format "  %-4s %s\n"
                          (key-description (vector (car row)))
                          (cdr row))))
      (insert "  (none)\n"))
      (insert "\nq or C-g quits this menu and resumes completion.\n")
      (buffer-string))))

(defun ei-choose--object-summary (object)
  "Return a short one-line description of OBJECT."
  (cond ((eieio-object-p object)
         (format "%s" (eieio-object-class object)))
        ((obarrayp object) "obarray")
        ((hash-table-p object)
         (format "hash-table (%d keys)" (hash-table-count object)))
        ((stringp object)
         (format "string %s" (ei-choose--short object 32)))
        ((null object) "nil")
        ((symbolp object) (format "symbol %s" object))
        (t (format "%s" (type-of object)))))

(defun ei-choose-describe-candidate ()
  "Describe the entry currently named in the minibuffer.
Display the type, value and documentation of the entry named by
the current minibuffer contents, as computed by
`ei-candidate-info' for `ei-current-object'.  Use this to read
the documentation of a slot while choosing one."
  (interactive)
  (let* ((object ei-current-object)
         (candidate (minibuffer-contents)))
    (if object
        (let ((info (ei-candidate-info object candidate)))
          (with-help-window "*ei-candidate*"
            (princ (or info
                       (format "No information about %s in %s"
                               candidate
                               (ei-choose--object-summary object))))))
      (message "ei: no choose object active"))))

(defun ei-choose--inspect-object (&optional object)
  "Describe OBJECT in a help buffer; return OBJECT.
OBJECT defaults to `ei-current-object', so that this works both
as an action of the choice interface and as a command during a
choice.  The description is inserted rather than printed, so that
text properties --- such as the `table-mode' tables of
`ei-object-info' --- survive."
  (interactive)
  (let ((obj (or object ei-current-object)))
    (when obj
      (with-help-window "*ei-object*"
        (with-current-buffer standard-output
          (insert (ei-object-info obj))))
      obj)))

(defcustom ei-choose-table-max-cell-width 40
  "Maximum width of one cell of a table of the choice interface.
Cell contents are cut to this width by `ei-choose--short' before
`table-insert' lays out the table, so that wide objects keep the
tables of `ei-object-info' readable."
  :type 'natnum
  :group 'ei)

(defcustom ei-choose-prompt-hint t
  "Hint about the choice-interface keys appended to every prompt.
With t (the default) the hint is computed from the keys currently
bound in `ei-choose-minibuffer-map', preferring bindings other
than the stock ones, so that a keyboard-layout system such as
Daselt that adds its own bindings to the map displays the keys
that are actually used.  A string is used verbatim; bind it to nil
or the empty string to keep the prompts plain."
  :type '(choice (const :tag "Automatic from the key bindings" t)
                 (string :tag "Hint text")
                 (const :tag "No hint" nil))
  :group 'ei)

(defvar ei-choose--commands-and-stock-keys
  '((ei-choose-actions "actions" ("M-a" "M-\\"))
    (ei-choose-describe-candidate "info" ("M-h"))
    (ei-choose-back "back" ("M-,")))
  "Commands of `ei-choose-minibuffer-map' with their stock keys.
Each entry is (COMMAND LABEL STOCK-KEYS): LABEL names COMMAND in
the prompt hint and STOCK-KEYS lists the key descriptions of the
stock bindings in `ei-choose-minibuffer-map', which
`ei-choose--command-key' demotes in favor of any other binding.")

(defun ei-choose--command-key (command)
  "Return the key description advertising COMMAND in the hint.
All keys bound to COMMAND in `ei-choose-minibuffer-map' are
considered; the first one that is not among the stock keys of
`ei-choose--commands-and-stock-keys' wins, so bindings added by a
keyboard-layout system such as Daselt are preferred over the stock
ones.  With only stock keys bound, the first stock key that is
bound wins.  Return nil when COMMAND has no binding."
  (let* ((stock (nth 2 (assq command
                             ei-choose--commands-and-stock-keys)))
         (bound (mapcar #'key-description
                        (where-is-internal command
                                           ei-choose-minibuffer-map)))
         (key (or (cl-find-if (lambda (d) (not (member d stock)))
                              bound)
                  (cl-find-if (lambda (d) (member d bound))
                              stock))))
    key))

(defun ei-choose--default-prompt-hint ()
  "Return the hint naming the keys bound in `ei-choose-minibuffer-map'.
Every entry of `ei-choose--commands-and-stock-keys' whose command
has a binding contributes a \"KEY label\" item.  Return nil when
none of the commands is bound."
  (let ((items
         (delq nil
               (mapcar
                (pcase-lambda (`(,cmd ,label ,_stock))
                  (let ((key (ei-choose--command-key cmd)))
                    (and key (format "%s %s" key label))))
                ei-choose--commands-and-stock-keys))))
    (and items (concat "[" (mapconcat #'identity items ", ") "] "))))

(defun ei-choose--path-label (key)
  "Return a short name for KEY, the entry chosen in one path step."
  (cond ((and (eieio-object-p key)
              (slot-exists-p key 'name)
              (slot-boundp key 'name)
              (slot-value key 'name))
         (ei-choose--short (slot-value key 'name) 20))
        ((eieio-object-p key) (format "%s" (eieio-object-class key)))
        (t (ei-choose--short key 20))))

(defun ei-choose--breadcrumb (path)
  "Return a prompt part naming the entries chosen along PATH.
PATH is a choice path as in `ei-current-path'.  The entries are
named outermost first, separated by \" › \" and enclosed in
parentheses; of a long path only the three innermost steps are
named, behind an ellipsis.  Return nil for an empty PATH."
  (when path
    (let ((labels (nreverse (mapcar (lambda (step)
                                      (ei-choose--path-label (cdr step)))
                                    (take 3 path)))))
      (concat "("
              (and (nthcdr 3 path) "… › ")
              (mapconcat #'identity labels " › ")
              ") "))))

(defun ei-choose--prompt (prompt &optional path)
  "Return PROMPT followed by the breadcrumb of PATH and the key hint.
The breadcrumb of `ei-choose--breadcrumb' for the choice path
PATH follows PROMPT, so that every prompt of a descent shows
where it is while still starting with its own text.
`ei-choose-prompt-hint' comes last when set; a value of t
computes the hint from the keys bound in
`ei-choose-minibuffer-map'.  What is appended is separated from
PROMPT by a space unless PROMPT ends with one already."
  (let* ((hint (cond
                ((or (null ei-choose-prompt-hint)
                     (string-empty-p ei-choose-prompt-hint))
                 nil)
                ((eq ei-choose-prompt-hint t)
                 (ei-choose--default-prompt-hint))
                (t ei-choose-prompt-hint)))
         (suffix (concat (ei-choose--breadcrumb path) hint)))
    (cond
     ((string-empty-p suffix) prompt)
     ((string-match-p "[ \t]$" prompt) (concat prompt suffix))
     (t (concat prompt " " suffix)))))

(defun ei-choose--insert-table (rows)
  "Insert ROWS as an editable `table-mode' table at point.
ROWS is a list of rows, each row a list of cell strings; the
first row is the header row and is shown with the bold face.
Cell contents are cut to `ei-choose-table-max-cell-width' by
`ei-choose--short' and padded with one blank on each side, and
every column is as wide as its widest cell.  `table-insert'
draws the skeleton and the cells are filled in place, so the
result is a live table.  Point is left behind the table's
bottom border."
  (require 'table)
  (when rows
    (let* ((limit ei-choose-table-max-cell-width)
           (cells (mapcar (lambda (row)
                            (mapcar (lambda (cell)
                                      (concat " "
                                              (ei-choose--short cell limit)
                                              " "))
                                    row))
                          rows))
           (widths (apply #'cl-mapcar
                          (lambda (&rest column)
                            (apply #'max (mapcar #'length column)))
                          cells))
           (ncols (length widths))
           (nrows (length cells))
           ;; Width of one line of the table: a border character
           ;; before, after and between every cell.
           (line-width (+ ncols 1 (apply #'+ widths)))
           ;; `table-insert' starts drawing at the beginning of the
           ;; line point is on; make sure that line is empty.
           (top (progn (unless (and (bolp)
                                    (save-excursion
                                      (beginning-of-line)
                                      (looking-at "\\s *$")))
                         (insert "\n"))
                       (point))))
      (table-insert ncols nrows widths 1)
      (cl-loop
       for row in cells
       for r from 1                 ; content rows sit between borders
       for offset = (+ top (* (1- (* 2 r)) (1+ line-width)))
       for header? = t then nil
       do (cl-loop
           for cell in row
           for width in widths
           for j from 1             ; column of the first cell character
           for column = (+ j (cl-loop for w in widths
                                      repeat (1- j)
                                      sum w))
           for start = (+ offset column)
           do (goto-char start)
           (insert (if header?
                       (propertize cell 'font-lock-face 'bold)
                     cell))
           ;; The cell was created with `width' blanks and the cell
           ;; text pushed them right; keep only the blanks that pad
           ;; the text up to the cell width.
           (delete-region (+ start width)
                          (+ start width (length cell)))))
      ;; Point behind the bottom border, at the start of the line
      ;; that follows the table.
      (goto-char (+ top (* (1+ (* 2 nrows)) (1+ line-width)))))))


(cl-defgeneric ei-object-info (object)
  "Return a multi-line description of OBJECT.
Used by the Inspect action of the choice interface,
`ei-choose--inspect-object'.  The description may carry text
properties; lists of entries are rendered as `table-mode' tables
through `ei-choose--insert-table'."
  (format "%s\n\n%s"
          (ei-choose--object-summary object)
          (ei-choose--short object 400)))

(cl-defmethod ei-object-info ((object eieio-default-superclass))
  "Describe OBJECT's class and the type and value of each of its slots.
The slots are listed in a `table-mode' table whose columns are the
slot name, the slot type and the current value, see
`ei-choose--insert-table'."
  (with-temp-buffer
    (insert (format "%s object\n\n" (eieio-object-class object)))
    (ei-choose--insert-table
     (cons '("Slot" "Type" "Value")
           (mapcar
            (lambda (entry)
              (let* ((slot (cdr entry))
                     (name (oref slot name))
                     (type (ignore-errors (oref slot type)))
                     (bound (slot-boundp object name)))
                (list (symbol-name name)
                      (format "%s" (or type t))
                      (if bound
                          (format "%s" (slot-value object name))
                        "#<unbound>"))))
            (ei-choose--class-slots object))))
    (insert "\n")
    (buffer-string)))

(cl-defmethod ei-object-info ((array obarray))
    "Describe the number of symbols in ARRAY and a sample of them."
    (let ((count 0)
        (sample nil))
    (mapatoms (lambda (sym)
                  (setq count (1+ count))
                  (when (and (null sample) (boundp sym))
                  (setq sample (symbol-name sym))))
              array)
    (format "obarray with %d symbol%s%s"
            count (if (= count 1) "" "s")
            (if sample (format ", e.g. %s" sample) ""))))

(cl-defmethod ei-object-info ((table hash-table))
    "Describe the number of entries of TABLE and its test function."
    (format "hash table with %d entries, test `%s'"
          (hash-table-count table) (hash-table-test table)))

(cl-defmethod ei-object-info ((lst list))
    "Describe the length of LST and its printed form."
    (format "list with %d element%s:\n%s"
          (length lst) (if (= (length lst) 1) "" "s")
          (ei-choose--short lst 400)))

(cl-defgeneric ei-candidate-annotation (_container _candidate)
  "Return an annotation string for CANDIDATE in CONTAINER, or nil.
CANDIDATE is the completion string naming one entry of CONTAINER.
The result is displayed in the *Completions* buffer next to the
candidate; it should be short and single-line, and it typically
shows the type or value of the candidate.  Specialize this
generic to teach the choice interface about new container types."
  nil)

(cl-defmethod ei-candidate-annotation
    ((object eieio-default-superclass) candidate)
    "Annotate CANDIDATE with its slot type and current value."
    (when-let* ((slot (ei-choose--entry object candidate)))
    (let* ((name (oref slot name))
           (type (ignore-errors (oref slot type)))
           (type-str (and type (not (eq type t)) (format "%s" type)))
           (val (and (slot-boundp object name) (slot-value object name)))
           (val-str (and val (ei-choose--short val 24)))
           (text (cond
                  ((and type-str val-str)
                   (format "  [%s] %s"
                           (ei-choose--short type-str 28) val-str))
                  (type-str (format "  [%s]"
                                    (ei-choose--short type-str 28)))
                  (val-str (format "  %s" val-str))
                  (t nil))))
      (ei-annotation-face text))))

(cl-defmethod ei-candidate-annotation ((array obarray) candidate)
    "Annotate CANDIDATE with the value bound to its symbol."
    (let ((sym (intern-soft candidate array)))
    (when sym
      (let ((text (cond
                    ((and (boundp sym) (stringp (symbol-value sym)))
                     (concat "  " (ei-choose--short (symbol-value sym) 40)))
                    ((boundp sym)
                     (format "  <%s>" (type-of (symbol-value sym))))
                    (t "  #<unbound>"))))
        (ei-annotation-face text)))))

(cl-defmethod ei-candidate-annotation ((table hash-table) candidate)
    "Annotate CANDIDATE with the value stored for its key."
    (let ((key (ei-choose--entry table candidate)))
    (when key
      (ei-annotation-face
       (concat "  " (ei-choose--short (gethash key table) 32))))))

(cl-defmethod ei-candidate-annotation ((lst list) candidate)
    "Annotate CANDIDATE with a short form of the named element."
    (let ((elt (ei-choose--entry lst candidate)))
    (when elt
      (ei-annotation-face
       (concat "  " (ei-choose--short elt 32))))))

(cl-defmethod ei-candidate-annotation ((sym symbol) candidate)
    "Annotate CANDIDATE with the value of the named property."
    (let ((prop (ei-choose--entry sym candidate)))
    (when prop
      (let ((val (if (eq prop 'value)
                          (and (boundp sym) (symbol-value sym))
                      (plist-get (symbol-plist sym) prop))))
        (ei-annotation-face
         (concat "  " (ei-choose--short val 32)))))))

(cl-defgeneric ei-candidate-info (container candidate)
  "Return a documentation string for CANDIDATE in CONTAINER.
CANDIDATE is the completion string naming one entry of CONTAINER.
The result is displayed by `ei-choose-describe-candidate', bound
to M-h during the choice interface, and typically includes the
entry's type, its current value and, for object slots, the slot
documentation."
  (format "%s is an entry of %s."
          candidate (ei-choose--object-summary container)))

(cl-defmethod ei-candidate-info
    ((object eieio-default-superclass) candidate)
    "Document the slot of OBJECT named by CANDIDATE."
    (if-let* ((slot (ei-choose--entry object candidate)))
          (let* ((name (oref slot name))
              (type (ignore-errors (oref slot type)))
              (doc (ignore-errors (alist-get :documentation
                                              (oref slot props))))
              (initform (ignore-errors (oref slot initform)))
              (bound (slot-boundp object name))
              (val (and bound (slot-value object name))))
         (with-temp-buffer
             (insert (format "%s is a slot of %s objects.\n\n"
                           name (eieio-object-class object)))
             (insert (format "Type:    %s\n"
                           (if (eq type t) "unspecified"
                             (ei-choose--short type 56))))
             (insert (format "Default: %s\n" (ei-choose--short initform 56)))
             (insert (format "Value:   %s\n"
                           (if bound (ei-choose--short val 56)
                             "#<unbound>")))
             (insert (format "\n%s\n"
                           (or doc
                               "This slot has no documentation.")))
             (insert "\nEdit this slot with `ei-edit'; see the action menu (M-a).\n")
             (buffer-string)))
    (cl-call-next-method)))

(cl-defmethod ei-candidate-info ((array obarray) candidate)
    "Document the symbol of ARRAY named by CANDIDATE."
    (let ((sym (intern-soft candidate array)))
    (if (not sym)
            (cl-call-next-method)
        (with-temp-buffer
          (insert (format "%s is a symbol of this obarray.\n\n" candidate))
          (insert (format "Value:         %s\n"
                         (if (boundp sym)
                                 (ei-choose--short (symbol-value sym) 56)
                             "#<unbound>")))
          (let ((props (symbol-plist sym)))
          (if props
                  (progn
                  (insert "\nProperties:\n")
                  (cl-loop for (k v) on props by #'cddr
                         do (insert (format "  %s: %s\n"
                                            (ei-choose--short k 24)
                                            (ei-choose--short v 44)))))
            (insert "\nThis symbol has no properties.\n")))
          (buffer-string)))))

(cl-defmethod ei-candidate-info ((table hash-table) candidate)
    "Document the key of TABLE named by CANDIDATE."
    (let ((key (ei-choose--entry table candidate)))
    (if (not key)
            (cl-call-next-method)
        (format "%s is a key of this hash table.\n\nValue: %s"
              candidate (ei-choose--short (gethash key table) 56)))))

(cl-defmethod ei-candidate-info ((lst list) candidate)
    "Document the element of LST named by CANDIDATE."
    (let ((elt (ei-choose--entry lst candidate)))
    (if (not elt)
            (cl-call-next-method)
        (format "%s is an element of a list of %d elements.\n\nValue: %s"
              candidate (length lst) (ei-choose--short elt 56)))))

(cl-defmethod ei-candidate-info ((sym symbol) candidate)
  "Document the property of SYM named by CANDIDATE."
  (let ((prop (ei-choose--entry sym candidate)))
    (if (not prop)
        (cl-call-next-method)
      (format "%s is %s of the symbol %s.\n\nValue: %s"
              candidate
              (if (eq prop 'value) "the value" "a property")
              sym
              (ei-choose--short
               (if (eq prop 'value)
                   (and (boundp sym) (symbol-value sym))
                 (plist-get (symbol-plist sym) prop))
               56)))))


(cl-defgeneric ei-actions (_object)
  "Return the actions available for OBJECT in the choice interface.
The return value is a list of entries (KEY LABEL FUNCTION): KEY
is the character selecting the action in the menu opened by
`ei-choose-actions', LABEL is a short description shown in the
menu's help screen, and FUNCTION is called with OBJECT when the
action runs.  Running an action ends the choice.  An entry of
the form (KEY LABEL FUNCTION :resume) instead runs FUNCTION while
the minibuffer stays active, so the choice continues afterwards;
use it for actions that only display something and neither
change OBJECT nor read from the minibuffer.  While an action
runs, the dynamic variables
`ei-current-object' and `ei-current-path' describe the choice
context.  Specializations should append their own entries to
`cl-call-next-method' so that the actions of all applicable
specializations compose; context-dependent commands arrive
through `ei-extra-actions'."
  (list (list ?i "Inspect the object" #'ei-choose--inspect-object
              :resume)))

(cl-defmethod ei-actions ((object eieio-default-superclass))
  "Basic editing actions for EIEIO objects."
  (append
   (list (list ?e "Edit in place"
               (lambda (ob)
                 (ei-edit ob :history-list ei-current-choice-history)))
         (list ?a "Add to the object"
               (lambda (ob)
                 (ei-add ob :history-list ei-current-choice-history)))
         (list ?d "Delete from the object"
               (lambda (ob)
                 (ei-delete ob :history-list ei-current-choice-history))))
   (ei-extra-actions object ei-current-path)
   (cl-call-next-method object)))


(cl-defmethod ei-actions ((table hash-table))
  "Editing actions for hash tables."
  (append
   (list (list ?e "Edit a value"
               (lambda (ob)
                 (ei-edit ob :history-list ei-current-choice-history)))
         (list ?a "Add an entry"
               (lambda (ob)
                 (ei-add ob :history-list ei-current-choice-history)))
         (list ?d "Delete a key"
               (lambda (ob)
                 (ei-delete ob :history-list ei-current-choice-history))))
   (ei-extra-actions table ei-current-path)
   (cl-call-next-method table)))



(cl-defgeneric ei-extra-actions (_object _path)
  "Return context-dependent actions for OBJECT reached through PATH.
PATH is the choice path (an alist of (CONTAINER . KEY) steps,
innermost first) leading to OBJECT in the current choice run.
The return value is a list of (KEY LABEL FUNCTION) entries in the
format of `ei-actions'; FUNCTION is called with OBJECT.
Specialize this generic to bind commands to the places they apply
to: for example, a method that finds the object owning the
current container through PATH can offer an edit buffer for it.
The default method returns no extra actions."
  nil)

(cl-defmethod ei-actions ((array obarray))
      "Editing and context-dependent actions for obarrays."
      (append
   (list (list ?e "Edit an entry"
               (lambda (ob)
                     (ei-edit ob :history-list ei-current-choice-history)))
         (list ?a "Add an entry"
               (lambda (ob)
                 (ei-add ob :history-list ei-current-choice-history)))
         (list ?d "Delete an entry"
               (lambda (ob)
                 (ei-delete ob :history-list ei-current-choice-history))))
   (ei-extra-actions array ei-current-path)
   (cl-call-next-method array)))



(cl-defmethod ei-actions ((sym symbol))
              "Editing actions for symbols."
              (append
   (list (list ?e "Edit value or property"
               (lambda (ob)
                             (ei-edit ob :history-list ei-current-choice-history))))
   (cl-call-next-method sym)))

(cl-defmethod ei-actions ((str string))
  "Editing actions for strings."
  (append
   (list (list ?e "Edit the string"
               (lambda (ob)
                 (ei-edit ob :history-list ei-current-choice-history))))
   (cl-call-next-method str)))

(defun ei-choose--list-action (operation)
  "Return an action running OPERATION on the list chosen in.
OPERATION is `ei-edit', `ei-add' or `ei-delete'.  Lists are value
containers: OPERATION hands the new list over through
`ei-edit--store-back', and the action installs it in the place
the list was reached through, the innermost step of
`ei-current-path'.  That place is a slot of an EIEIO object or a
value of a hash table; a list reached any other way has no place
the action could write, and a message says so."
  (lambda (lst)
    (let* ((ei-edit--store-back 'unset)
           (result (funcall operation lst
                            :history-list ei-current-choice-history)))
      (when (listp ei-edit--store-back)
        (pcase (car ei-current-path)
          ((and `(,owner . ,slot)
                (guard (and (eieio-object-p owner)
                            (symbolp slot)
                            (slot-exists-p owner slot)
                            (slot-boundp owner slot)
                            (eq (slot-value owner slot) lst))))
           (ei-choose--set-slot-value owner slot ei-edit--store-back))
          ((and `(,owner . ,key)
                (guard (and (hash-table-p owner)
                            (eq (gethash key owner) lst))))
           (puthash key ei-edit--store-back owner))
          (_ (message "ei: the changed list is not held by a slot or \
hash-table value; nothing was stored"))))
      result)))

(cl-defmethod ei-actions ((lst list))
  "Editing actions for lists.
The actions store the changed list in the place owning it, see
`ei-choose--list-action'."
  (append
   (list (list ?e "Edit the list" (ei-choose--list-action #'ei-edit))
         (list ?a "Add an element" (ei-choose--list-action #'ei-add))
         (list ?d "Delete an element" (ei-choose--list-action #'ei-delete)))
   (ei-extra-actions lst ei-current-path)
   (cl-call-next-method lst)))


(defvar ei-choose--entry-index nil
  "Cons (CONTAINER . TABLE) indexing CONTAINER's entries, or nil.
TABLE maps the completion name of every entry of CONTAINER to
what `ei-choose--entries' pairs it with.  Bound by the function
of `ei-annotator' while it annotates, so that annotating
all candidates of a large container does not list its entries
once per candidate; consulted by `ei-choose--entry'.")

(defun ei-choose--entries (container)
  "Return the entries of CONTAINER as (NAME . THING) pairs.
NAME is the completion string of an entry; THING is the slot
descriptor for an EIEIO object, the key for a hash table, the
element for a list and the place for a symbol.  Obarrays and
atoms have no entry list; return nil for them."
  (cond ((eieio-object-p container) (ei-choose--class-slots container))
        ((hash-table-p container) (ei-choose--hash-entries container))
        ((consp container) (ei-choose--list-entries container))
        ((and container (symbolp container))
         (ei-choose--symbol-entries container))))

(defun ei-choose--entry (container candidate)
  "Return what CANDIDATE names among the entries of CONTAINER.
See `ei-choose--entries' for what that is; return nil when no
entry is named CANDIDATE.  Of several entries with the same name
the first one wins."
  (if (and ei-choose--entry-index
           (eq (car ei-choose--entry-index) container))
      (gethash candidate (cdr ei-choose--entry-index))
    (cdr (assoc candidate (ei-choose--entries container)))))

(defun ei-annotator (container)
  "Return an annotation function for the entries of CONTAINER.
The function indexes the entries of CONTAINER when it is first
called and reuses that index for every later candidate, see
`ei-choose--entry-index'."
  (let ((index nil))
    (lambda (candidate)
      (unless index
        (let ((table (make-hash-table :test #'equal)))
          (pcase-dolist (`(,name . ,thing) (ei-choose--entries container))
            (unless (gethash name table)
              (puthash name thing table)))
          (setq index (cons container table))))
      (let ((ei-choose--entry-index index))
        (ei-candidate-annotation container candidate)))))

(defun ei-choose--annotated-table (collection annotator)
  "Return a completion table delegating to COLLECTION.
The table's metadata gives the candidates the completion category
`ei-entry', for packages that attach to categories, and carries
ANNOTATOR as the annotation function, so that candidates show the
extra information computed by `ei-candidate-annotation'.  The
affixation function shows the same annotations aligned in one
column."
  (let ((affixator
         (lambda (candidates)
           (let ((width (cl-loop for cand in candidates
                                 maximize (string-width cand))))
             (mapcar
              (lambda (cand)
                (let ((annotation (funcall annotator cand)))
                  (list cand ""
                        (if annotation
                            (concat (make-string
                                     (- width (string-width cand)) ?\s)
                                    annotation)
                          ""))))
              candidates)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . ei-entry)
                     (annotation-function . ,annotator)
                     (affixation-function . ,affixator))
        (complete-with-action action collection string pred)))))

(cl-defun ei-read (prompt collection handler
                          &key object path
                          (history-list nil)
                          (require-match nil)
                          (initial-input nil)
                          (annotation nil))
  "Read one entry of COLLECTION and run HANDLER on the choice.
Bind the choice context OBJECT and PATH, activate
`ei-choose-minibuffer-map', annotate the candidates with
ANNOTATION and read through `ei-completing-read-function'.
PROMPT carries the hint of `ei-choose-prompt-hint', which
advertises the M-a, M-h and M-, keys.

HANDLER is called as (CHOICE NEW-HISTORY) when an entry was
selected; its return value is the return value of this function.
When an action was picked in the menu opened with M-a, the result
of running that action is returned instead.  Quitting the menu
with q re-reads, with the minibuffer contents as the new initial
input.

Going back with M-, from a read of a lower level of the descent
lands in this level's re-entry loop: the read is repeated with
the entry chosen here as the initial input.  This works because
HANDLER --- and with it the whole descent below --- runs inside
this function's re-entry loop; see `ei-choose-back'."
  (declare (indent 3))
  (catch 'ei-choose--exit
    (let ((tag (gensym "ei-choose--back-"))
          (input initial-input))
      (push tag ei-choose--tag-stack)
      (unwind-protect
          (while t                   ; re-entry loop for M-, from below
            (catch tag
              (let ((result
                     (catch 'ei-choose-action
                       (setq input
                             (catch 'ei-choose--action-abort
                               (ei-choose--read-once prompt collection
                                                     :object object
                                                     :path path
                                                     :history-list history-list
                                                     :require-match require-match
                                                     :initial-input input
                                                     :annotation annotation)))
                       nil)))
                (when result
                  ;; Remember the entry chosen here, so a back from
                  ;; below re-reads with it as the initial input.
                  (when (eq (car result) :choice)
                    (setq input (nth 1 result)))
                  (throw 'ei-choose--exit
                         (ei-choose--handle-read-result result handler))))))
        (pop ei-choose--tag-stack)))))

(cl-defun ei-choose--read-once (prompt collection
                                       &key object path (history-list nil)
                                       (require-match nil)
                                       (initial-input nil)
                                       (annotation nil))
  "Perform one completing read over COLLECTION for the interface.
PROMPT is extended by `ei-choose-prompt-hint' before the read.
Always report the outcome by throwing to `ei-choose-action': the
value is (cons :action (FUNCTION OBJECT PATH)) when the action
menu selected an action, and (cons :choice (CHOICE .
NEW-HISTORY)) for a completed entry."
  (let* ((table (if annotation
                    (ei-choose--annotated-table collection annotation)
                  collection))
         (prompt (ei-choose--prompt prompt path))
         (choice
          (let ((ei-current-object object)
                (ei-current-path path))
            (minibuffer-with-setup-hook
                (lambda ()
                  (use-local-map
                   (make-composed-keymap ei-choose-minibuffer-map
                                         (current-local-map))))
              (funcall (or ei-completing-read-function
                           #'ei-choose--default-completing-read)
                       prompt table
                       :history-list history-list
                       :require-match require-match
                       :initial-input initial-input)))))
    (if (consp choice)
        (throw 'ei-choose-action (cons :choice choice))
      (throw 'ei-choose-action
             (cons :choice (cons choice history-list))))))

(defun ei-choose--handle-read-result (result handler)
  "Dispatch on RESULT of `ei-read', calling HANDLER.
RESULT is either (cons :choice (CHOICE . NEW-HISTORY)), whose two
parts are passed to HANDLER, or (cons :action (FUNCTION OBJECT
PATH)), in which case FUNCTION is applied to OBJECT with the
choice context bound and its result returned."
  (pcase result
    (`(:action . (,fn ,obj ,path))
     (let ((ei-current-object obj)
           (ei-current-path path))
       (funcall fn obj)))
    (`(:choice ,choice . ,new-history)
     (funcall handler choice new-history))))

(defun ei-read-choice (prompt collection &rest keys)
  "Read one entry of COLLECTION through the choice interface.
A convenience wrapper over `ei-read' for
callers that only need the chosen entry: PROMPT, COLLECTION and
KEYS are passed through and the chosen entry's name is returned.
When an action was picked in the menu instead of choosing, the
result of running that action is returned."
  (apply #'ei-read
         prompt collection
         (lambda (choice _new-history) choice)
         keys))

(defun ei-choose--read-element (lst &key history-list)
  "Read a new element for LST through the choice interface.
Complete over the names of LST's existing elements with the
annotations of the interface; an entry matching no candidate
names a new element, converted by `ei-choose--list-entry-element'.
HISTORY-LIST is the history threaded through the reads.  Return
the new element."
  (ei-read
    "New element: " (mapcar #'car (ei-choose--list-entries lst))
    (lambda (choice _new-history)
      (ei-choose--list-entry-element lst choice))
    :object lst :path ei-current-path
    :history-list history-list
    :annotation (ei-annotator lst)))



(defun ei-choose--apply-op (container item test fun args)
  "Apply the TEST and FUN of `ei-choose' to CONTAINER and ITEM.
TEST and FUN are each either a single function or a list of them.
The first TEST accepting ITEM selects the function at the same
position, which is called as (FUN CONTAINER ITEM ARGS).  Return
(t . RESULT) when a test matched, nil otherwise."
  (let ((tests (if (and (listp test) (functionp (car test)))
                   test (list test)))
        (funs (if (and (listp fun) (functionp (car fun)))
                  fun (list fun))))
    (cl-loop for tst in tests
             for fn in funs
             when (and (functionp tst) (functionp fn)
                       (apply tst container item args))
             return (cons t (apply fn container item args)))))

(defun ei-choose--class-slots (object)
  "Return the slot entries of OBJECT as (NAME . SLOT) pairs.
NAME is the slot name as a completion string and SLOT the slot
descriptor of OBJECT's class."
  (mapcar (lambda (slot) (cons (symbol-name (oref slot name)) slot))
          (eieio-class-slots (eieio-object-class object))))

(defun ei-choose--symbol-entries (sym)
  "Return the entries (NAME . PLACE) of SYM's value and properties.
PLACE is the symbol `value' for the symbol's own value or the
property symbol for a property of SYM.  The value entry is only
present when SYM is bound."
  (append (and (boundp sym) '(("value" . value)))
          (cl-loop for (k _v) on (symbol-plist sym) by #'cddr
                   collect (cons (symbol-name k) k))))

(defun ei-choose--list-entries (lst)
  "Return the entries (NAME . ELEMENT) of the elements of LST.
Elements are named by themselves when possible: EIEIO objects by
their name slot or printed form, strings and characters by
themselves, everything else by its printed form."
  (cond
   ((cl-every #'eieio-object-p lst)
    (mapcar (lambda (obj)
              (cons (if (and (slot-exists-p obj 'name)
                             (slot-boundp obj 'name))
                        (format "%s" (slot-value obj 'name))
                      (format "%s" obj))
                    obj))
            lst))
   ((cl-every #'stringp lst)
    (mapcar (lambda (s) (cons s s)) lst))
   ((cl-every #'characterp lst)
    (mapcar (lambda (c) (cons (char-to-string c) c)) lst))
   (t (mapcar (lambda (x) (cons (prin1-to-string x) x)) lst))))

(defun ei-choose--list-entry-element (lst choice)
  "Return the element of LST named by the candidate string CHOICE.
An existing entry's element is returned; a CHOICE matching no
entry of a list of strings or characters is the new element
itself, and any other unmatched CHOICE is read as a Lisp object.
A CHOICE with no Lisp reading (the empty string of a bare RET,
or an unbalanced fragment) has no element; return nil instead of
signaling out of the reader."
  (or (cdr (assoc choice (ei-choose--list-entries lst)))
      (cond
       ((cl-every #'stringp lst) choice)
       ((cl-every #'characterp lst) (string-to-char choice))
       (t (condition-case nil
            (car-safe (read-from-string choice))
          (error nil))))))


(defun ei-choose--hash-entries (table)
  "Return the entries (NAME . KEY) for the keys of TABLE."
  (mapcar (lambda (k) (cons (format "%s" k) k))
          (hash-table-keys table)))

(defun ei-choose--hash-entry-key (table choice)
  "Return the key of TABLE named by the candidate string CHOICE.
An existing entry's key is returned.  A CHOICE matching no entry
names a new key: CHOICE itself when all keys of TABLE are
strings, the symbol named CHOICE when they are all symbols, and
otherwise --- also for an empty TABLE --- CHOICE read as a Lisp
object.  Return nil for a CHOICE with no Lisp reading, such as
the empty string of a bare RET."
  (let ((entry (assoc choice (ei-choose--hash-entries table)))
        (keys (hash-table-keys table)))
    (cond (entry (cdr entry))
          ((string-empty-p choice) nil)
          ((and keys (cl-every #'stringp keys)) choice)
          ((and keys (cl-every #'symbolp keys)) (intern choice))
          (t (condition-case nil
                 (car (read-from-string choice))
               (error nil))))))

(defun ei-choose--container-kind (value)
  "Return the container kind of VALUE, or nil for leaf values.
The container kinds are `eieio', `obarray', `hash-table' and
`list'; nil and all atoms are leaves of the choice tree."
  (cond ((eieio-object-p value) 'eieio)
        ((obarrayp value) 'obarray)
        ((hash-table-p value) 'hash-table)
        ((and (listp value) value) 'list)
        (t nil)))

(defun ei-choose--constant-symbol-p (value)
  "Return non-nil when VALUE is a symbol that cannot hold a new value.
These are the constants `t' and `nil' and all keywords.  They are
leaf values of the choice tree: descending into their properties
or their own value cannot edit the place that holds them."
  (or (memq value '(t nil)) (keywordp value)))

(defun ei-choose--value-compatible-p (old new)
  "Return non-nil when NEW may replace OLD in an edited place.
Values are compatible when OLD is nil, when they are of the same
type, or when both are lists or both strings.  This is the safety
net that keeps a deep edit from replacing, for example, an
obarray slot by a string."
  (or (null old)
      (eq (type-of old) (type-of new))
      (and (listp old) (listp new))
      (and (stringp old) (stringp new))))

(defun ei-choose--set-slot-value (object slot-name new-value)
  "Set OBJECT's SLOT-NAME to NEW-VALUE, guarded by a type check.
`ei-choose--value-compatible-p' guards the assignment: when the
new value's type differs from the old one's, ask before
replacing.  Return NEW-VALUE."
  (let ((old (and (slot-boundp object slot-name)
                  (slot-value object slot-name))))
    (if (or (ei-choose--value-compatible-p old new-value)
            (y-or-n-p (format "Slot `%s' holds %s; store %s anyway? "
                              slot-name
                              (if old (type-of old) "nothing")
                              (type-of new-value))))
            (setf (slot-value object slot-name) new-value)
        (message "ei: keeping the old value of `%s'" slot-name))
    new-value))

(defun ei-choose--short (object width)
  "Return a one-line printed form of OBJECT of at most WIDTH chars.
Newlines and tabs are replaced by spaces; longer forms are cut
and terminated with an ellipsis.  Printing stops as soon as more
than WIDTH characters were produced, so the cost does not depend
on the size of OBJECT."
  (let ((printed (replace-regexp-in-string
                  "[\n\r\t]" " "
                  (condition-case nil
                      (let ((chars nil)
                            (count 0))
                        (catch 'ei-choose--short
                          (princ object
                                 (lambda (ch)
                                   (push ch chars)
                                   (when (> (cl-incf count) width)
                                     (throw 'ei-choose--short nil)))))
                        (concat (nreverse chars)))
                    (error "#<unprintable>")))))
    (if (<= (length printed) width)
            printed
        (concat (substring printed 0 (max 0 (1- width))) "…"))))

(defun ei-annotation-face (annotation)
  "Return ANNOTATION propertized for the *Completions* buffer.
Nil and empty annotations are returned as nil."
  (if (and annotation (not (string-empty-p annotation)))
      (progn
        (add-face-text-property 0 (length annotation)
                                'completions-annotations t annotation)
        annotation)
    nil))

(define-obsolete-function-alias 'ei-choose--annotation-face
  #'ei-annotation-face "0.2.0")

(provide 'ei-choose)

;;; ei-choose.el ends here
