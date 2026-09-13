;;; ei-generics-tests.el --- Tests for the ei choice interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; License: GPL-3.0-or-later
;; Keywords: lisp, tools

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
;; Non-interactive tests for the choice interface of `ei-interface',
;; with the prompting functions monkey-patched, plus tests for the
;; backward navigation and the edit buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ei-interface)
(require 'table)

(defclass ei-tests--object ()
  ((name :initarg :name :initform "test"
         :type string
         :documentation "Name of the test object.")
   (count :initarg :count :initform 0
          :type number
          :documentation "A number slot.")
   (items :initarg :items :initform nil
          :type list
          :documentation "A list-valued slot.")
   (entries :initarg :entries
            :documentation "An obarray-valued slot, unbound by default."))
  :documentation "Test class for the choice interface.")

(ert-deftest ei-choose-list-chars ()
  "Choosing from a list of chars returns the selected char."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "b")))
    (should (equal (ei-choose (list ?a ?b ?c) :recurse nil) ?b))))

(ert-deftest ei-choose-eieio-returns-slot-value ()
  "Choosing a slot of an EIEIO object returns the slot's value."
  (let ((obj (ei-tests--object :name "hello" :count 7)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "count")))
      (should (equal (ei-choose obj :recurse nil) 7)))))

(ert-deftest ei-choose-does-not-modify-object ()
  "Choosing never writes into the chosen object."
  (let ((obj (ei-tests--object :name "hello" :count 7)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "name")))
      (ei-choose obj)
      (should (equal (oref obj name) "hello"))
      (should (equal (oref obj count) 7)))))

(ert-deftest ei-annotation-shows-slot-type ()
  "The slot annotation includes the slot's type when it has one."
  (let* ((obj (ei-tests--object :name "hello" :count 7))
         (annotation (ei-candidate-annotation obj "count")))
    (should (stringp annotation))
    (should (string-match-p "number" annotation))
    (should (string-match-p "7" annotation))))

(ert-deftest ei-candidate-info-documents-slot ()
  "M-h documentation includes the slot documentation string."
  (let* ((obj (ei-tests--object))
         (info (ei-candidate-info obj "count")))
    (should (stringp info))
    (should (string-match-p "count" info))
    (should (string-match-p "A number slot" info))))

(ert-deftest ei-value-compatible-p ()
  "Compatibility of replacement values."
  (should (ei-choose--value-compatible-p nil "anything"))
  (should (ei-choose--value-compatible-p "old" "new"))
  (should (ei-choose--value-compatible-p '(1 2) '(3)))
  (should-not (ei-choose--value-compatible-p "old" 7)))

(ert-deftest ei-set-slot-value-guards-type ()
  "Incompatible assignments are declined when the user says no."
  (let ((obj (ei-tests--object :count 7)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _a) nil)))
      (ei-choose--set-slot-value obj 'count "not a number")
      (should (equal (oref obj count) 7)))))

(ert-deftest ei-actions-menu-builds-keymap ()
  "The action menu binds every action and reserves q and C-g."
  (let* ((obj (ei-tests--object))
         (menu (ei-choose--actions-menu (ei-actions obj) obj nil))
         (map (car menu))
         (text (cdr menu)))
    (should (keymapp map))
    (should (commandp (lookup-key map [?e])))
    (should (commandp (lookup-key map [?a])))
    (should (commandp (lookup-key map [?d])))
    (should (commandp (lookup-key map [?i])))
    (should (commandp (lookup-key map [?q])))
    (should (commandp (lookup-key map [?\C-g])))
    (should (string-match-p "Edit in place" text))
    (should (string-match-p "Inspect" text))))

(ert-deftest ei-actions-menu-reassigns-taken-keys ()
  "A second action wanting the same key gets the next free one."
  (let* ((actions (list (list ?x "first" #'identity)
                        (list ?x "second" #'identity)))
         (menu (ei-choose--actions-menu actions nil nil)))
    (should (commandp (lookup-key (car menu) [?x])))
    (should (commandp (lookup-key (car menu) [?y])))
    (should (string-match-p "first" (cdr menu)))
    (should (string-match-p "second" (cdr menu)))))

(ert-deftest ei-obarray-annotation-shows-value ()
  "Obarray entries are annotated with their value."
  (let* ((oa (obarray-make))
         (obarray oa)
         (sym (intern "abbrev" oa)))
    (set sym "expansion")
    (let ((annotation (ei-candidate-annotation oa "abbrev")))
      (should (stringp annotation))
      (should (string-match-p "expansion" annotation)))))

(ert-deftest ei-choose-hash-table-value-only ()
  "Choosing a key of a hash-table returns its value."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "one" 1 ht)
    (puthash "two" 2 ht)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "two")))
      (should (equal (ei-choose ht :value-only t) 2)))))

(ert-deftest ei-choose-hash-table-symbol-keys ()
  "Symbol keys are rendered and resolved by name."
  (let ((ht (make-hash-table :test 'eq)))
    (puthash 'alpha "a" ht)
    (puthash 'beta "b" ht)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "beta")))
      (should (equal (ei-choose ht :value-only t) "b")))))

(ert-deftest ei-choose-hash-table-recurse ()
  "Without :value-only the chosen value is chosen into recursively."
  (let ((ht (make-hash-table :test 'equal))
        (inner '("x" "y")))
    (puthash "outer" inner ht)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt _choices &rest _args)
                 (if (string-prefix-p "Key" prompt) "outer" "y"))))
      ;; First choose the key, then the element of the inner list.
      (should (equal (ei-choose ht :recurse t) "y")))))

(ert-deftest ei-choose-hash-table-test-fun ()
  "TEST and FUN receive the table and the chosen key."
  (let ((ht (make-hash-table :test 'equal))
        (got nil))
    (puthash "one" 1 ht)
    (puthash "two" 2 ht)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "two")))
      (ei-choose ht
                 :test (lambda (_tbl x) (string= x "two"))
                 :fun (lambda (_tbl x) (setq got x))))
    (should (equal got "two"))))

(ert-deftest ei-choose-and-apply-list ()
  "Choose-and-apply for lists delegates to fun when predicate passes."
  (let ((called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "two")))
      (ei-choose
       '("one" "two" "three")
       :test (lambda (_lst x) (string= x "two"))
       :fun (lambda (_lst x) (setq called x))))
    (should (equal called "two"))))

(ert-deftest ei-add-list-strings ()
  "Adding to a list of strings conses the read element.
The element is read through the choice interface, and the new
list is handed to the owner through `ei-edit--store-back'."
  (let ((ei-edit--store-back nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "new-item")))
      (let ((res (ei-add (list "a" "b"))))
        (should (equal res '("new-item" "a" "b")))
        (should (equal ei-edit--store-back res))))))

(ert-deftest ei-add-with-obarray-name ()
  "Add-with with string name reads value and installs binding."
  (let ((array (obarray-make 29)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _a) "VAL")))
      (let* ((sym (ei-add array :key "foo"))
             (obarray array))
        (should (symbolp sym))
        (should (eq sym (intern "foo")))
        (should (equal (symbol-value sym) "VAL"))))))

(ert-deftest ei-add-with-obarray-value ()
  "Add-with with value reads the name and installs the binding.
The name is read through the choice interface, so the mock covers
`completing-read'."
  (let ((array (obarray-make 29)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _a) "foo"))
              ((symbol-function 'ei-check)
               (lambda (_array name &rest _a) name)))
      (let* ((sym (ei-add array :value "VAL"))
             (obarray array))
        (should (symbolp sym))
        (should (eq sym (intern "foo")))
        (should (equal (symbol-value sym) "VAL"))))))

(ert-deftest ei-check-obarray-overwrite ()
  "Overwrite path returns the original name."
  (let* ((array (obarray-make 29))
         (obarray array)
         (sym (intern "foo")))
    (set sym "OLD")
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _a) (list ?y))))
      (should (equal (ei-check array "foo") "foo")))))

(ert-deftest ei-check-obarray-change ()
  "Change path prompts for a new name and returns it."
  (let* ((array (obarray-make 29))
         (obarray array)
         (sym (intern "foo")))
    (set sym "OLD")
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _a) (list ?n)))
              ((symbol-function 'completing-read)
               (lambda (&rest _a) "bar")))
      (should (equal (ei-check array "foo") "bar")))))

(ert-deftest ei-with-restored-windows-restores-window ()
  "The selected window shows its old buffer again after the body."
  (let* ((buffer (get-buffer-create " *ei-wr-window*"))
         (before (window-buffer (selected-window)))
         (inside nil))
    (unwind-protect
        (progn
          (ei-with-restored-windows
            (set-window-buffer (selected-window) buffer)
            (setq inside (window-buffer (selected-window))))
          (should (eq inside buffer))
          (should (eq (window-buffer (selected-window)) before)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ei-with-restored-windows-keeps-current-buffer ()
  "The current buffer stays the one the body leaves behind.
`set-window-configuration' would otherwise switch back to the
buffer current when the configuration was recorded; callers
parsing an edit buffer after its session rely on the body's
buffer staying current."
  (let ((buffer (get-buffer-create " *ei-wr-current*")))
    (unwind-protect
        (let ((inside nil)
              (after nil))
          (ei-with-restored-windows
            (set-buffer buffer)
            (setq inside (current-buffer)))
          (setq after (current-buffer))
          (should (eq inside buffer))
          (should (eq after buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ei-with-restored-windows-restores-on-error ()
  "A non-local exit through the body restores the window, too."
  (let* ((buffer (get-buffer-create " *ei-wr-error*"))
         (before (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (should-error
           (ei-with-restored-windows
             (set-window-buffer (selected-window) buffer)
             (error "boom")))
          (should (eq (window-buffer (selected-window)) before)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ei-choose-obarray-value-only ()
  "Choosing from an obarray with :value-only returns the value."
  (let* ((oa (obarray-make))
         (obarray oa)
         (sym (intern "foo" oa)))
    (set sym "expansion")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "foo")))
      (should (equal (ei-choose oa :value-only t) "expansion")))))

(ert-deftest ei-choose-eieio-value-only ()
  "With :value-only the slot value is returned without descending."
  (let ((obj (ei-tests--object :items '(1 2))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "items")))
      (should (equal (ei-choose obj :value-only t) '(1 2))))))

(ert-deftest ei-choose-list-value-only ()
  "With :value-only the chosen element is returned, not descended into."
  (let ((reads 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (cl-incf reads)
                 "(a)")))
      (should (equal (ei-choose '((a) (b)) :value-only t) '(a)))
      (should (= reads 1)))))

(ert-deftest ei-list-entry-element ()
  "Unmatched candidates name new elements according to the list type."
  (should (equal (ei-choose--list-entry-element '("a" "b") "a") "a"))
  (should (equal (ei-choose--list-entry-element '("a" "b") "c") "c"))
  (should (equal (ei-choose--list-entry-element '(?a ?b) "c") ?c))
  (should (equal (ei-choose--list-entry-element '(a b) "c") 'c)))

(ert-deftest ei-read-choice-returns-choice ()
  "The read wrapper hands back the chosen entry's name."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "two")))
    (should (equal (ei-choose--read-choice "Pick: " '("one" "two"))
                   "two"))))

(ert-deftest ei-delete-obarray ()
  "Deleting from an obarray removes the chosen symbol."
  (let* ((oa (obarray-make))
         (obarray oa)
         (sym (intern "foo" oa)))
    (set sym "v")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "foo")))
      (should (eq (ei-delete oa) oa))
      (should-not (intern-soft "foo" oa)))))

(ert-deftest ei-delete-list ()
  "Deleting from a list removes the chosen element and hands it back."
  (let ((ei-edit--store-back nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "b")))
      (should (equal (ei-delete '("a" "b" "c")) '("a" "c")))
      (should (equal ei-edit--store-back '("a" "c"))))))

(ert-deftest ei-actions-menu-list-actions ()
  "Lists offer edit, add and delete in the action menu."
  (let* ((menu (ei-choose--actions-menu (ei-actions '("a" "b")) nil nil))
         (map (car menu)))
    (should (commandp (lookup-key map [?e])))
    (should (commandp (lookup-key map [?a])))
    (should (commandp (lookup-key map [?d])))
    (should (commandp (lookup-key map [?i])))))

(ert-deftest ei-edit-obarray-value-path-plist ()
  "Editing an obarray entry's value reports it as a plist."
  (let* ((oa (obarray-make))
         (obarray oa)
         (sym (intern "foo" oa)))
    (set sym "expansion")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "foo"))
              ((symbol-function 'read-string)
               (lambda (&rest _args) "new")))
      (should (equal (ei-edit oa :history-list nil)
                     `(:old "expansion" :symbol ,sym :new "new")))
      (should (equal (symbol-value sym) "new")))))

(ert-deftest ei-edit-obarray-property-path-plist ()
  "Editing an entry's properties reports a plist with nil values."
  (let* ((oa (obarray-make))
         (obarray oa)
         (sym (intern "foo" oa)))
    (set sym "expansion")
    (let ((completions '("foo" "value"))
          (strings '("" "edited")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) (pop completions)))
                ((symbol-function 'read-string)
                 (lambda (&rest _args) (pop strings))))
        ;; Empty new value falls through to the property edit, which
        ;; edits the symbol's value in place.
        (should (equal (ei-edit oa :history-list nil)
                       `(:old nil :symbol ,sym :new nil)))
        (should (equal (symbol-value sym) "edited"))))))

(ert-deftest ei-edit-symbol-returns-symbol ()
  "Editing a symbol edits its value in place and returns the symbol."
  (let ((sym (intern "ei-tests--edit-symbol")))
    (set sym "old")
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _args) "value"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _args) "new")))
          (should (eq (ei-edit sym :history-list nil) sym))
          (should (equal (symbol-value sym) "new")))
      (unintern sym obarray))))

(ert-deftest ei-edit-eieio-list-slot-store-back ()
  "Editing a list slot installs the edited list handed back."
  (let ((obj (ei-tests--object :items '(1 2))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "items"))
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _args) '(9 8))))
      (ei-edit obj :history-list nil)
      (should (equal (oref obj items) '(9 8))))))

(ert-deftest ei-edit-eieio-obarray-slot-in-place ()
  "Editing an obarray slot edits the obarray, not the slot.
This guards the data-loss bug where the edit's return value
replaced the slot."
  (let* ((oa (obarray-make))
         (obarray oa)
         (sym (intern "k" oa))
         (obj (ei-tests--object :entries oa)))
    (set sym "v")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _args)
                 (cond ((string-prefix-p "Edit slot of" prompt) "entries")
                       (t "k"))))
              ((symbol-function 'read-string)
               (lambda (&rest _args) "w")))
      (ei-edit obj :history-list nil)
      (should (eq (oref obj entries) oa))
      (should (equal (symbol-value sym) "w")))))

;;; Backward navigation

(ert-deftest ei-choose-back-resumes-parent ()
  "Going back from a nested read resumes the parent read.
The descent is hash-table -> inner list; at the inner list, M-,
goes back to the key read, which reads again and descends once
more."
  (let* ((ht (make-hash-table :test 'equal))
         (key-reads 0)
         (calls nil))
    (puthash "outer" '("x" "y") ht)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt _coll &rest _args)
                 (push prompt calls)
                 (cond ((string-prefix-p "Key" prompt)
                        (cl-incf key-reads)
                        "outer")
                       ((string-prefix-p "Element" prompt)
                        (if (= key-reads 1)
                            (ei-choose-back)
                          "y"))))))
      (should (equal (ei-choose ht :recurse t) "y")))
    (should (equal (length calls) 4))))

(ert-deftest ei-choose-back-at-root-stays ()
  "Going back at the top level stays in the same read."
  (let ((calls 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (cl-incf calls)
                 (when (= calls 1) (ei-choose-back))
                 "y"))
              ((symbol-function 'ding) (lambda (&rest _a) t)))
      (should (equal (ei-choose '("x" "y") :value-only t) "y"))
      (should (equal calls 1)))))

(ert-deftest ei-choose-back-restores-parent-input ()
  "After going back, the parent read gets its previous input.
The mock completing-read records the initial-input arguments of
the key reads."
  (let* ((ht (make-hash-table :test 'equal))
         (key-inputs nil)
         (backed nil))
    (puthash "outer" '("x" "y") ht)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt _coll _pred _rm _hist _histvar input)
                 (cond ((string-prefix-p "Key" prompt)
                        (push input key-inputs)
                        "outer")
                       ((string-prefix-p "Element" prompt)
                        (if backed
                            "y"
                          (setq backed t)
                          (ei-choose-back)))))))
      ;; Choose the key, go back from the element read, choose the
      ;; key again and finish at the element; the re-read key
      ;; prompt receives "outer" as its input.
      (should (equal (ei-choose ht :recurse t) "y")))
    (should (equal key-inputs '("outer" nil)))))

;;; The pluggable reader

(ert-deftest ei-completing-read-function-is-used ()
  "The pluggable reader replaces the default completing read."
  (let ((used nil))
    (cl-letf (((symbol-function 'ei-test--reader)
               (lambda (prompt _table &rest _rest)
                 (push prompt used)
                 (cons "y" nil))))
      (let ((ei-completing-read-function #'ei-test--reader))
        (should (equal (ei-choose '("x" "y") :value-only t) "y"))))
    (should (equal (length used) 1))))

;;; The property hook

(ert-deftest ei-after-set-property-hook-runs ()
  "The property hook runs after a leaf property edit."
  (let ((sym (intern "ei-tests--hook-symbol"))
        (seen nil))
    (unwind-protect
        (progn
          (put sym 'myprop 0)
          (cl-defmethod ei-after-set-property
            ((_s (eql ei-tests--hook-symbol)) prop new)
            (push (list prop new) seen))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args) "myprop"))
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _args) '42)))
            (ei-edit sym :history-list nil)
            (should (equal (get sym 'myprop) 42))
            (should (equal seen '((myprop 42))))))
      (unintern sym obarray))))

;;; Edit buffers

(ert-deftest ei-edit-buffer-lines-and-fields ()
  "Content lines are read back; comment lines are skipped."
  (with-temp-buffer
    (ei-edit-buffer-mode)
    (insert "# Title\n# instruction\n\na\t1\nb\t2\n\nplain\n")
    (should (equal (ei-edit-buffer-lines) '("a\t1" "b\t2" "plain")))
    (should (equal (ei-edit-buffer-fields "\t")
                   '(("a" . "1") ("b" . "2") ("plain"))))))

(ert-deftest ei-edit-buffer-driver-finish ()
  "`ei-edit-buffer' returns the buffer with the edited content.
The recursive edit is mocked to edit the buffer in place."
  (unwind-protect
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (goto-char (point-max))
                   (insert "k\tv\n"))))
        (let ((buf (ei-edit-buffer "*ei-test-edit*"
                                   "Test edit"
                                   '("line 1" "line 2")
                                   :populate
                                   (lambda () (insert "old\n")))))
          (should (bufferp buf))
          (with-current-buffer buf
            (should (equal (ei-edit-buffer-lines) '("old" "k\tv"))))))
    (when (get-buffer "*ei-test-edit*")
      (kill-buffer "*ei-test-edit*"))))

(ert-deftest ei-edit-buffer-driver-cancel ()
  "Cancelling the edit signals `quit' from `ei-edit-buffer'."
  (unwind-protect
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda () (signal 'quit nil))))
        (should-error
         (ei-edit-buffer "*ei-test-edit-cancel*" "Cancel" nil)
         :type 'quit))
    (when (get-buffer "*ei-test-edit-cancel*")
      (kill-buffer "*ei-test-edit-cancel*"))))

(ert-deftest ei-edit-buffer-finish-and-cancel-commands ()
  "The finish and cancel commands exit and abort the recursive edit."
  (let ((exited nil) (aborted nil))
    (cl-letf (((symbol-function 'exit-recursive-edit)
               (lambda () (setq exited t)))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (setq aborted t))))
      (ei-edit-buffer-finish)
      (should exited)
      (ei-edit-buffer-cancel)
      (should aborted))))

(ert-deftest ei-escape-string-round-trip ()
  "Escaping and unescaping strings is lossless."
  (dolist (str '("plain" "with\nnewline" "with\ttab"
                 "back\\slash" "all\rthree\n\t\\" ""))
    (should (equal (ei-unescape-string (ei-escape-string str)) str))))

(ert-deftest ei-escape-string-one-line ()
  "Escaped strings contain no raw newlines or tabs."
  (let ((escaped (ei-escape-string "a\nb\tc\\d\re")))
    (should-not (string-match-p "[\n\r\t]" escaped))
    (should (equal escaped "a\\nb\\tc\\\\d\\re"))))

;;; Clean compilation

(defvar ei-generics-tests--directory
  (and load-file-name (file-name-directory load-file-name))
  "Directory holding the ei sources under test.")

(defconst ei-generics-tests--compile-files
  '("ei-choose.el" "ei-generics.el" "ei-edit-buffer.el" "ei-interface.el")
  "Sources byte-compiled by `ei-byte-compile-clean'.")

(defun ei-generics-tests--compile-warnings (file)
  "Return the warning and error lines of byte-compiling FILE in memory.
Compile FILE with `byte-compile-from-buffer' without writing an
output file, capturing the compiler's log in a private buffer."
  (let ((byte-compile-log-buffer " *ei-generics-tests--compile-log*"))
    (with-current-buffer (get-buffer-create byte-compile-log-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((load-path (cons ei-generics-tests--directory load-path)))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name file ei-generics-tests--directory))
        (emacs-lisp-mode)
        (let ((byte-compile-current-file file)
              (byte-compile-dest-file nil))
          (byte-compile-from-buffer (current-buffer)))))
    (with-current-buffer byte-compile-log-buffer
      (cl-remove-if-not
       (lambda (line)
         (or (string-match-p "Warning" line)
             (string-match-p "Error" line)))
       (split-string (buffer-string) "\n" t)))))

(ert-deftest ei-byte-compile-clean ()
  "The ei sources byte-compile without warnings or errors."
  (skip-unless ei-generics-tests--directory)
  (dolist (file ei-generics-tests--compile-files)
    (let ((warnings (ei-generics-tests--compile-warnings file)))
      (should (equal warnings nil)))))

;;; Prompt hints

(ert-deftest ei-prompt-hint-appended ()
  "Every read of the interface advertises the interface keys.
The hint is appended once, after the prompt, and separated by a
space when the prompt does not end with one."
  (let ((prompts nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _args)
                 (push prompt prompts)
                 "count")))
      (ei-choose (ei-tests--object :count 7) :recurse nil)
      (should (string-prefix-p "Slot of " (car prompts)))
      (should (string-match-p "M-a" (car prompts)))
      (should (string-match-p "M-h" (car prompts)))
      (should (string-match-p "M-," (car prompts))))))

(ert-deftest ei-prompt-hint-suppressed ()
  "A nil `ei-choose-prompt-hint' keeps the prompt plain."
  (let ((ei-choose-prompt-hint nil)
        (prompts nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _args)
                 (push prompt prompts)
                 "count")))
      (ei-choose (ei-tests--object) :recurse nil)
      (should (equal (car prompts)
                     "Slot of ei-tests--object: ")))))

;;; Object info

(ert-deftest ei-object-info-table-layout ()
  "The object description lists the slots as a `table-mode' table.
The table has the border characters of `table-insert', the header
row carries the bold face, and every slot appears in it."
  (let* ((obj (ei-tests--object :name "hello" :count 7))
         (info (ei-object-info obj)))
    (should (stringp info))
    (should (string-prefix-p "ei-tests--object object" info))
    (should (string-match-p "\n\\+-+" info))
    (should (string-match-p "| *Slot" info))
    (should (string-match-p "| *Type" info))
    (should (string-match-p "| *Value" info))
    (should (string-match-p "| *name" info))
    (should (string-match-p "| *count" info))
    (should (string-match-p "| *items" info))
    (should (string-match-p "hello" info))
    (should (string-match-p "| *7 *" info))
    (should (text-property-any 0 (length info)
                               'font-lock-face 'bold info))))

(ert-deftest ei-object-info-unbound-slot ()
  "An unbound slot is listed with the unbound marker."
  (let* ((obj (ei-tests--object))
         (info (ei-object-info obj)))
    (should (string-match-p "#<unbound>" info))))

(ert-deftest ei-insert-table-caps-cell-width ()
  "Cell contents are cut to `ei-choose-table-max-cell-width'."
  (with-temp-buffer
    (let* ((limit ei-choose-table-max-cell-width)
           (long (make-string (+ limit 10) ?x))
           (cut (concat (substring long 0 (1- limit)) "…")))
      (ei-choose--insert-table (list (list "key" long)))
      (goto-char (point-min))
      (should (search-forward cut nil t))
      (goto-char (point-min))
      (should-not (search-forward long nil t)))))

(ert-deftest ei-insert-table-leaves-point-behind ()
  "The table fills its cells and point ends behind the last border."
  (with-temp-buffer
    (insert "prefix\n")
    (ei-choose--insert-table '(("a" "b") ("1" "22")))
    (insert "suffix\n")
    (should (equal (buffer-substring (point-min) (point-max))
                   "prefix
+---+----+
| a | b  |
+---+----+
| 1 | 22 |
+---+----+
suffix
"))))

(provide 'ei-generics-tests)

;;; ei-generics-tests.el ends here
