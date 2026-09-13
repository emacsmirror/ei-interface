;;; ei-generics.el --- Generic choice/edit operations for containers  -*- lexical-binding: t; -*-

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

;; This file defines the polymorphic API that drives every
;; interactive descent through EIEIO objects and other containers:
;; `ei-choose', `ei-edit', `ei-add', `ei-delete', `ei-check' and
;; `ei-move'.
;;
;; `ei-choose' and its mutating siblings `ei-edit', `ei-add' and
;; `ei-delete' drive the descent: EIEIO objects (choose a slot),
;; obarrays (choose a symbol), hash tables (choose a key), lists
;; (choose an element) and symbols (choose the value or a property);
;; atoms are leaves.  Every completion goes through `ei-choose--read'
;; and `ei-choose--read-once', which report either the chosen entry or
;; an action picked in the menu; `ei-choose--handle-read-result'
;; dispatches on that report.
;;
;; Two conventions keep the interface safe:
;;
;; - `ei-choose' never modifies; only `ei-edit', `ei-add' and
;;   `ei-delete' write, and in-place containers (EIEIO objects,
;;   obarrays, hash tables, symbols) write themselves.  Lists are
;;   value containers: their methods return the new list and store it
;;   in `ei-edit--store-back', and the owning place installs it.
;;   Leaf values travel back as the return value and pass the type
;;   guard `ei-choose--set-slot-value', which asks before replacing a
;;   slot by a value of an incompatible type.
;; - `:test' and `:fun' arguments of `ei-choose' are applied through
;;   `ei-choose--apply-op': the first matching test selects the fun
;;   at the same position, called as (FUN CONTAINER ITEM ARGS).

;;; Code:

(require 'eieio)
(require 'help-macro)
(require 'ei-choose)

(defmacro ei-with-restored-windows (&rest body)
  "Run BODY, then restore the window configuration of the frame.
The configuration recorded with `current-window-configuration'
before BODY runs is reinstated after BODY finishes, whether it
finishes normally or non-locally, for instance on the `quit'
signalled by an aborted recursive edit.  The current buffer is
left as BODY leaves it, so callers that parse an edit buffer
after its session keep working.

Buffers created and displayed by BODY survive; only the
arrangement of windows returns to its prior state.  Wrap the
interactive query sessions of commands with this macro ---
batch-edit buffers, review buffers, wait sessions --- so that
capturing a window to ask the user something does not
permanently reorganize the frame.  Do not wrap code that
displays results meant to stay visible."
  (declare (indent 0) (debug t))
  (let ((config (gensym "ei--window-config-"))
        (buffer-after (gensym "ei--buffer-after-")))
    `(let ((,config (current-window-configuration)))
       (unwind-protect
           (progn ,@body)
         (let ((,buffer-after (current-buffer)))
           ;; `set-window-configuration' also makes the buffer of
           ;; the restored selected window current; switch back so
           ;; the current buffer is the one BODY left behind.
           (set-window-configuration ,config)
           (when (buffer-live-p ,buffer-after)
             (set-buffer ,buffer-after)))))))

(cl-defgeneric ei-choose (object &key test fun args history-list
                                 initial-input value-only recurse
                                 &allow-other-keys)
  "Choose interactively inside OBJECT and return what was chosen.
OBJECT is any container: an EIEIO object (choose a slot), an
obarray (choose a symbol), a hash table (choose a key), a list
(choose an element), a symbol (choose a property) or an atom
(returned unchanged).  Choosing descends into the chosen value
while RECURSE is non-nil (the default); M-, in the minibuffer
goes back to the previous container of the descent.

TEST and FUN, when given, are applied to the chosen entry first,
see `ei-choose--apply-op'.  VALUE-ONLY non-nil returns the value
of the chosen entry instead of descending.  HISTORY-LIST is
threaded through the reads and returned updated by readers that
keep a persistent history.

`ei-choose' never modifies OBJECT; use `ei-edit', `ei-add' and
`ei-delete' for the mutating counterparts.")

(cl-defmethod ei-choose (object &key _test _fun _args _history-list
                                _initial-input _value-only _recurse
                                &allow-other-keys)
  "Return OBJECT unchanged; atoms are leaves of the choice tree."
  object)

(cl-defmethod ei-choose ((_x (eql nil)) &key &allow-other-keys)
  "Return nil; an empty place ends the descent."
  nil)



(cl-defmethod ei-choose ((object eieio-default-superclass)
                         &key test fun args history-list initial-input
                         value-only (recurse t) &allow-other-keys)
  "Choose a slot of OBJECT and return its value.
The candidates are the slot names of OBJECT's class, annotated
with the slot type and current value.  TEST and FUN are applied
to (OBJECT SLOT-NAME ARGS) first; when no test matches, the
chosen slot's value is returned, chosen into recursively while
RECURSE is non-nil and VALUE-ONLY is nil."
  (let ((entries (ei-choose--class-slots object)))
    (ei-choose--read-and-handle
      (format "Slot of %s: " (eieio-object-class object))
      entries
      (lambda (choice new-history)
        (let* ((slot (cdr (assoc choice entries)))
               (slot-name (and slot (oref slot name)))
               (op (and slot-name test fun
                        (ei-choose--apply-op object slot-name
                                              test fun args))))
          (cond
           (op (cdr op))
           ((null slot-name) nil)
           ((not (slot-boundp object slot-name)) nil)
           ((or value-only (not recurse))
            (slot-value object slot-name))
           (t
            (let* ((val (slot-value object slot-name))
                   (ei-current-path
                    (cons (cons object slot-name)
                          ei-current-path)))
              (if (ei-choose--container-kind val)
                  (ei-choose val :history-list new-history
                              :test test :fun fun :args args
                              :recurse recurse)
                val))))))
      :object object :path ei-current-path
      :history-list history-list
      :require-match t
      :initial-input initial-input
      :annotation (ei-choose--annotator object))))

(cl-defmethod ei-choose ((sym symbol) &key history-list &allow-other-keys)
  "Choose a property of SYM and return its value.
The symbol's own value is offered under the name `value' when SYM
is bound."
  (let ((entries (ei-choose--symbol-entries sym)))
    (ei-choose--read-and-handle
      "Property of symbol: " entries
      (lambda (choice _new-history)
        (let ((prop (cdr (assoc choice entries))))
          (cond
           ((eq prop 'value) (and (boundp sym) (symbol-value sym)))
           (prop (plist-get (symbol-plist sym) prop))
           (t nil))))
      :object sym :path ei-current-path
      :history-list history-list
      :annotation (ei-choose--annotator sym))))

(cl-defmethod ei-choose ((array obarray) &key test fun args initial-input
                         value-only history-list (recurse t)
                         &allow-other-keys)
  "Choose a symbol of ARRAY.
Return the symbol's value when it is bound and VALUE-ONLY is
non-nil or RECURSE is nil; otherwise choose into the symbol's
properties.  An unbound chosen symbol is returned itself.  TEST
and FUN are applied to (ARRAY SYMBOL ARGS) first."
  (ei-choose--read-and-handle
    "Entry of obarray: " array
    (lambda (choice new-history)
      (let* ((sym (intern-soft choice array))
             (val (and sym (boundp sym) (symbol-value sym)))
             (op (and sym test fun
                      (ei-choose--apply-op array sym test fun args))))
        (cond
         (op (cdr op))
         ((and sym recurse (not value-only))
          (let ((ei-current-path
                 (cons (cons array sym) ei-current-path)))
            (ei-choose sym :history-list new-history :test test
                       :fun fun :args args :recurse recurse)))
         (t (or val sym choice)))))
    :object array :path ei-current-path
    :history-list history-list
    :initial-input initial-input
    :annotation (ei-choose--annotator array)))

(cl-defmethod ei-choose ((lst list) &key test fun args _initial-input
                         value-only history-list (recurse t)
                         &allow-other-keys)
  "Choose an element of LST.
Candidates are the elements themselves, named as in
`ei-choose--list-entries'; a typed entry matching no candidate
of a string or character list is accepted as a new element, see
`ei-choose--list-entry-element'.  TEST and FUN are applied to
(LST ELEMENT ARGS) first.  With VALUE-ONLY non-nil or RECURSE
nil the chosen element is returned without descending into it."
  (let ((entries (ei-choose--list-entries lst)))
    (ei-choose--read-and-handle
      "Element: " (mapcar #'car entries)
      (lambda (choice new-history)
        (let* ((chosen (ei-choose--list-entry-element lst choice))
               (op (and test fun
                        (ei-choose--apply-op lst chosen
                                              test fun args))))
          (cond
           (op (cdr op))
           ((and chosen recurse (not value-only))
            (let ((ei-current-path
                   (cons (cons lst chosen) ei-current-path)))
              (ei-choose chosen :history-list new-history :test test
                         :fun fun :args args :recurse recurse)))
           (t chosen))))
      :object lst :path ei-current-path
      :history-list history-list
      :annotation (ei-choose--annotator lst))))

(cl-defmethod ei-choose ((table hash-table) &key test fun args initial-input
                         value-only history-list (recurse t)
                         &allow-other-keys)
  "Choose a key of TABLE.
Return the key's value when VALUE-ONLY is non-nil or the value is
a leaf; otherwise choose into the value recursively.  TEST and
FUN are applied to (TABLE KEY ARGS) first."
  (let ((entries (ei-choose--hash-entries table)))
    (ei-choose--read-and-handle
      "Key: " (mapcar #'car entries)
      (lambda (choice new-history)
        (let* ((key (cdr (assoc choice entries)))
               (val (and key (gethash key table)))
               (op (and key test fun
                        (ei-choose--apply-op table key test fun args))))
          (cond
           (op (cdr op))
           ((null key) nil)
           (value-only val)
           ((and recurse (ei-choose--container-kind val))
            (let ((ei-current-path
                   (cons (cons table key) ei-current-path)))
              (ei-choose val :history-list new-history :test test
                         :fun fun :args args :recurse recurse)))
           (t val))))
      :object table :path ei-current-path
      :history-list history-list
      :initial-input initial-input
      :annotation (ei-choose--annotator table))))

(cl-defmethod ei-choose ((str string) &key _history-list &allow-other-keys)
  "Return STR; strings are leaves of the choice tree.
Edit a chosen string through the action menu (M-a) or `ei-edit'."
  str)

(cl-defgeneric ei-add (object &key key value history-list &allow-other-keys)
  "Interactively add data into OBJECT.
If KEY and VALUE are provided, add them directly.
Otherwise, prompt appropriately.")

(cl-defmethod ei-add ((object eieio-default-superclass)
                      &key history-list &allow-other-keys)
  "Fallback method for adding to an EIEIO object.
By default, this falls back to `ei-edit'."
  (message "Adding to object not supported natively. Falling back to edit.")
  (ei-edit object :history-list history-list))

(cl-defmethod ei-add ((array obarray) &key key (value nil value-supplied-p)
                      history-list &allow-other-keys)
  "Add a new binding to ARRAY and return the new symbol.
With KEY and a supplied VALUE (which may be nil), bind KEY to
VALUE.  With KEY alone, read the value with `read-string'.  With
VALUE alone or with neither, read the name through the choice
interface, so the annotations, the action menu (M-a) and the
candidate documentation (M-h) are available, and check it against
the existing entries with `ei-check'."
  (cond
   ((and key value-supplied-p)
    (let ((sym (intern (if (symbolp key) (symbol-name key) key) array)))
      (set sym value)
      sym))
   (key
    (let ((val (read-string (format "Value for %s: " key))))
      (ei-add array :key key :value val :history-list history-list)))
   (t
    (pcase-let ((`(,name . ,new-history)
                 (ei-choose--read-and-handle
                   "Name: " array
                   (lambda (choice hist) (cons choice hist))
                   :object array
                   :path ei-current-path
                   :history-list history-list
                   :annotation (ei-choose--annotator array))))
      (let ((checked (ei-check array name :history-list new-history)))
        (if value-supplied-p
            (ei-add array :key checked :value value
                    :history-list new-history)
          (ei-add array :key checked :history-list new-history)))))))

(cl-defmethod ei-add ((lst list) &key value history-list &allow-other-keys)
  "Add VALUE or a newly read element to LST and return the new list.
The new element is read through the choice interface with
completion over the existing entries, see
`ei-choose--read-element'.  Lists of EIEIO objects with a `name'
slot instead choose one of their elements and add to that object,
since new objects cannot be read as text.  The new list is also
stored in `ei-edit--store-back' for the owning place, see there."
  (cond
   (value
    (setq ei-edit--store-back (cons value lst))
    (cons value lst))
   ((and lst (cl-every #'eieio-object-p lst))
    (ei-choose lst
               :test (lambda (_lst obj)
                       (and (slot-exists-p obj 'name)
                            (slot-boundp obj 'name)
                            (slot-value obj 'name)))
               :fun (lambda (_lst obj)
                      (ei-add obj :history-list history-list))))
   (t
    (let ((elt (ei-choose--read-element lst
                                        :history-list history-list)))
      (setq ei-edit--store-back (cons elt lst))
      (cons elt lst)))))

(cl-defgeneric ei-delete (object &key history-list &allow-other-keys)
  "Interactively delete data from OBJECT.

This is the destructive counterpart of `ei-add'.  Implementations
read the entry to remove through the choice interface, so the
annotations, the action menu (M-a) and the candidate documentation
\(M-h) are available.  The return value is the updated object or a
non-nil status.")

(cl-defmethod ei-delete ((array obarray) &key history-list &allow-other-keys)
  "Interactively delete a symbol from ARRAY through the choice interface.
The candidates carry the annotations of the choice interface and
the action menu (M-a) and the candidate documentation (M-h) are
available while choosing.  Return ARRAY."
  (ei-choose--read-and-handle
    "Delete from obarray: " array
    (lambda (choice _new-history)
      (let ((sym (intern-soft choice array)))
        (when sym
          (unintern sym array)
          (message "Deleted %s from obarray." choice)))
      array)
    :object array :path ei-current-path
    :history-list history-list
    :require-match t
    :annotation (ei-choose--annotator array)))

(cl-defgeneric ei-move (first second)
  "Move an entry from FIRST to SECOND.

Methods specialize on the types of FIRST and SECOND to provide
the appropriate interactive behavior.  The default method does
nothing."
  (ignore first second))

(cl-defmethod ei-delete ((object eieio-default-superclass)
                         &key history-list &allow-other-keys)
  "Delete an entry from a container-valued slot of OBJECT.
Choose a slot with `ei-choose': a slot holding a list loses the
element chosen from it, and a slot holding an obarray delegates
to `ei-delete' on that obarray.  Return OBJECT."
  (ei-choose object
             :test (lambda (ob slot)
                     (let ((val (and (slot-boundp ob slot)
                                     (slot-value ob slot))))
                       (or (obarrayp val)
                           (and (listp val) val))))
             :fun (lambda (ob slot)
                    (let ((val (slot-value ob slot)))
                      (if (obarrayp val)
                          (ei-delete val :history-list history-list)
                        (let* ((elt (ei-choose val :value-only t
                                                :history-list history-list))
                               (new (cl-remove elt val
                                               :test #'equal :count 1)))
                          (setf (slot-value ob slot) new)))))
             :history-list history-list)
  object)

(cl-defmethod ei-delete ((lst list) &key history-list &allow-other-keys)
  "Interactively delete an element from LST.
Choose the element with `ei-choose' and return the shortened
list, which is also stored in `ei-edit--store-back' so the
owning place can install it, see there."
  (let* ((elt (ei-choose lst :value-only t :history-list history-list))
         (new (cl-remove elt lst :test #'equal :count 1)))
    (setq ei-edit--store-back new)
    new))

(cl-defgeneric ei-edit (object &key history-list &allow-other-keys)
  "Interactively edit data inside OBJECT.

This is the modify counterpart of `ei-add'.  Editing descends
with the choice interface until the user reaches a leaf value,
which is edited by the method for its type.  In-place containers
\\(EIEIO objects, obarrays, hash tables and symbols) write their
changes themselves and return a description of the edit; the list
method returns a new list and additionally stores it in
`ei-edit--store-back' so the owning place can install it.  Use
the action menu (M-a) of the choice interface for
context-dependent operations.")

(cl-defmethod ei-edit ((array obarray) &key history-list &allow-other-keys)
  "Edit the value or a property of a symbol chosen from ARRAY.
The value is edited as a string; empty input leaves the value
alone and edits the symbol's properties instead.  Only the chosen
symbol is changed, never ARRAY itself.  Return a plist of the
form (:old OLD-VALUE :symbol SYMBOL :new NEW-VALUE) describing
the edit; OLD-VALUE and NEW-VALUE are the values before and
after the edit on the value path and both nil when a property of
the symbol was edited instead."
  (ei-choose--read-and-handle
    "Edit entry of obarray: " array
    (lambda (choice new-history)
      (let* ((sym (intern-soft choice array))
             (old-val (and sym (boundp sym) (symbol-value sym)))
             (new-val (and sym (stringp old-val)
                           (read-string "New value: " old-val))))
        (cond
         ((and sym new-val (not (string-empty-p new-val)))
          (set sym new-val)
          `(:old ,old-val :symbol ,sym :new ,new-val))
         (sym
          (ei-edit sym :history-list new-history)
          `(:old nil :symbol ,sym :new nil))
         (t nil))))
    :object array :path ei-current-path
    :history-list history-list
    :require-match t
    :annotation (ei-choose--annotator array)))

(cl-defmethod ei-edit ((str string) &key _history-list &allow-other-keys)
  "Edit STR by prompting the user in the minibuffer.
Returns the user's input as a new string."
  (read-string "Edit string: " str))

(cl-defgeneric ei-after-set-property (_symbol _property _new-value)
  "Hook run after a property of SYMBOL was set to NEW-VALUE.
The default method does nothing; specialize it to maintain
derived state after property edits, e.g. caches that depend on
specific properties.")

(cl-defmethod ei-edit ((sym symbol) &key history-list &allow-other-keys)
  "Edit the value or a property of SYM in place.
The symbol's own value is offered under the name `value' when SYM
is bound.  Editing a property with a container value descends
into it, and that container edits itself in place; leaf values
are read back as Lisp objects and stored with `put', after which
`ei-after-set-property' runs.  SYM is returned unchanged so that
a place holding SYM keeps holding it, exactly like the other
in-place containers."
  (let ((entries (ei-choose--symbol-entries sym)))
    (ei-choose--read-and-handle
      "Edit property of symbol: " entries
      (lambda (choice _new-history)
        (let ((prop (cdr (assoc choice entries))))
          (cond
           ((eq prop 'value)
            (set sym (ei-edit (symbol-value sym)))
            sym)
           (prop
            (let ((old (plist-get (symbol-plist sym) prop)))
              (if (ei-choose--container-kind old)
                  (ei-edit old)
                (let ((new (read-from-minibuffer
                            "Set value: "
                            (and old (prin1-to-string old))
                            nil t)))
                  (put sym prop new)
                  (ei-after-set-property sym prop new))))
            sym)
           (t nil))))
      :object sym :path ei-current-path
      :history-list history-list
      :require-match t
      :annotation (ei-choose--annotator sym))))

(cl-defmethod ei-edit ((object eieio-default-superclass)
                       &key history-list &allow-other-keys)
  "Choose a slot of OBJECT and edit its value.
Descend into container-valued slots with `ei-edit'; those write
their changes themselves, so the slot keeps holding them.  A leaf
value's edit result, and the new list handed over by a list
through `ei-edit--store-back', are written back through
`ei-choose--set-slot-value', which guards against replacing a
slot by a value of an incompatible type."
  (let ((entries (ei-choose--class-slots object)))
    (ei-choose--read-and-handle
      (format "Edit slot of %s: " (eieio-object-class object))
      entries
      (lambda (choice new-history)
        (let* ((slot (cdr (assoc choice entries)))
               (slot-name (and slot (oref slot name))))
          (when (and slot-name (slot-boundp object slot-name))
            (let* ((val (slot-value object slot-name))
                   (ei-current-path
                    (cons (cons object slot-name)
                          ei-current-path))
                   (ei-edit--store-back nil)
                   (new (ei-edit val :history-list new-history))
                   ;; In-place containers write themselves; leaves
                   ;; report their new value, lists hand it over
                   ;; through `ei-edit--store-back'.
                   (replacement (or ei-edit--store-back
                                    (and (not (ei-choose--container-kind
                                               val))
                                         new))))
              (when replacement
                (ei-choose--set-slot-value object slot-name replacement))
              new))))
      :object object :path ei-current-path
      :history-list history-list
      :require-match t
      :annotation (ei-choose--annotator object))))

(cl-defmethod ei-edit ((lst list) &key _history-list &allow-other-keys)
  "Edit LST by printing it in the minibuffer and reading it back.

The user edits the printed representation of LST in the
minibuffer and the result is read back as a Lisp object.  Return
the new list and store it in `ei-edit--store-back' so that the
place owning LST can install it."
  (let ((new (read-from-minibuffer "Edit list: " (prin1-to-string lst)
                                   nil t)))
    (setq ei-edit--store-back new)
    new))

(cl-defgeneric ei-check (ob ele &rest args)
  "Validate a prospective element ELE for insertion into OB.

Return a truthy, normalized key or interactively resolve
conflicts.  Specializations handle obarrays and lists.")

(cl-defmethod ei-check ((array obarray) (name string) &key object history-list
                        &allow-other-keys)
  "Check NAME against ARRAY for conflicts.

If NAME already exists in ARRAY as a symbol with a value, prompt
the user to overwrite, choose a different name, or rebind the
existing entry.  Returns the (possibly changed) name."
  (let* ((sym-in-table (intern-soft name array))
         (val (and sym-in-table (symbol-value sym-in-table))))
    (if sym-in-table
        (pcase (car (read-multiple-choice
                     (format "%s for %s taken by %s. Overwrite?" name object val)
                     '((?y "overwrite" "Overwrite existing binding")
                       (?n "change" "Choose another entry")
                       (?r "rebind" "Rebind existing to another name"))))
          (?y name)
          (?n (let ((choice (funcall (or ei-completing-read-function
                                         #'ei-choose--default-completing-read)
                                     "Name: " array
                                     :history-list history-list)))
                (ei-check array (car choice) :history-list (cdr choice))))
          (?r (let* ((choice (funcall (or ei-completing-read-function
                                          #'ei-choose--default-completing-read)
                                      "Rebind existing to name: " array
                                      :history-list history-list))
                    (new (ei-check array (car choice)
                                   :history-list (cdr choice)))
                    (new-sym (intern new)))
                 (set new-sym val)
                 name)))
      name)))

(provide 'ei-generics)

;;; ei-generics.el ends here
