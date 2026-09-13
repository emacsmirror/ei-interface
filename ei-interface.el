;;; ei-interface.el --- Completion-based interface for EIEIO objects  -*- lexical-binding: t; -*-

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

;; An unofficial, completion-based interface for EIEIO objects and
;; other containers (obarrays, hash tables, lists, symbols).
;; `ei-choose' descends through the slots of an object with
;; minibuffer completion, with annotations, per-candidate
;; documentation (M-h), a context-dependent action menu (M-a) and
;; backward navigation through the descent (M-,).  The mutating
;; counterparts `ei-edit', `ei-add' and `ei-delete' change slots
;; and entries along the way, and `ei-edit-buffer' provides
;; recursive-edit buffers for editing many entries at once.
;;
;; This file just loads the components; see ei-choose.el for the
;; completion engine, ei-generics.el for the generic operations
;; and ei-edit-buffer.el for the edit buffers.
;;
;; ei-interface is not (yet) on MELPA; packages using it (abv,
;; Daselt) link to a local checkout through their Eldev files
;; until it is.

;;; Code:

(require 'ei-choose)
(require 'ei-generics)
(require 'ei-edit-buffer)

(provide 'ei-interface)

;;; ei-interface.el ends here
