# ei-interface

An unofficial, completion-based interface for EIEIO objects (and
other containers: obarrays, hash tables, lists, symbols) for GNU
Emacs 30.1+.

## What it does

- `ei-choose` descends through the slots of an EIEIO object (or the
  entries of an obarray, hash table, list, or the properties of a
  symbol) with minibuffer completion:
  - candidates are annotated with their type and current value,
  - `M-h` documents the candidate named in the minibuffer
    (including slot documentation),
  - `M-a` opens a context-dependent action menu,
  - `M-,` goes back to the previous container of the descent.
- The mutating counterparts `ei-edit`, `ei-add` and `ei-delete`
  change slots and entries; writes are guarded by a type check
  (`ei-choose--set-slot-value`).
- `ei-edit-buffer` provides generic recursive-edit buffers for
  editing many entries at once (`C-c C-c` finish, `C-c C-k`
  cancel).
- Extension points: specialize `ei-actions`, `ei-extra-actions`
  (context-dependent actions reached through the choice path),
  `ei-object-info`, `ei-candidate-annotation`, `ei-candidate-info`,
  and the generic operations themselves for your own classes.

## Status / installation

ei-interface is **not yet on MELPA**.  Until it is, packages that
use it (abv, Daselt) link to a local checkout through their Eldev
files, e.g.:

```elisp
(add-to-list 'load-path
             (expand-file-name "~/proj/ei-interface"))
```

or, in an `init.el`:

```elisp
(add-to-list 'load-path "~/proj/ei-interface")
(require 'ei-interface)
```

The completing read goes through `ei-completing-read-function`;
bind it to a history-aware reader to give every decision point of
the descent a persistent history.

## License

GPL-3.0-or-later, see [LICENSE](LICENSE).
