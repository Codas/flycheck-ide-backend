Flycheck for Haskell via ide-backend
===========================================================

[![License GPL 3][badge-license]][copying]

This checker uses
[ide-backend-client] and in extend [ide-backend] via [ide-backend-mode] to
provide a fast and easy to configure [Flycheck] syntax checker for [Haskell]
that just works (wherever [ide-backend-mode] works).

Installation
------------

[Flycheck] version 0.21 or greater is needed for this checker to work properly
since this version added support for generic syntax checkers.

To install and register this checker first Clone this repository:

```
git clone https://github.com/Codas/flycheck-ide-backend
```

Then call `flycheck-ide-backend-setup` in your `init.el`:

```emacs-lisp
(use-package haskell-mode
  :defer t
  :config
  (progn
    (require 'ide-backend-mode)
    (add-hook 'haskell-mode-hook 'ide-backend-mode)
    (use-package flycheck
      :defer t
      :config
      (progn
        (require 'flycheck-ide-backend)
        (flycheck-ide-backend-setup)
      ))))
```

Or simply add `haskell-ide-backend` to the list of syntax checkers:

```emacs-lisp
(require 'flycheck-ide-backend)
(add-to-list 'flycheck-checkers 'haskell-ide-backend)
```


Usage
-----

Just use Flycheck as usual in your Cabal projects.

Note that this checker frequently reloads the current file in ide-backend-mode
(via ide-backend-mode-load). It does not however save the current file and
cannot provide on the fly error checking on unsaved files.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [`COPYING`][copying] for details.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/Codas/flycheck-ide-backend/blob/master/COPYING
[Flycheck]: https://www.flycheck.org
[ide-backend-mode]: https://github.com/chrisdone/ide-backend-mode
[ide-backend-client]: https://github.com/chrisdone/ide-backend-client
[ide-backend]: https://github.com/fpco/ide-backend
[Haskell]: https://www.haskell.org/
