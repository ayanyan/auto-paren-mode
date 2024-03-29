# auto-paren-mode

This software provides a minor mode in Emacs.

## Features

* When you type an opening parenthesis, a closing parenthesis is automatically inserted depending on the current major mode.

* Typing `\C-c)` closes all opened expressions.

* The minor mode can be used with any major mode in programming.

* Insertion is easily customizable in the traditional manner.

* Electric Pair mode provided in Emacs 24 or later is maybe better for some people.

## Quick Start

1. Put the file into one of your load-paths.

2. Write `(require 'auto-paren)` in your init-file and restart Emacs.

3. Execute `auto-paren-mode` in your buffer.

0. Instead of the above steps 1-2, you can use `straight.el`.
```
(use-package auto-paren
  :straight (:host github :repo "ayanyan/auto-paren-mode")
  :commands (auto-paren-mode))
```

## Copyright Notice

Follow GPL v3 or later.
