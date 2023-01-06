;;; auto-paren.el --- automatic insertion of closing parentheses

;;; Copyright (C) 2005 Yoshihiko Kakutani

;;; Author: Yoshihiko Kakutani

;;; Copyright Notice:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A minor mode to help you to type parentheses.  When you type an
;; opening parenthesis, a closing parenthesis is automatically
;; inserted depending on the current major mode.  In Emacs 24.3 or
;; later, Electric Pair mode is probably more useful.
;;
;; First, you need to load the file.  It is usual to write the
;; following code in your .emacs file.
;;
;; (require 'auto-paren)
;;
;; Alternatively, the following is maybe enough.
;;
;; (autoload 'auto-paren-mode "auto-paren" nil t)
;;
;; You can enjoy the mode by calling `auto-paren-mode' interactively.
;; It is recommended to set `skeleton-pair' nil while the mode is
;; active.
;;
;; If the following code is in your .emacs, the mode is automatically
;; activated whenever an Emacs Lisp file is opened.
;;
;; (add-hook 'emacs-lisp-mode-hook
;;   (lambda ()
;;     (auto-paren-mode 1)))

;;; Code:

(defvar auto-paren-on-word nil
  "If nil, the automatic insertion is inhibited before or inside
a word in Auto Paren minor mode.")

(defvar auto-paren-respect-syntax-table t
  "If non-nil, the current syntax table is respected in Auto
  Paren minor mode.")

(defconst auto-paren-lisp-matching-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\')
    (?\`)
    (?\" . ?\")))

(defconst auto-paren-code-matching-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))

(defconst auto-paren-code-with-regex-matching-pairs
  (append
   '((?/ . ?/))
   auto-paren-code-matching-pairs))

(defconst auto-paren-shell-matching-pairs
  (append
   '((?\[ . " \]"))
   auto-paren-code-matching-pairs))

(defconst auto-paren-ruby-matching-pairs
  (append
   '((?| . ?|))
   auto-paren-code-with-regex-matching-pairs))

(defconst auto-paren-text-matching-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\` . auto-paren-replace-and-insert-quotes)
    (?- . auto-paren-replace-dash)
    (?\" . ?\")))

(defconst auto-paren-tex-matching-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?$ . ?$)
    (?\` . ?\')
    (?| . ?|)))

(defconst auto-paren-xml-matching-pairs
  (append
   '((?< . ?>)
     (?\" . ?\")
     (?& . ";")
     (?/ . auto-paren-nxml-remove-double-closer))
   auto-paren-text-matching-pairs))

(defvar auto-paren-matching-pairs auto-paren-code-matching-pairs)

(defvar auto-paren-global-matching-pairs nil
  "List of key pairs, which is available in any major mode with
Auto Paren minor mode.")

(defvar auto-paren-matching-alist
  `((lisp-mode . ,auto-paren-lisp-matching-pairs)
    (emacs-lisp-mode . lisp-mode)
    (scheme-mode . lisp-mode)
    (common-lisp-mode . lisp-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (sh-mode . ,auto-paren-shell-matching-pairs)
    (makefile-mode . sh-mode)
    (makefile-bsdmake-mode . makefile-mode)
    (makefile-gmake-mode . makefile-mode)
    (c-mode . ,auto-paren-code-matching-pairs)
    (c++-mode . c-mode)
    (java-mode . c-mode)
    (perl-mode . ,auto-paren-code-matching-pairs)
    (cperl-mode . perl-mode)
    (ruby-mode . ,auto-paren-ruby-matching-pairs)
    (python-mode . ,auto-paren-code-matching-pairs)
    (js-mode . ,auto-paren-code-with-regex-matching-pairs)
    (caml-mode . ,auto-paren-lisp-matching-pairs)
    (tuareg-mode . caml-mode)
    (sml-mode . caml-mode)
    (haskell-mode . ,auto-paren-lisp-matching-pairs)
    (coq-mode . ,auto-paren-lisp-matching-pairs)
    (coffee-mode . js-mode)
    (pascal-mode . ,auto-paren-code-matching-pairs)
    (fortran-mode . ,auto-paren-code-matching-pairs)
    (f90-mode . fortran-mode)
    (R-mode . ,auto-paren-code-matching-pairs)
    (ess-mode . R-mode)
    (text-mode . ,auto-paren-text-matching-pairs)
    (tex-mode . ,auto-paren-tex-matching-pairs)
    (latex-mode . tex-mode)
    (bibtex-mode . tex-mode)
    (xml-mode . ,auto-paren-xml-matching-pairs)
    (sgml-mode . xml-mode)
    (html-mode . sgml-mode)
    (psgml-mode . sgml-mode)
    (nxml-mode . xml-mode)
    (rhtml-mode . html-mode)
    (web-mode . ,auto-paren-code-matching-pairs)
    (css-mode . html-mode)
    (scss-mode . css-mode)
    (sass-mode . css-mode)
    (yatex-mode)
    (yahtml-mode)
    (markdown-mode . ,auto-paren-code-matching-pairs)
    (fundamental-mode . text-mode))
  "Alist of major mode names and lists of key pairs.  A key pair
consists of an opening parenthesis and its associated closing
parenthesis.  A closing parenthesis may be a string or a function
without argument.")

(defvar auto-paren-mode-map nil
  "Keymap for Auto Paren minor mode.")

(unless auto-paren-mode-map
  (setq auto-paren-mode-map (make-sparse-keymap))
  (define-key auto-paren-mode-map "(" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map ")" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "[" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "]" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "{" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "}" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "<" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map ">" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "\"" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "`" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "'" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "|" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "$" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "@" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "#" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "%" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "&" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "*" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "/" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "-" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "\C-c\C-t" 'auto-paren-toggle-on-word)
  (define-key auto-paren-mode-map "\C-c)" 'auto-paren-close-all))

(defvar auto-paren-mode-hook nil)

(defun rec-assoc (key alist)
  (let ((pair (assoc key alist)))
    (if (and pair (not (listp (cdr pair))))
        (rec-assoc (cdr pair) alist)
      pair)))

(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

(define-minor-mode auto-paren-mode
  "Toggle Auto Paren minor mode.
With ARG, turn the mode off if and only if ARG is a non-positive
number.

When the mode is enabled, the corresponding closing parenthesis
is automatically inserted immediately after an opening
parenthesis is inserted."
  nil
  " AutoCl"
  auto-paren-mode-map
  (make-local-variable 'auto-paren-matching-pairs)
  (make-local-variable 'auto-paren-on-word)
  (make-local-variable 'auto-paren-respect-syntax-table)
  (let ((pair (rec-assoc major-mode auto-paren-matching-alist)))
    (if pair
        (setq auto-paren-matching-pairs (cdr pair))
      (setq auto-paren-matching-pairs auto-paren-code-matching-pairs)))
  (run-hooks 'auto-paren-mode-hook))

(defun auto-paren-toggle-on-word ()
  "Toggle the value of `auto-paren-on-word'."
  (interactive)
  (setq auto-paren-on-word (not auto-paren-on-word))
  (message (if auto-paren-on-word "enabled" "disabled")))

(defun auto-paren-escapedp (&optional pos)
  (if (not pos) (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (not (zerop (% (skip-syntax-backward "\\") 2)))))

(defun auto-paren-match (char)
  (and
   char
   (or (assoc char auto-paren-matching-pairs)
       (if auto-paren-respect-syntax-table
           (let ((syntax (char-syntax char)))
             (cond ((equal ?\( syntax) (aref (syntax-table) char))
                   ((equal ?\" syntax) (cons nil char))
                   (t nil)))
         nil)
       (assoc char auto-paren-global-matching-pairs))))

(defun auto-paren-post-insert (char)
  (let ((pair (auto-paren-match char)))
    (when pair
      (let ((obj (cdr pair)))
        (cond
         ((not obj))
         ((stringp obj) (insert obj))
         ((characterp obj) (insert-char obj 1))
         ((functionp obj) (apply obj (list char))))))))

(defun auto-paren-self-insert (n)
  "Insert a character or maybe a pair of parentheses."
  (interactive "*p")
  (let ((escaped (auto-paren-escapedp)))
    (let ((now auto-paren-mode))
      (auto-paren-mode 0)
      (call-interactively (key-binding (char-to-string last-command-event)))
      (auto-paren-mode (if now 1 0)))
    (when (and (not escaped)
               (or auto-paren-on-word (not (looking-at "\\w"))))
      (save-excursion
        (auto-paren-post-insert last-command-event)))))

(defun auto-paren-skip-backward (&optional level)
  (if (not level) (setq level 1))
  (while (and (< 0 level) (< (point-min) (point)))
    (skip-syntax-backward "^()")
    (if (= (point-min) (point))
        nil
      (backward-char)
      (unless (auto-paren-escapedp)
        (let ((syntax (char-syntax (char-after))))
          (cond ((equal ?\( syntax) (setq level (- level 1)))
                ((equal ?\) syntax) (setq level (+ level 1))))))))
  (and (<= level 0) (point)))

(defun auto-paren-close-any (&optional equiv)
  "Guess a closing parenthesis at point and insert it."
  (interactive)
  (if (not equiv) (setq equiv (point)))
  (if (auto-paren-escapedp)
      nil
    (let ((pos (save-excursion
                 (goto-char equiv)
                 (auto-paren-skip-backward 1))))
      (if pos (auto-paren-post-insert (char-after pos)))
      pos)))

(defun auto-paren-close-all (&optional equiv)
  "Insert all closing parentheses necessary at point.  This
function guesses wrong when an unmatched parenthesis occurs in a
string data."
  (interactive)
  (if (not equiv) (setq equiv (point)))
  (let ((equiv (auto-paren-close-any equiv)))
    (if equiv
        (auto-paren-close-all equiv))))

(defun auto-paren-nxml-remove-double-closer (&optional char)
  "Remove an unexpectedly inserted \">\"."
  (when (and (equal ?> (char-before))
             (equal ?> (char-after)))
    (delete-char 1)
    (let ((tab-always-indent t))
      (indent-for-tab-command))))

(defun auto-paren-replace-and-insert-quotes (char)
  "Insert quotation marks from ascii."
  (backward-char)
  (cond
   ((and (equal ?\` char) (equal ?\‘ (char-before)) (equal ?\’ (char-after (+ (point) 1))))
    (backward-char)
    (insert "“")
    (delete-char 3)
    (insert "”"))
   ((equal ?\` char)
    (insert "‘")
    (delete-char 1)
    (insert "’"))
   ((equal ?\" char)
    (insert "“")
    (delete-char 1)
    (insert "”"))
   (t
    (forward-char))))

(defun auto-paren-replace-dash (char)
  "Insert dash symbols from ascii."
  (backward-char)
  (cond
   ((and (equal ?- char) (equal ?- (char-before)))
    (backward-char)
    (insert "—")
    (delete-char 2))
   (t
    (forward-char))))

(provide 'auto-paren)

;;; auto-paren.el ends here
