;;; auto-paren.el --- automatic insertion of closing parentheses

;;; Copyright (C) 2005 Yoshihiko Kakutani

;;; Author: Yoshihiko Kakutani <yoshihiko.kakutani@gmail.com>

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
;; inserted depending on the current major mode.
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
;;
;; You can customize the behavior on a specified major mode as
;; follows.
;;
;; (setq auto-paren-major-mode-alist
;;       (cons
;;        '(your-favorite-major-mode
;;          (?\( . ?\))
;;          (?{ . ?})
;;          (?\" . ?\")
;;          (?\: . ?\;)
;;          (?/ . ?/))
;;        auto-paren-major-mode-alist))
;; (define-key auto-paren-mode-map ":" 'auto-paren-self-insert)
;; (define-key auto-paren-mode-map "/" 'auto-paren-self-insert)

;;; Code:

(defvar auto-paren-on-word nil
  "If nil, the automatic insertion is inhibited before or inside
a word in Auto Paren minor mode.")

(defconst auto-paren-lisp-matching-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?\" . ?\")))

(defconst auto-paren-code-matching-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))

(defconst auto-paren-text-matching-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\` . ?\')
    (?\" . ?\")))

(defconst auto-paren-tex-matching-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\` . ?\')
    (?$ . ?$)))

(defconst auto-paren-xml-matching-alist
  '((?< . ?>)
    (?\" . ?\")))

(defvar auto-paren-matching-alist auto-paren-code-matching-alist)

(defvar auto-paren-major-mode-alist
  `((lisp-mode . ,auto-paren-lisp-matching-alist)
    (emacs-lisp-mode . lisp-mode)
    (scheme-mode . lisp-mode)
    (common-lisp-mode . lisp-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (sh-mode . ,auto-paren-code-matching-alist)
    (makefile-mode . sh-mode)
    (makefile-bsdmake-mode . makefile-mode)
    (makefile-gmake-mode . makefile-mode)
    (c-mode . ,auto-paren-code-matching-alist)
    (ruby-mode . ,auto-paren-code-matching-alist)
    (perl-mode . ,auto-paren-code-matching-alist)
    (cperl-mode . perl-mode)
    (caml-mode . ,auto-paren-code-matching-alist)
    (sml-mode . ,auto-paren-code-matching-alist)
    (haskel-mode . ,auto-paren-code-matching-alist)
    (c++-mode . c-mode)
    (java-mode . ,auto-paren-code-matching-alist)
    (js-mode . ,auto-paren-code-matching-alist)
    (pascal-mode . ,auto-paren-code-matching-alist)
    (text-mode . ,auto-paren-text-matching-alist)
    (tex-mode . ,auto-paren-tex-matching-alist)
    (latex-mode . tex-mode)
    (bibtex-mode . tex-mode)
    (yatex-mode)
    (xml-mode . ,auto-paren-xml-matching-alist)
    (sgml-mode . xml-mode)
    (html-mode . sgml-mode)
    (psgml-mode . sgml-mode)
    (yahtml-mode)
    (fundamental-mode . text-mode)))

(defvar auto-paren-mode-map nil
  "Keymap for Auto Paren minor mode.")

(unless auto-paren-mode-map
  (setq auto-paren-mode-map (make-sparse-keymap))
  (define-key auto-paren-mode-map "(" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "[" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "{" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "<" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "`" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "'" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "\"" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "$" 'auto-paren-self-insert))

(defvar auto-paren-mode-hook nil)

(defun rec-assoc (key alist)
  (let ((pair (assoc key alist)))
    (if (and pair (not (listp (cdr pair))))
        (rec-assoc (cdr pair) alist)
      pair)))

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
  (make-local-variable 'auto-paren-matching-alist)
  (make-local-variable 'auto-paren-on-word)
  (let ((pair (rec-assoc major-mode auto-paren-major-mode-alist)))
    (if pair
        (setq auto-paren-matching-alist (cdr pair))
      (setq auto-paren-matching-alist auto-paren-code-matching-alist)))
  (run-hooks 'auto-paren-mode-hook))

(defun auto-paren-self-insert (n)
  "Insert a parentheses pair."
  (interactive "*p")
  (let ((escaped nil))
    (save-excursion
      (let ((matched t))
        (while (and matched  (< (point-min) (point)))
          (backward-char 1)
          (if (looking-at "\\s\\")
              (setq escaped (if escaped nil (match-string 0)))
            (setq matched nil)))))
    (let ((now auto-paren-mode))
      (auto-paren-mode 0)
      (call-interactively (key-binding (char-to-string last-command-event)))
      (auto-paren-mode (if now 1 0)))
    (unless escaped
      (let ((pair (assoc last-command-event auto-paren-matching-alist)))
        (when (and pair (or auto-paren-on-word (not (looking-at "\\w"))))
          (save-excursion
            (let ((c-or-s (cdr pair)))
              (cond
               ((not c-or-s))
               ((stringp c-or-s) (insert-string c-or-s))
               (t (insert-char c-or-s 1))))))))))

(provide 'auto-paren)

;;; auto-paren.el ends here
