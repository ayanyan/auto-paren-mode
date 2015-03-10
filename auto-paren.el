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
;;
;; You can customize the behavior on a specified major mode as
;; follows.
;;
;; (setq auto-paren-matching-alist
;;       (cons
;;        '(your-favorite-major-mode
;;          (?\( . ?\))
;;          (?{ . ?})
;;          (?\" . ?\")
;;          (?\: . ?\;)
;;          (?/ . ?/))
;;        auto-paren-matching-alist))
;; (define-key auto-paren-mode-map ":" 'auto-paren-self-insert)
;; (define-key auto-paren-mode-map "/" 'auto-paren-self-insert)

;;; Code:

(defvar auto-paren-on-word nil
  "If nil, the automatic insertion is inhibited before or inside
a word in Auto Paren minor mode.")

(defvar auto-paren-respect-syntax-table t
  "If non-nil, the current syntax table is respected.")

(defconst auto-paren-lisp-matching-pairs
  '((?\( . ?\))
    (?\[ . ?\])
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

(defconst auto-paren-text-matching-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?{ . ?})
    (?\` . ?\')
    (?\" . ?\")))

(defconst auto-paren-tex-matching-pairs
  (append
   '((?\[ . ?\])
     (?{ . ?})
     (?| . ?|)
     (?$ . ?$))
   auto-paren-text-matching-pairs))

(defconst auto-paren-xml-matching-pairs
  (append
   '((?< . ?>)
     (?\" . ?\")
     (?& . ?\;))
   auto-paren-text-matching-pairs))

(defvar auto-paren-matching-pairs auto-paren-code-matching-pairs)

(defvar auto-paren-global-matching-pairs nil)

(defvar auto-paren-matching-alist
  `((lisp-mode . ,auto-paren-lisp-matching-pairs)
    (emacs-lisp-mode . lisp-mode)
    (scheme-mode . lisp-mode)
    (common-lisp-mode . lisp-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (sh-mode . ,auto-paren-code-matching-pairs)
    (makefile-mode . sh-mode)
    (makefile-bsdmake-mode . makefile-mode)
    (makefile-gmake-mode . makefile-mode)
    (c-mode . ,auto-paren-code-matching-pairs)
    (ruby-mode . ,auto-paren-code-matching-pairs)
    (perl-mode . ,auto-paren-code-matching-pairs)
    (cperl-mode . perl-mode)
    (caml-mode . ,auto-paren-code-matching-pairs)
    (sml-mode . ,auto-paren-code-matching-pairs)
    (haskel-mode . ,auto-paren-code-matching-pairs)
    (c++-mode . c-mode)
    (java-mode . ,auto-paren-code-matching-pairs)
    (js-mode . ,auto-paren-code-matching-pairs)
    (pascal-mode . ,auto-paren-code-matching-pairs)
    (text-mode . ,auto-paren-text-matching-pairs)
    (tex-mode . ,auto-paren-tex-matching-pairs)
    (latex-mode . tex-mode)
    (bibtex-mode . tex-mode)
    (yatex-mode)
    (xml-mode . ,auto-paren-xml-matching-pairs)
    (sgml-mode . xml-mode)
    (html-mode . sgml-mode)
    (psgml-mode . sgml-mode)
    (nxml-mode . xml-mode)
    (yahtml-mode)
    (fundamental-mode . text-mode)))

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
  (define-key auto-paren-mode-map "&" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "@" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "#" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "%" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "^" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "*" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "/" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "+" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "-" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "=" 'auto-paren-self-insert)
  (define-key auto-paren-mode-map "\C-c)" 'auto-paren-close-all))

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
  (make-local-variable 'auto-paren-matching-pairs)
  (make-local-variable 'auto-paren-on-word)
  (make-local-variable 'auto-paren-respect-syntax-table)
  (let ((pair (rec-assoc major-mode auto-paren-matching-alist)))
    (if pair
        (setq auto-paren-matching-pairs (cdr pair))
      (setq auto-paren-matching-pairs auto-paren-code-matching-pairs)))
  (run-hooks 'auto-paren-mode-hook))

(defun auto-paren-escapedp (&optional pos)
  (if (not pos) (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (not (zerop (% (skip-syntax-backward "\\") 2)))))

(defun auto-paren-match (char)
  (and
   char
   (or (if auto-paren-respect-syntax-table
           (let ((syntax (char-syntax char)))
             (cond ((equal syntax ?\() (aref (syntax-table) char))
                   ((equal syntax ?\") (cons nil char))
                   (t (assoc char auto-paren-matching-pairs))))
         (assoc char auto-paren-matching-pairs))
       (assoc char auto-paren-global-matching-pairs))))

(defun auto-paren-post-insert (char)
  (let ((pair (auto-paren-match char)))
    (when pair
      (let ((c-or-s (cdr pair)))
        (cond
         ((not c-or-s))
         ((stringp c-or-s) (insert c-or-s))
         (t (insert-char c-or-s 1)))))))

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

(defun auto-paren-close-any ()
  (interactive)
  (if (auto-paren-escapedp)
      nil
    (let ((char
           (save-excursion
             (let ((level 0))
               (while (and (<= 0 level) (skip-syntax-backward "^()") (< (point-min) (point)))
                 (backward-char 1)
                 (unless (auto-paren-escapedp)
                   (let ((syntax (char-syntax (char-after))))
                     (setq level (+ level
                                    (cond ((equal syntax ?\() -1)
                                          ((equal syntax ?\)) 1)))))))
               (if (< level 0) (char-after) nil)))))
      (auto-paren-post-insert char)
      char)))

(defun auto-paren-close-all ()
  (interactive)
  (let ((char (auto-paren-close-any)))
    (if char
        (auto-paren-close-all))))

(provide 'auto-paren)

;;; auto-paren.el ends here
