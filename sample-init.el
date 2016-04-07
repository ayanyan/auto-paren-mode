;;; Load
(require 'auto-paren)

;;; Automatic activation for specific modes
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (auto-paren-mode 1)))

;;; Automatic activation for any buffer
(setq-default auto-paren-mode t)

;;; Customization
(setq auto-paren-matching-alist
      (cons
       '(your-favorite-major-mode
         (?\( . ?\))
         (?{ . ?})
         (?\" . ?\")
         (?\: . ?\;)
         (?/ . ?/))
       auto-paren-matching-alist))
(define-key auto-paren-mode-map ":" 'auto-paren-self-insert)
(define-key auto-paren-mode-map "/" 'auto-paren-self-insert)
