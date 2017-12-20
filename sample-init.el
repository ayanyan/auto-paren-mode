;;; Load
(require 'auto-paren)

;;; Automatic activation for specific modes
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (auto-paren-mode 1)))

;;; Automatic activation for any buffer
(setq-default auto-paren-mode t)

;;; Customization
(add-to-list 'auto-paren-matching-alist
             '(your-favorite-major-mode
               (?\( . ?\))
               (?{ . ?})
               (?\" . ?\")
               (?\: . ";;") ; a string may be inserted
               (?& . your-function) ; a function may be called
               (?+ . ?+)))
(define-key auto-paren-mode-map ":" 'auto-paren-self-insert)
(define-key auto-paren-mode-map "&" 'auto-paren-self-insert)
(define-key auto-paren-mode-map "+" 'auto-paren-self-insert)
