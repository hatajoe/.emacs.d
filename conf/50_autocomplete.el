(require 'auto-complete)
(global-auto-complete-mode t)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

;; 日本語入力中にonだとうざいので切る
(defadvice ac-on-post-command
    (around check-whether-input-type-is-japanese activate)
      (or current-input-method ad-do-it))
