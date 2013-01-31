;; php-mode
(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . php-mode))
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/")
  (setq php-mode-force-pear t))

;; php-completion
(defun php-completion-hook ()
  (when (require 'php-completion nil t)
    (php-completion-mode t)
    (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)

    (when (require 'auto-completion nil t)
      (make-variable-buffer-local 'as-sources)
      (add-to-list 'as-sources 'as-souorce-php-completion)
      (auto-complete-mode t))))

(add-hook 'php-mode-hook 'php-completion-hook)
