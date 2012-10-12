;; kill buffer
(define-key global-map (kbd "C-x C-k") 'kill-buffer)

;; default encoding
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; silent
(set-message-beep 'silent)

;; disabeld startup display
(setq inhibit-startup-message t)

;; block highlight
(show-paren-mode 1)

;; frame title
(setq frame-title-format "%f")

;; current directory
(cd "~/")

;; newline and indent
(global-set-key "\C-m" 'newline-and-indent)

;; backup setting
(add-to-list 'backup-directory-alist
	     (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; add to load-path
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")

;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")

;; auto-install
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; key-chord
(when (require 'key-chord nil t)
  (key-chord-mode 1)
  (key-chord-define-global "df" 'describe-bindings))

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; undo tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; gtags
(setq gtags-suggested-key-mapping t)
(require 'gtags nil t)
(defun my-c-mode-update-gtags ()
  (let* ((file (buffer-file-name (current-buffer)))
     (dir (directory-file-name (file-name-directory file))))
    (when (executable-find "global")
      (start-process "gtags-update" nil
             "global" "-uv"))))

(add-hook 'after-save-hook
      'my-c-mode-update-gtags)

;; anything
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 100
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
	     (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))
  
  (define-key global-map (kbd "C-l") 'anything)

  ;; anything-gtags
  (when (require 'anything-gtags nil t)
    (setq anything-for-tags
	  (list anything-c-source-gtags-select))

    (defun anything-for-tags ()
      "Preconfigured `anything' for anything-for-tags."
      (interactive)
      (anything anything-for-tags
		(thing-at-point 'symbol)
		nil nil nil "*anything for tags*"))

    (define-key global-map (kbd "M-t") 'anything-for-tags)
    (define-key global-map (kbd "C-M-t") 'gtags-find-rtag))

  ;; anything-show-kill-ring
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring)

  ;; anything-c-moccur
  (when (require 'anything-c-moccur nil t)
    (setq
     anything-c-moccur-anything-idle-delay 0.1
     anything-c-moccur-highlight-info-line-flag t
     anything-c-moccur-enable-auto-look-flag t
     anything-c-moccur-enable-initial-pattern t)
    (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

  ;; anything-for-document
  (setq anything-for-document-sources
	(list anything-c-source-man-pages
	      anything-c-source-info-cl
	      anything-c-source-info-pages
	      anything-c-source-info-elisp
	      anything-c-source-apropos-emacs-commands
	      anything-c-source-apropos-emacs-functions
	      anything-c-source-apropos-emacs-variables))

  (defun anything-for-document ()
    "Preconfigured `anything' for anything-for-document."
    (interactive)
    (anything anything-for-document-sources
	      (thing-at-point 'symbol) nil nil nil
	      "*anything for document*"))
  (define-key global-map (kbd "C-M-d") 'anything-for-document))
  
;; package
(when (require 'package nil t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
	       '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

;; auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; php-mode
(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . php-mode))
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/")
  (setq php-mode-force-pear t))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'js-indent-hook)

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

;; flymake-php
(add-hook 'php-mode-hook 'flymake-php-load)
(when (require 'flymake nil t)
  (global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line))

;; howm 
(setq howm-directory (concat user-emacs-directory "howm"))
(setq howm-menu-lang 'ja)
; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
(when (require 'howm-mode nil t)
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;; ELScreen
(setq elscreen-prefix-key (kbd "C-t"))
(when (require 'elscreen nil t)
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))
