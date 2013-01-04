
;; alpha setting
(set-frame-parameter nil 'alpha 90)

;; set font size
(set-face-attribute 'default nil :height 100)

;; disable menu bar
(tool-bar-mode 0)

;; disable scrollbar
(scroll-bar-mode nil)

;; disable fringe
(fringe-mode 'none)

;; disable blinking cursor
(blink-cursor-mode 0)

;; scrolled as one line
(setq scroll-step 1)

;; delete region at C-h
(delete-selection-mode 1)

;; indent to space
(setq-default indent-tabs-mode nil)

;; C-h to backspace
(global-set-key "\C-h" 'delete-backward-char)

;; kill buffer
(define-key global-map (kbd "C-x C-k") 'kill-buffer)

;; kill process
(setq delete-exited-processes t)

;; default encoding
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; silent
(when window-system
  (set-message-beep 'silent))

;; disabeld startup display
(setq inhibit-startup-message t)

;; block highlight
(show-paren-mode 1)

;; frame title
(setq frame-title-format "%f")

;; current directory
(cd "~/Work/")

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

;; source
;;; color-theme-ir-black.el --- pastel color theme
;; MIT License Copyright (c) 2009 Burke Libbey <burke@burkelibbey.org>
;; URL: https://github.com/kurtharriger/color-theme-ir-black
;; Author: Burke Libbey, Kurt Harriger
;; Version: 1.0.1
;; Package-Requires: ((color-theme "6.6.1"))

(require 'color-theme)
(defun color-theme-ir-black ()
  (interactive)

  (let ((*normal*       "#F6F3E8")
	(*background*   "#000000")
	(*cursor*       "#FFA560")
	(*mouse*        "sienna1")
	(*region*       "#1D1E2C")
	(*current-line* "#151515")
	(*string*       "#A8FF60")
	(*keyword*      "#66B5FF")
	(*method*       "#FFB774")
	(*comment*      "#7C7C7C")
	(*constant*     "#99CC99")
	(*red*          "#FF6C60")
	(*operator*     "#FFFFFF")
	(*class*        "#FFFFB6")
	(*variable*     "#C6C5FE"))

    (flet ((color (fgcolor &optional (bgcolor nil) (bold nil) (italic nil) (underline nil))
		  `((t (,@(if fgcolor   `(:foreground ,fgcolor))
			,@(if bgcolor   `(:background ,bgcolor))
			,@(if bold      '(:bold       t))
			,@(if italic    '(:italic     t))
			,@(if underline '(:underline  t))))))
	   (face (face &rest args)
		 `(,(intern (concat "font-lock-" face "-face"))
		   ,(apply #'color args))))

      (color-theme-install
       `(color-theme-ir-black
	 ((background-color . ,*background*)
	  (background-mode  . dark)
	  (border-color     . ,*background*)
	  (cursor-color     . ,*cursor*)
	  (foreground-color . ,*normal*)
	  (mouse-color      . ,*mouse*))
	 (default ,(color *normal* *background*))
	 (blue ,(color "blue"))
	 (border-glyph ((t (nil))))
	 (buffers-tab ,(color *normal* *background*))
	 ,(face "builtin" *normal*)
	 ,(face "comment" *comment*)
	 ,(face "constant" *constant*)
	 ,(face "doc-string" *string*)
	 ,(face "function-name" *method*)
	 ,(face "keyword" *keyword*)
	 ,(face "preprocessor" *keyword*)
	 ,(face "reference" "#99CC99")
	 ,(face "regexp-grouping-backslash" *red*)
	 ,(face "regexp-grouping-construct" *red*)
	 ,(face "string" *string*)
	 ,(face "type" "#FFB774")
	 ,(face "variable-name" *variable*)
	 ,(face "warning" "white" *red*)
	 (gui-element ,(color *background* "#D4D0C8"))
	 (region ,(color nil *region*))
	 (mode-line ,(color *background* "grey75"))
	 (highlight ,(color nil *current-line*))
	 (highline-face ,(color nil *current-line*))
	 (italic ((t (nil))))
	 (left-margin ((t (nil))))
	 (text-cursor ,(color *background* "yellow"))
	 (toolbar ((t (nil))))
	 (bold ((t (:bold t))))
	 (bold-italic ((t (:bold t))))
	 (underline ((nil (:underline nil)))))))))

(provide 'color-theme-ir-black)

(eval-after-load "color-theme"
  '(progn
     (color-theme-ir-black)))


;; undo tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

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

  (require 'anything-grep nil t)

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
  (when (and (require 'anything-exuberant-ctags nil t)
	     (require 'anything-gtags nil t))
    (setq anything-for-tags
	  (list anything-c-source-imenu
		anything-c-source-gtags-select
		anything-c-source-exuberant-ctags-select))

    (defun anything-for-tags ()
      "Preconfigured `anything' for anything-for-tags."
      (interactive)
      (anything anything-for-tags
		(thing-at-point 'symbol)
		nil nil nil "*anything for tags*"))

    (define-key global-map (kbd "M-t") 'anything-gtags-select))

  ;; anything-grep
  (define-key global-map (kbd "C-c C-g") 'anything-grep)

  ;; anything-show-kill-ring
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring)

  ;; anything-c-moccur
  (when (require 'anything-c-moccur nil t)
    (setq
     anything-c-moccur-anything-idle-delay 0.1
     anything-c-moccur-highlight-info-line-flag t
     anything-c-moccur-enable-auto-look-flag t
     anything-c-moccur-enable-initial-pattern t)
    (global-set-key (kbd "C-r") 'anything-c-moccur-occur-by-moccur))

  (global-set-key (kbd "C-M-r") 'anything-c-moccur-dmoccur)

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
  (define-key ac-mode-map (kbd "M-q") 'auto-complete)
  (ac-config-default))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'js-indent-hook)

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

;; flymake-php
(add-hook 'php-mode-hook 'flymake-php-load)
(when (require 'flymake nil t)
  (global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line))

;; perl
;; steal from typester ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)
(setq cperl-continued-statement-offset 4)
(setq cperl-brace-offset -4)
(setq cperl-label-offset -4)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-tab-always-indent t)
;(setq cperl-electric-parens t) ; 対応する括弧自動挿入 うざい
;(setq cperl-invalid-face nil)
(setq cperl-highlight-variables-indiscriminately t)

; steal from perlhacks
(global-set-key "\M-p" 'cperl-perldoc)

(add-hook 'cperl-mode-hook '(lambda ()
                              (setq indent-tabs-mode nil)
                              ; BestPractices からぱくったがなんかうごいてない
                              (setq fill-column 78)
                              (setq auto-fill-mode t)
                              ; face設定。これはどっかちがうとこにうつす
                              (set-face-background 'cperl-hash-face (face-background 'default))
                              (setq cperl-hash-face 'cperl-hash-face)
                              ;(make-face 'cperl-array-face)
                              ;(set-face-foreground 'cperl-array-face "color-69")
                              (set-face-background 'cperl-array-face (face-background 'default))
                              (setq cperl-array-face 'cperl-array-face)
                              ))
                              

; from Best Practices
; Use % to match various kinds of brackets...
;(defun match-paren (arg)
;  "Go to the matching paren if on a paren; otherwide insert %."
;  (interactive "p")
;  (let ((prev-char (char-to-string (preceding-char)))
;	(next-char (char-to-string (following-char))))
;    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
;	  ((string-match "[\]})>" prev-char) (backward-sexp 1))
;	  (t (self-insert-command (or arg 1))))))
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)


; perl tidy
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
  (perltidy-region)))

;; perl completion
;(add-to-list 'load-path "~/.emacs.d/elisp/perl-completion")
;(add-hook 'cperl-mode-hook (lambda ()
;                             (require 'perl-completion)
;                             (perl-completion-mode t)))

(add-hook  'cperl-mode-hook (lambda ()
                              (require 'auto-complete)
                              (require 'perl-completion)
                              (add-to-list 'ac-sources 'ac-source-perl-completion)
                              (perl-completion-mode t)))

(global-set-key "\C-ct" 'perltidy-region)
(global-set-key "\C-c\C-t" 'perltidy-defun)

(add-to-list 'auto-mode-alist '("\\.t$" . perl-mode))

(eval-after-load "perl-completion"
  '(progn
     (defadvice flymake-start-syntax-check-process (around flymake-start-syntax-check-lib-path activate) (plcmp-with-set-perl5-lib ad-do-it))
     (define-key plcmp-mode-map (kbd "M-TAB") nil)
     (define-key plcmp-mode-map (kbd "M-C-o") 'plcmp-cmd-smart-complete)))

;; flymake for perl
(defvar flymake-perl-err-line-patterns '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))
(defconst flymake-allowed-perl-file-name-masks '(("\\.pl$" flymake-perl-init)
                                               ("\\.pm$" flymake-perl-init)
                                               ("\\.t$" flymake-perl-init)
                                               ))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (flymake-mode t))

(add-hook 'cperl-mode-hook '(lambda () (flymake-perl-load)))

(defun next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))
(global-set-key "\M-e" 'next-flymake-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; howm 
(setq howm-directory (concat user-emacs-directory "howm"))
(setq howm-menu-lang 'ja)
; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
(when (require 'howm-mode nil t)
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;; ELScreen
; (setq elscreen-prefix-key (kbd "C-t"))
; (when (require 'elscreen nil t)
;  (if window-system
;      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
;    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))
