;------------------
; package settings
;------------------

;; el-get
;; develop version recipe install without emacswiki recipes
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; adding local recipe
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;; select packages and install A.K.A 'sync'
(setq my-packages
      (append
       '(
         el-get
         color-theme-ir-black
         auto-complete
         anything
         anything-c-moccur
         moccur-edit
         descbinds-anything
         ;; still Forbidden...
         ;; undo-tree
         cperl-mode
         set-perl5lib
         ruby-mode
         php-mode
         js2-mode
         flymake
         )
       ))

(el-get 'sync my-packages)


;------------------
; reset settings
;------------------

;; C-h is backspace
(keyboard-translate ?\C-h ?\C-?)

;; be able to open image binary as image
(auto-image-file-mode t)

;; disable startup message
(setq inhibit-startup-message t)

;; disable menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; disable visual bell and beep
(setq visible-bell nil)
(setq ring-bell-function '(lambda ()))

;; blink cursor is annoying...
(blink-cursor-mode 0)

;; each blacket is resonate
(show-paren-mode 1)

;; white space is evil.
(require 'whitespace)
(setq whitespace-style '(
                         face
                         trailing
                         lines-tail
                         space-before-tab
                         space-after-tab))
(global-whitespace-mode 1)

;; show cursor positions
(column-number-mode t)
(line-number-mode t)

;; dont need backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; dont need auto save
(setq auto-save-default nil)

;; dont care about uppercase and lowercase
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; more than short history is greatness
(setq history-length 10000)
(setq recentf-max-saved-items 10000)

;; misleading files is to be uniquely
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; show function name
(which-function-mode 1)

;; indent space is not evil
(setq-default indent-tabs-mode nil)

;; tab width is 4
(setq-default tab-width 4)

;; auto revert buffer
(global-auto-revert-mode 1)

;; 1 line scroll
(setq scroll-step            1
      scroll-conservatively  10000)

;; toggle line wrapping
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; recentf mode
(recentf-mode t)

;; C-t to switch or split window
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-t") 'other-window-or-split)

;; C-x k to kill current buffer
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer))
(define-key global-map (kbd "C-x k") 'kill-current-buffer)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;------------------
; elisp settings
;------------------

;; color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-ir-black)

;; moccur-edit
(require 'moccur-edit)
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))

;; anything
(require 'anything-startup)

(setq
 anything-idle-delay 0.3
 anything-quick-update t)

(define-key global-map (kbd "C-x b") 'anything)

;; anything-show-kill-ring
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; anything-complete
(anything-read-string-mode 1)

;; anything-c-moccur
(require 'color-moccur)
(setq moccur-split-word t)

(require 'anything-c-moccur)
(setq anything-c-moccur-anything-idle-delay 0.2
      anything-c-moccur-higligt-info-line-flag t
      anything-c-moccur-enable-auto-look-flag t
      anything-c-moccur-enable-initial-pattern t)

(define-key global-map (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
(define-key global-map (kbd "C-M-o") 'anything-c-moccur-dmoccur)

;; descbinds-anything
(require 'descbinds-anything)
(descbinds-anything-install)


;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'objc-mode)

(setq ac-auto-show-menu 0.8)

(setq ac-use-menu-map t)

(define-key ac-mode-map (kbd "C-;") 'auto-complete)


;------------------
; flymake settings
;------------------
(require 'flymake)

;; disable GUI warnings
(setq flymake-gui-warnings-enabled nil)

;; M-e to jump error
(defun next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))
(define-key global-map (kbd "M-e") 'next-flymake-error)


;;-----------------------------------------------------------
;; cperl-mode (https://github.com/typester/emacs-config.git)
;;-----------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.psgi$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . perl-mode))

(setq cperl-indent-level 4)
(setq cperl-continued-statement-offset 4)
(setq cperl-brace-offset -4)
(setq cperl-label-offset -4)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-tab-always-indent t)
(setq cperl-highlight-variables-indiscriminately t)

(require 'cperl-mode)

;; func(sub { みたいなときのインデント崩れを修正パッチ powered by IMAKADO
;; https://gist.github.com/145957
(defun cperl-calculate-indent (&optional parse-data) ; was parse-start
  "Return appropriate indentation for current line as Perl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.

Will not correct the indentation for labels, but will correct it for braces
and closing parentheses and brackets."
  ;; This code is still a broken architecture: in some cases we need to
  ;; compensate for some modifications which `cperl-indent-line' will add later
  (save-excursion
    (let ((i (cperl-sniff-for-indent parse-data)) what p)
      (cond
       ;;((or (null i) (eq i t) (numberp i))
       ;;  i)
       ((vectorp i)
        (setq what (assoc (elt i 0) cperl-indent-rules-alist))
        (cond
         (what (cadr what))		; Load from table
         ;;
         ;; Indenters for regular expressions with //x and qw()
         ;;
         ((eq 'REx-part2 (elt i 0)) ;; [self start] start of /REP in s//REP/x
          (goto-char (elt i 1))
          (condition-case nil	; Use indentation of the 1st part
              (forward-sexp -1))
          (current-column))
         ((eq 'indentable (elt i 0))	; Indenter for REGEXP qw() etc
          (cond       ;;; [indentable terminator start-pos is-block]
           ((eq 'terminator (elt i 1)) ; Lone terminator of "indentable string"
            (goto-char (elt i 2))	; After opening parens
            (1- (current-column)))
           ((eq 'first-line (elt i 1)); [indentable first-line start-pos]
            (goto-char (elt i 2))
            (+ (or cperl-regexp-indent-step cperl-indent-level)
               -1
               (current-column)))
           ((eq 'cont-line (elt i 1)); [indentable cont-line pos prev-pos first-char start-pos]
            ;; Indent as the level after closing parens
            (goto-char (elt i 2))	; indent line
            (skip-chars-forward " \t)") ; Skip closing parens
            (setq p (point))
            (goto-char (elt i 3))	; previous line
            (skip-chars-forward " \t)") ; Skip closing parens
            ;; Number of parens in between:
            (setq p (nth 0 (parse-partial-sexp (point) p))
                  what (elt i 4))	; First char on current line
            (goto-char (elt i 3))	; previous line
            (+ (* p (or cperl-regexp-indent-step cperl-indent-level))
               (cond ((eq what ?\) )
                      (- cperl-close-paren-offset)) ; compensate
                     ((eq what ?\| )
                      (- (or cperl-regexp-indent-step cperl-indent-level)))
                     (t 0))
               (if (eq (following-char) ?\| )
                   (or cperl-regexp-indent-step cperl-indent-level)
                 0)
               (current-column)))
           (t
            (error "Unrecognized value of indent: %s" i))))
         ;;
         ;; Indenter for stuff at toplevel
         ;;
         ((eq 'toplevel (elt i 0)) ;; [toplevel start char-after state immed-after-block]
          (+ (save-excursion		; To beg-of-defun, or end of last sexp
               (goto-char (elt i 1))	; start = Good place to start parsing
               (- (current-indentation) ;
                  (if (elt i 4) cperl-indent-level 0)))	; immed-after-block
             (if (eq (elt i 2) ?{) cperl-continued-brace-offset 0) ; char-after
             ;; Look at previous line that's at column 0
             ;; to determine whether we are in top-level decls
             ;; or function's arg decls.  Set basic-indent accordingly.
             ;; Now add a little if this is a continuation line.
             (if (elt i 3)		; state (XXX What is the semantic???)
                 0
               cperl-continued-statement-offset)))
         ;;
         ;; Indenter for stuff in "parentheses" (or brackets, braces-as-hash)
         ;;
         ((eq 'in-parens (elt i 0))
          ;; in-parens char-after old-indent-point is-brace containing-sexp

          ;; group is an expression, not a block:
          ;; indent to just after the surrounding open parens,
          ;; skip blanks if we do not close the expression.
          (+ (progn
               (goto-char (elt i 2))		; old-indent-point
               (current-column))
             (if (and (elt i 3)		; is-brace
                      (eq (elt i 1) ?\})) ; char-after
                 ;; Correct indentation of trailing ?\}
                 (+ cperl-indent-level cperl-close-paren-offset)
               0)))
         ;;
         ;; Indenter for continuation lines
         ;;
         ((eq 'continuation (elt i 0))
          ;; [continuation statement-start char-after is-block is-brace]
          (goto-char (elt i 1))		; statement-start
          (+ (if (memq (elt i 2) (append "}])" nil)) ; char-after
                 0			; Closing parenth
               cperl-continued-statement-offset)
             (if (or (elt i 3)		; is-block
                     (not (elt i 4))		; is-brace
                     (not (eq (elt i 2) ?\}))) ; char-after
                 0
               ;; Now it is a hash reference
               (+ cperl-indent-level cperl-close-paren-offset))
             ;; Labels do not take :: ...
             (if (looking-at "\\(\\w\\|_\\)+[ \t]*:")
                 (if (> (current-indentation) cperl-min-label-indent)
                     (- (current-indentation) cperl-label-offset)
                   ;; Do not move `parse-data', this should
                   ;; be quick anyway (this comment comes
                   ;; from different location):
                   (cperl-calculate-indent))
               (current-column))
             (if (eq (elt i 2) ?\{)	; char-after
                 cperl-continued-brace-offset 0)))
         ;;
         ;; Indenter for lines in a block which are not leading lines
         ;;
         ((eq 'have-prev-sibling (elt i 0))
          ;; [have-prev-sibling sibling-beg colon-line-end block-start]
          (goto-char (elt i 1))		; sibling-beg
          (if (> (elt i 2) (point)) ; colon-line-end; have label before point
              (if (> (current-indentation)
                     cperl-min-label-indent)
                  (- (current-indentation) cperl-label-offset)
                ;; Do not believe: `max' was involved in calculation of indent
                (+ cperl-indent-level
                   (save-excursion
                     (goto-char (elt i 3)) ; block-start
                     (current-indentation))))
            (current-column)))
         ;;
         ;; Indenter for the first line in a block
         ;;
         ((eq 'code-start-in-block (elt i 0))
          ;;[code-start-in-block before-brace char-after
          ;; is-a-HASH-ref brace-is-first-thing-on-a-line
          ;; group-starts-before-start-of-sub start-of-control-group]
          (goto-char (elt i 1))
          ;; For open brace in column zero, don't let statement
          ;; start there too.  If cperl-indent-level=0,
          ;; use cperl-brace-offset + cperl-continued-statement-offset instead.
          (+ (if (and (bolp) (zerop cperl-indent-level))
                 (+ cperl-brace-offset cperl-continued-statement-offset)
               cperl-indent-level)
             (if (and (elt i 3)	; is-a-HASH-ref
                      (eq (elt i 2) ?\})) ; char-after: End of a hash reference
                 (+ cperl-indent-level cperl-close-paren-offset)
               0)
             ;; Unless openbrace is the first nonwhite thing on the line,
             ;; add the cperl-brace-imaginary-offset.
             (if (elt i 4) 0		; brace-is-first-thing-on-a-line
               cperl-brace-imaginary-offset)
             (progn
               (goto-char (elt i 6))	; start-of-control-group
               (if (elt i 5)		; group-starts-before-start-of-sub
                   ;; handle  |fn(sub {..
                   ;;         |    `!!'
                   (if (save-excursion
                         (ignore-errors
                           (skip-chars-backward " \t")
                           (or (string= (char-to-string (preceding-char)) "(")
                               (string= (char-to-string (preceding-char)) ","))))
                       (save-excursion (back-to-indentation)
                                       (current-column))
                     (current-column))
                 ;; Get initial indentation of the line we are on.
                 ;; If line starts with label, calculate label indentation
                 (if (save-excursion
                       (beginning-of-line)
                       (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*:[^:]"))
                     (if (> (current-indentation) cperl-min-label-indent)
                         (- (current-indentation) cperl-label-offset)
                       ;; Do not move `parse-data', this should
                       ;; be quick anyway:
                       (cperl-calculate-indent))
                   (current-indentation))))))
         (t
          (error "Unrecognized value of indent: %s" i))))
       (t
        (error "Got strange value of indent: %s" i))))))

;; flymake for perl
;; (defun flymake-perl-init ()
;;   (plcmp-with-set-perl5-lib
;;    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                         'flymake-create-temp-inplace))
;;           (local-file  (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name)))
;;           (perl5lib (split-string (or (getenv "PERL5LIB") "") ":"))
;;           (args '("-wc")))
;;      (progn
;;        (dolist (lib perl5lib)
;;          (unless (equal lib "")
;;            (add-to-list 'args (concat "-I" lib) t)))
;;        (add-to-list 'args local-file t)
;;        (list "perl" args)))))

;; (setq flymake-allowed-file-name-masks
;;       (cons '("\\.\\(t\\|p[ml]\\|psgi\\)$"
;;               flymake-perl-init
;;               flymake-simple-cleanup
;;               flymake-get-real-file-name)
;;             flymake-allowed-file-name-masks))

;; (add-hook 'cperl-mode-hook
;;           '(lambda () (flymake-mode t)))


;------------------
; js2-mode settings
;------------------
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js2-mode-hook 'js-indent-hook)

