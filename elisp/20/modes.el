;;;;;;;;;;;;;;;;;;;;;;;;; mode defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scheme (copied from cs51)

(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq scheme-mit-dialect nil)
(setq scheme-program-name "stk")

;;; script mode

(defun determine-script-mode ()
  (goto-char (point-min))
  (cond
   ((looking-at "#![ \n\t]*/[^ \n\t]*perl")
    (perl-mode))
   ((looking-at "#![ \n\t]*/[^ \n\t]*make")
    (makefile-mode))
   ((looking-at "#![ \n\t]*/[^ \n\t]*awk")
    (awk-mode))
   ))

;(setq find-file-hooks
;      (cons 'determine-script-mode
;            find-file-hooks))

;;;text mode

(defun my-text-setup ()
  (interactive)
;  (define-key indented-text-mode-map "" 'delete-backward-char)
;  (define-key indented-text-mode-map "\C-m" 'newline-and-indent)
;  (define-key indented-text-mode-map "\t" 'tab-to-tab-stop)
  (auto-fill-mode 1)
  ;;(setq fill-column 75)
  )
(add-hook 'text-mode-hook 'my-text-setup)

;; setup java compile

;; setup java/cc modes

(require 'font-lock)

;;; customerize your java style
(defun my-java-setup ()
  (abbrev-mode 1)  
  ;(viper-mode)
  (c-add-style
   "my-java"
   '((c-offsets-alist . ((arglist-intro . +)
			 (access-label . 0)
			 (case-label . *)
			 (statement-case-intro . *)
			 )))
   t)
  (cond (window-system
	 (require 'andersl-java-font-lock)
	 (turn-on-font-lock)))
  )

;;; customerize your C/C++ style
(defun my-cc-setup ()
  (c-add-style
   "my-cc"
   '((c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			(substatement-open . +)
			(label . 0)
			(statement-case-open . +)
			(statement-cont . +)
			(arglist-close . c-lineup-arglist)
			(access-label  . 0)                        
			)))
   t)
  )

;;
;; global TAB definitions
;;
(load "hippie-exp")

(setq hippie-expand-try-functions-list '(
;          try-expand-all-abbrevs
;          try-expand-line
	   try-expand-dabbrev
	   try-expand-dabbrev-all-buffers
	   try-complete-file-name
	   try-complete-lisp-symbol
))

;;; Kaiyang -- not used yet.
(define-abbrev-table 'java-mode-abbrev-table
  '(("sop" "System.out.println(" nil 1)
    ("ctm" "System.currentTimeMillis(" nil 1)
    ("sac" "System.arraycopy" nil 1)
    ("ioe" "IOException" nil 1)
    ("iji" "import java.io.*;")
    ("ijn" "import java.net.*;")
    ("ijt" "import java.text.*;")
    ("iju" "import java.util.*;")
    ))

(defun should-expand ()
  (and (eq (char-syntax (preceding-char)) ?w)
       (not (= (current-column) 0))))

(defun global-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "P")
  (if (should-expand)
      (hippie-expand arg)
      (c-indent-command)))

(defun my-cc-common-setup ()
  (interactive)
  (setq c-basic-offset 4)
  (setq c-auto-newline nil)
  (setq indent-tabs-mode t)
  (setq c-hungry-delete-key t)
  (setq dabbrev-case-replace nil)
  (define-key c-mode-base-map "\t" 'global-tab)
  (define-key c-mode-base-map [C-right] 'c-indent-command)
  (define-key c-mode-base-map [C-kp-right] 'c-indent-command)  
  (define-key c-mode-base-map "\C-m" `newline-and-indent)
  (define-key c-mode-base-map "\C-c\C-c" 'comment-region)  
  (define-key c-mode-base-map "\C-c\C-u" 'uncomment-region))

(add-hook 'java-mode-hook 'my-java-setup)
(add-hook 'c-mode-common-hook 'my-cc-common-setup)
(add-hook 'c++-mode-hook 'my-cc-setup)

; Kaiyang -- setup the alist, add your extension here
(setq auto-mode-alist (append '(("\\.pl\\'" . perl-mode)
				("\\.C\\'" . c++-mode)
				("\\.H\\'" . c++-mode)
				("\\.cc\\'" . c++-mode)
				("\\.cxx\\'" . c++-mode)
				("\\.c\\'" . c++-mode)
				("\\.h\\'" . c++-mode)
				("\\.jad\\'" . java-mode)
				("\\.mocha\\'" . java-mode)
				("\\.tpl\\'" . html-mode))
			      auto-mode-alist))

;;; Setup perl mode

(defun my-perl-setup ()
  (interactive)
  (font-lock-mode))

(add-hook 'perl-mode-hook 'my-perl-setup)

;;; Setup lisp mode

(defun my-lisp-setup ()
  (interactive)
  (define-key lisp-mode-map "" 'delete-backward-char)
  (define-key lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key lisp-mode-map "\M-\C-m" 'indent-new-comment-line)
  (cond (window-system
     (font-lock-mode 1)
     (setq font-lock-keywords lisp-font-lock-keywords))))

(add-hook 'lisp-mode-hook 'my-lisp-setup)
(defun my-elisp-setup ()
  (interactive)
  (define-key emacs-lisp-mode-map "" 'delete-backward-char)
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key emacs-lisp-mode-map "\M-\C-m" 'indent-new-comment-line)
  (cond (window-system
     (font-lock-mode 1)
     (setq font-lock-keywords lisp-font-lock-keywords))))


(add-hook 'emacs-lisp-mode-hook 'my-elisp-setup)

;;; elisp debugging
(autoload 'edebug-defun "edebug" "debugger for elisp" t)
(autoload 'edebug-all-defuns "edebug" "debugger for elisp" t)
(setq edebug-global-prefix "\C-xX")
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)
(setq debugger 'edebug-debug)
(setq max-lisp-eval-depth 10000)

;;; 
(cond (window-system
       (make-face 'fl-comment-face)
       (if (face-differs-from-default-p 'fl-comment-face) nil
		 (set-face-foreground 'fl-comment-face "Green")
;                (set-face-font 'fl-comment-face (face-font 'italic))
		 )

       (make-face 'fl-string-face)
       (if (face-differs-from-default-p 'fl-string-face) nil
		 (set-face-foreground 'fl-string-face "Red")
		 )
       (make-face 'fl-function-name-face)
       (if (face-differs-from-default-p 'fl-function-name-face) nil
		 (set-face-foreground 'fl-function-name-face "Purple")
		 (set-face-font 'fl-function-name-face (face-font 'bold))
		 )
       (make-face 'fl-keyword-face)
       (if (face-differs-from-default-p 'fl-keyword-face) nil
;                (set-face-font 'fl-keyword-face (face-font 'bold-italic))
		 (set-face-foreground 'fl-keyword-face "MediumSlateBlue")
		 )
	   
       (make-face 'fl-type-face)
       (if (face-differs-from-default-p 'fl-type-face) nil
;                (set-face-font 'fl-type-face (face-font 'italic))
		 (set-face-foreground 'fl-type-face "Coral")
		 )

       (make-face 'fl-member-face)
       (if (face-differs-from-default-p 'fl-member-face) nil
;                (set-face-font 'fl-member-face (face-font 'italic))
		 (set-face-foreground 'fl-member-face "Yellow")
		 )

       (make-face 'fl-constant-face)
       (if (face-differs-from-default-p 'fl-constant-face) nil
;                (set-face-font 'fl-constant-face (face-font 'italic))
		 (set-face-foreground 'fl-constant-face "Yellow")
		 )
		 
	 ))

(defun my-font-lock-init ()
  (setq font-lock-string-face 'fl-string-face)
  (setq font-lock-comment-face 'fl-comment-face)
  (setq font-lock-string-face 'fl-string-face)
  (setq font-lock-function-name-face 'fl-function-name-face)
  (setq font-lock-keyword-face 'fl-keyword-face)
  (setq font-lock-type-face 'fl-type-face)
  (setq font-lock-reference-face 'fl-type-face)
  (setq font-lock-member-face 'fl-member-face)
  (setq font-lock-constant-face 'fl-constant-face)
  (setq search-highlight t))

;;(add-hook 'font-lock-mode-hook 'my-font-lock-init)

;;; Kaiyang -- terminal mode.
(setq termtype (getenv "TERM"))         ; get our terminal type

(if (and (or (equal termtype "dialup")  ; if we are running one
	     (equal termtype "vt100")   ;  of these terminals
	     (equal termtype "vt200"))
	 (null (getenv "LAYER")))       ; ... but not layers
      (enable-flow-control))

(if (equal termtype "vt320")
    (progn
      (enable-flow-control)
      (load (concat term-file-prefix "vt200") nil t)))
