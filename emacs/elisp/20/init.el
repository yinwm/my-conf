;;; initialization for emacs

;;; set frame properties.
(setq initial-frame-alist
      (append
       '((auto-raise . nil)
	 (top . 10)
	 (left . 359)
	 (width . 90)
	 (height . 45)
	 (cursor-color . "red")
	 (cursor-type . box)
	 (foreground-color . "ivory")
	 (background-color . "black")
	 )
       initial-frame-alist))

(if *USE-HELLS*
    (setq initial-frame-alist
      (append
       '((font . "-*-Hells Programmer-normal-r-*-*-11-82-*-*-c-*-*-ansi-"))
       initial-frame-alist)))

;;; other frame appearance
(setq default-frame-alist initial-frame-alist)

(defun my-emacs-startup ()
  (setq auto-save-list-file-name nil)
  )
(add-hook 'emacs-startup-hook 'my-emacs-startup)

(defun memequal (el list)
  "[Jak] Returns non-nil if ELT is an element of LIST.  Comparison
done with EQUAL.  The value is actually the tail of LIST whose car is ELT."
  (let ((res nil))
    (while list
      (if (equal el (car list))
          (progn
            (setq res (cdr list))
            (setq list nil)))
      (setq list (cdr list)))
    res))

(defmacro append-no-dup (el list)
  "[Jak] Macro: adds EL to the beginning of LIST if it is not already there."
  (`(if (not (memequal (, el) (, list)))
       (setq (, list) (append (, list) (list (, el)))))))

(defmacro prepend-no-dup (el list)
  "[Jak] Macro: adds EL to the beginning of LIST if it is not already there."
  (`(if (not (memequal (, el) (, list)))
       (setq (, list) (cons (, el) (, list))))))

;;; Set up our load path
(prepend-no-dup (format "%s/elisp/20" *HOME*) load-path)
(prepend-no-dup (format "%s/elisp/20/emacslib" *HOME*) load-path)
(prepend-no-dup (format "%s/elisp/20/python" *HOME*) load-path)

;;; turn off audio bell
(setq visible-bell t)
(defun my-dummy-ring-bell-function ()
   nil)
 (setq ring-bell-function 'my-dummy-ring-bell-function)

;;; wrap too-long lines in half windows
(setq truncate-partial-width-windows nil)

;;; some defaults
(setq text-mode-hook 'turn-on-auto-fill)
(setq line-number-mode t)
(setq delete-auto-save-files t)

(setq require-final-newline 'ask)
(setq compile-command '"make -k")
(setq revert-without-query (list "\\.txt\\'" "\\.java\\'"))

(defun mode-line-resize-dynamically ()
  "Resize a window by dragging the mode-line.
This must be bound to a mouse-down event in the mode-line."
  (interactive "@")
  (let* ((mouse (mouse-position))
	 (start-frame (car mouse))
	 (prev-y (cdr (cdr mouse)))
	 (next (next-window)))
    (if (window-at 0 (+ 1 prev-y)) nil
      (split-window (selected-window) (- (window-height) 4))
      (setq next (next-window)))
    (track-mouse
      (while (and (eq (car-safe (read-event)) 'mouse-movement)
		  (eq next (next-window)))
	(let* ((mouse (mouse-position))
	       (frame (car mouse))
	       (new-y (cdr (cdr mouse)))
	       (delta (- new-y prev-y)))
	  (cond ((and (eq frame start-frame)
		      (> (+ delta (window-height (selected-window)))
			 window-min-height))
		 (enlarge-window delta)
		 (setq prev-y new-y))))))))

;;; Set up our tabs in a cool way
(setq default-tab-width 8)
(setq tab-stop-list '( 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 ))

;;; some look and feel stuff
;(scroll-bar-mode -1)
(cond (window-system
       (progn
	 (transient-mark-mode 1)
	 
	 (setq w32-enable-italics t)
	 ; you can change the font here...
	 (if *USE-HELLS*	 
	     (set-default-font "-*-Hells Programmer-normal-r-*-*-11-82-*-*-c-*-*-ansi-")
	   ;;(set-default-font "-adobe-courier-medium-r-normal--10-120-75-75-m-70-iso8859-1"))
	   ;;(set-default-font "-*-Lucida Console-normal-r-*-*-14-90-*-*-c-*-*-ansi-"))
	 (set-default-font "-*-Courier New-normal-r-*-*-12-97-*-*-c-*-*-ansi-"))
	 (menu-bar-mode 1)
	 
	 ;; set some colors
	 (set-face-foreground 'modeline "black")
	 (set-face-background 'modeline "ivory")	 	 
	 (set-face-background 'secondary-selection "OrangeRed")
       
	 ;; set the selection face
	 (copy-face 'bold-italic 'selection-face)
	 (setq selection-face 'selection-face)
	 (set-face-background 'region "DarkSlateGray")
	 (set-face-background 'selection-face "DarkSlateGray")
	 (overlay-put mouse-drag-overlay 'face 'selection-face))
       (setq font-lock-face-attributes
;              face                              fg        bg  bf  it  ul 
	     '((font-lock-comment-face       "turquoise"   nil nil nil nil)
	       (font-lock-doc-string-face    "khaki"       nil nil nil nil)
	       ;(font-lock-string-face        "bisque"      nil nil  t  nil)
	       (font-lock-string-face        "burlywood"      nil nil nil  nil)
	       (font-lock-keyword-face       "salmon"     nil  t  nil nil)
	       (font-lock-type-face          "SpringGreen"   nil  t  nil nil)
	       (font-lock-function-name-face "Cyan"    nil  t  nil nil)
	       (font-lock-variable-name-face "burlywood"   nil  t  nil nil)
	       (font-lock-reference-face     "beige"     nil nil  nil  t )
	       ;;(font-lock-constant-face      "Magenta"     nil nil  t   nil ))
	       (font-lock-constant-face      "Magenta"     nil nil  nil  nil ))
	     ;font-lock-maximum-decoration t
	     ;font-lock-support-mode 'lazy-lock-mode
	     ;font-lock-verbose nil
	     ;lazy-lock-stealth-verbose nil
	     )))

;;; font lock mode
(global-font-lock-mode t)
(setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-maximum-decoration t)

;; Complicated regexp to match method declarations in interfaces or classes
;; A nasty test case is:
;;    else if(foo instanceof bar) {
;; To avoid matching this as a method named "if" must check that within
;; a parameter list there are an even number of symbols, i.e., one type name
;; paired with one variable name.  The trick there is to use the regexp
;; patterns \< and \> to match beginning and end of words.
(defvar java-function-regexp
  (concat
   "^[ \t]*"                                   ; leading white space
   "\\(public\\|private\\|protected\\|"        ; some of these 8 keywords
   "abstract\\|final\\|static\\|"
   "synchronized\\|native"
   "\\|[ \t\n\r]\\)*"                          ; or whitespace
   "[a-zA-Z0-9_$]+"                            ; return type
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\([a-zA-Z0-9_$]+\\)"                      ; the name we want!
   "[ \t\n\r]*"                                ; optional whitespace
   "("                                         ; open the param list
   "\\([ \t\n\r]*"                             ; optional whitespace
   "\\<[a-zA-Z0-9_$]+\\>"                      ; typename
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\<[a-zA-Z0-9_$]+\\>"                      ; variable name
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]*,?\\)*"                          ; opt whitespace and comma
   "[ \t\n\r]*"                                ; optional whitespace
   ")"                                         ; end the param list
   "[ \t\n\r]*"                                ; whitespace
;   "\\(throws\\([, \t\n\r]\\|[a-zA-Z0-9_$]\\)+\\)?{"
   "\\(throws[^{;]+\\)?"                       ; optional exceptions
   "[;{]"                                      ; ending ';' (interfaces) or '{'
   )
  "Matches method names in java code, select match 2")

(defvar java-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in java code, select match 2")

(defvar java-interface-regexp
  "^[ \t\n\r]*\\(abstract\\|public\\|[ \t\n\r]\\)*interface[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;]*;"
  "Matches interface names in java code, select match 2")

(defvar java-imenu-regexp
  (list (list nil java-function-regexp 2)
        (list ".CLASSES." java-class-regexp 2)
        (list ".INTERFACES." java-interface-regexp 2))
  "Imenu expression for Java")

;; install it
(add-hook 'java-mode-hook
          (function (lambda ()
		      (setq imenu-sort-function 'imenu--sort-by-name)
                      (setq imenu-generic-expression java-imenu-regexp))))

(require 'generic)
(define-generic-mode 'properties-generic-mode
   (list ?\#)
   nil
  '(("^\\([\\.A-Za-z0-9_]+\\)\\(\\s-+\\|\\(\\s-*=\\s-*\\)\\)\\([^\r\n]*\\)$" 
     (1 font-lock-reference-face) (4 font-lock-variable-name-face)))
   (list "properties\\.txt\\'")
   (list 
    (function
     (lambda ()
       (setq imenu-generic-expression 
	     '((nil "^\\s-*\\([^=]+\\)\\s-*=" 1)))
       )))
   "Generic mode for properties.txt files." t)

;; gnuserv
;;(if *USE-GNUSERV*
;;    (progn
;;      (require 'gnuserv)
;;      (gnuserv-start)))
;;
;;(setq gnuserv-frame (selected-frame))

;;; load other elisp modules.
(load "ishl")
(if *USE-SUN-JAVA*
    (load "sun-java"))
(load "modes")
(load "trace")
(load "complete")
(load "firefly")
(load "mykeys")
(load "wb-line-number")
(load "python-mode")
(load "folding")
;;(load "hilit-java")

(setq gc-cons-threshold 200000)
(setq focus-follow-mouse nil)
(setq initial-scratch-message nil)
(partial-completion-mode t)

; modeline
;(setq display-time-format "%a %b %e ")
(setq display-time-mode nil)

(setq tack-eol t)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces)

(setq PC-word-delimiters "-_.")

(setq default-major-mode 'text-mode)
(ishl-mode)
(setq backup-by-copying t)
(setq compilation-window-height 20)
(setq frame-title-format "Emacs - %f")
(setq speedbar-track-mouse-flag nil)

; remove ^M from shells
;(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
; stop echoing, shell!
(setq comint-process-echoes t)

;;; Kaiyang -- set viper mode for who addict to vi
;(setq viper-mode t)
;(require 'viper)
;(global-set-key [S-iso-lefttab] 'viper-insert-tab)

;;; Kaiyang -- CUA mode to prive SHIFT, Ctrl-C, Ctrl-V for copy/paste...
;;(require 'cua-mode)
;;(CUA-mode t)

;;; Kaiyang -- added some major mode binding.
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tpl$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.js$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htc$" . java-mode) auto-mode-alist))

;; add for csharp mode
(setq auto-mode-alist (cons '("\\.cs$" . java-mode) auto-mode-alist))

;;
(autoload 'folding-mode          "folding" "Folding mode" t)
;;
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;;
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "Chinese-GB")
 '(default-input-method "chinese-py-punct")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
