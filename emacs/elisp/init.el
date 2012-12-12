;;; initialization for emacs

;;; set frame properties.
(setq initial-frame-alist
      (append
       '((auto-raise . nil)
	 (cursor-color . "red")
	 (cursor-type . line)
	 (foreground-color . "ivory")
	 (background-color . "black")
	 )
       initial-frame-alist))

;; add goto-line
(global-unset-key "\M-gd"  )
(global-unset-key "\M-gb"  )
(global-unset-key "\M-gi"  )
(global-unset-key "\M-gl"  )
(global-unset-key "\M-gu"  )
(global-unset-key "\M-go"  )
(global-set-key "\M-g"  'goto-line)
(global-set-key [C-tab]  'dabbrev-expand)
(global-set-key [C-M-tab]  'dabbrev-completion)
(global-set-key [S-tab]  'tab-to-tab-stop)
;(global-set-key [tab] ')


(global-font-lock-mode t)
(transient-mark-mode t)
(hi-lock-mode t)

;; no menu bar
(menu-bar-mode nil)

(load "modes")

(setq load-path
      (append load-path
	      '("~/elisp/python-mode-1.0/")))

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(cond (window-system
       (setq hilit-mode-enable-list  '(not text-mode)
             hilit-background-mode   'light
             hilit-inhibit-hooks     nil
             hilit-inhibit-rebinding nil)

       (require 'hilit19)
       ))

     (hilit-translate type     'RoyalBlue   ; enable highlighting in C/C++
		 string	  nil)         ; disable string highlighting

;(autoload 'javascript-mode "javascript" nil t)
;(setq auto-mode-alist (cons '("\\.js$" . javascript-mode) auto-mode-alist))

