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

(add-to-list 'load-path "~/elisp/color-theme-6.6.0")

(defun py_color ()
  (interactive)
  (color-theme-install
   '(py_color
      ((background-color . "#101e2e")
      (background-mode . light)
      (border-color . "#1a1a1a")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "black"))
     (fringe ((t (:background "#1a1a1a"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#0d4519"))))
     (font-lock-builtin-face ((t (:foreground "#729fcf"))))
     (font-lock-comment-face ((t (:foreground "#888a85"))))
     (font-lock-function-name-face ((t (:foreground "#edd400"))))
     (font-lock-keyword-face ((t (:foreground "#729fcf"))))
     (font-lock-string-face ((t (:foreground "#ad7fa8"))))
     (font-lock-type-face ((t (:foreground"#8ae234"))))
     (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(provide 'py_color)

(require 'color-theme)
(color-theme-initialize)
(py_color)


(mouse-avoidance-mode 'animate)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(tool-bar-mode nil)
(column-number-mode 1)

;; add goto-line
(global-unset-key "\M-gd"  )
(global-unset-key "\M-gb"  )
(global-unset-key "\M-gi"  )
(global-unset-key "\M-gl"  )
(global-unset-key "\M-gu"  )
(global-unset-key "\M-go"  )
(global-set-key "\M-g"  'goto-line)
(global-set-key "\M-o"  'switch-to-buffer)
(global-set-key "\C-o"  'other-window)
(global-set-key [C-tab]  'dabbrev-expand)
(global-set-key [C-M-tab]  'dabbrev-completion)
(global-set-key [S-tab]  'tab-to-tab-stop)
(global-set-key [backtab]  'tab-to-tab-stop)
;(global-set-key [tab] ')


(global-font-lock-mode t)
(transient-mark-mode t)
(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

;; no menu bar
(menu-bar-mode nil)

(setq indent-tabs-mode nil)
(set-default 'tab-width 4)
(setq tab-width 4)
(setq default-tab-width 4)

(setq-default tab-width 4 indent-tabs-mode nil)

(cond (window-system
       (setq hilit-mode-enable-list  '(not text-mode)
             hilit-background-mode   'light
             hilit-inhibit-hooks     nil
             hilit-inhibit-rebinding nil)

))


;;(require 'hilit19)


;;(hilit-translate type     'RoyalBlue   ; enable highlighting in C/C++
;;	 string	  nil)         ; disable string highlighting


(setq font-lock-maximum-decoration t)

(require 'php-mode)
(add-hook 'php-mode-user-hook 'turn-on-font-lock)
(add-hook 'php-mode-user-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

(autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
(add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))

(require 'rst)

(require 'tramp)
(setq tramp-default-method "ssh")

(set-default-font "Dejavu Sans Mono")

; (autoload 'js2-mode "js2" nil t)
; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; (autoload 'javascript-mode "javascript" nil t)
; (setq auto-mode-alist (cons '("\\.js$" . javascript-mode) auto-mode-alist))

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(require 'ahg)
(require 'mercurial)


(add-to-list 'load-path "~/elisp/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

(add-to-list 'load-path
              "~/elisp/yasnippet-0.6.1c")

(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/elisp/yasnippet-0.6.1c/snippets")

(add-to-list 'load-path
             "~/elisp/scala-mode")
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
      (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(setq yas/my-directory "~/elisp/scala-mode/contrib/yasnippet/snippets")
(yas/load-directory yas/my-directory)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

 (add-hook 'yaml-mode-hook
      '(lambda ()
         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))