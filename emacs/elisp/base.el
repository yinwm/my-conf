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
