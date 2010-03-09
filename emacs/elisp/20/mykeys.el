;;; Kaiyang -- customerize your key binding here.
(message "Loading my key bindings.")

(global-set-key "\M-g" 'goto-line)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [C-up] 'previous-line)
(global-set-key [C-down] 'next-line)

(cond (window-system
       (global-set-key [C-down-mouse-2] 'facemenu-menu)
;;;       (global-set-key [C-down-mouse-3] 'function-menu)))
       (global-set-key [C-down-mouse-3] 'imenu)))       

; File a file.
(global-set-key "\C-x\C-f" 'find-file)
(global-set-key "\C-xf" 'find-file)

(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-xk" 'kill-buffer)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-xS" 'run-scheme)
(global-set-key "\C-xs" 'save-buffer)

(global-set-key [C-kp-down] 'next-line)
(global-set-key [C-kp-up] 'previous-line)

;;; Kaiyang -- a better grep than unix default one
(global-set-key [(f4)] 'grep)
;;; Kaiyang -- show speedbar
(global-set-key [(f5)] 'speedbar-get-focus)

;;; Kaiyang -- firefly command, only 'edit' I actually use.
(global-set-key [(f6)] 'ff-edit)
(global-set-key [(f7)] 'ff-delget)

;;; numeric keypad
(global-set-key [(f27)] 'beginning-of-line)
(global-set-key [(f29)] 'scroll-down)
(global-set-key [(f31)] 'recenter)
(global-set-key [(f33)] 'end-of-line)
(global-set-key [(f35)] 'scroll-up)

