(setq load-path
      (append load-path
        '("~/elisp")))

(load "base.el")
(load "tags.el")



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(load-home-init-file t t)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; used for python
(setq load-path
      (append load-path
	      '("~/elisp/python-mode-1.0/")))

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)


;; javascript
;; (autoload 'js-mode "javascript" nil t)
;;  (setq auto-mode-alist (cons '("\\.js$" . js-mode) auto-mode-alist))


;; line number
(require 'wb-line-number)

(load "modes")

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(modify-frame-parameters nil '((wait-for-wm . nil)))