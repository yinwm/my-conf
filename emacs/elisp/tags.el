(setq tags-file-name "D:\\jdk\\jdk15\\src\\JDK15_ETAGS")

; find . -name "*.[chCH]" -print | etags -
;    * F7, 查找 TAGS 文件（更新 TAGS 表）
;    * C-F7, 在当前目录下生成包含所有递归子目录的 TAGS 文件（使用了shell中的find命令）
;    * C-. 开个小窗查看光标处的 tag
;    * C-, 只留下当前查看代码的窗口（关闭查看 tag 的小窗）
;    * M-. 查找光标处的 tag，并跳转
;    * M-, 跳回原来查找 tag 的地方
;    * C-M-, 提示要查找的 tag，并跳转
;    * C-M-. 要匹配的 tag 表达式（系统已定义）
;    * Shift-Tab, C/C++ 和 lisp 等模式中补全函数名（一般情况下M-Tab被窗口管理器遮屏了）

(global-set-key [(f1)] 'find-tag)
(global-set-key [(M-f1)] 'visit-tags-table)         ; visit(open) tags table
;(global-set-key [C-f7] 'sucha-generate-tag-table) ; generate tag table
(global-set-key [(f2)] '(lambda () (interactive) (lev/find-tag t)))
(global-set-key [(M-f2)] 'sucha-release-small-tag-window)
(global-set-key [(f3)] 'lev/find-tag)
(global-set-key [(M-f3)] 'pop-tag-mark)

;(define-key lisp-mode-shared-map [(shift tab)] 'complete-tag)
;(add-hook 'c-mode-common-hook      ; both c and c++ mode
;          (lambda ()
;            (define-key c-mode-base-map [(shift tab)] 'complete-tag)))

(defun lev/find-tag (&optional show-only)
  "Show tag in other window with no prompt in minibuf."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if show-only
        (progn (find-tag-other-window default)
               (shrink-window (- (window-height) 12)) ;; allow 12 rows
               (recenter 1)
               (other-window 1))
      (find-tag default))))

(defun sucha-generate-tag-table ()
  "Generate tag tables under current directory(Linux)."
  (interactive)
  (let
      ((exp "")
       (dir ""))
    (setq dir
          (read-from-minibuffer "generate tags in: " default-directory)
          exp
          (read-from-minibuffer "suffix: "))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" exp "\" | xargs etags ")
       (buffer-name)))))

(defun sucha-release-small-tag-window ()
  "Kill other window also pop tag mark."
  (interactive)
  (delete-other-windows)
  (ignore-errors
    (pop-tag-mark)))
