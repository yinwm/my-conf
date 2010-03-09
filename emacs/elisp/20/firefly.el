(defvar ff-window-height 10)

(defun current-line-number ()
  "Returns the current line number, such that goto-line <n> would
go to the current line."
  (save-excursion
    (let ((line 1)
	  (limit (point)))
      (goto-char 1)
      (while (search-forward "\n" limit t 40)
	(setq line (+ line 40)))
      (while (search-forward "\n" limit t 1)
	(setq line (+ line 1)))
      line)))

(defun get-empty-buffer (name)
  (let ((b (get-buffer-create name)) (inwin (selected-window)) outwin)
    (save-excursion
      (set-buffer b)
      (delete-region (point-min) (point-max))
      (setq outwin (display-buffer b))
      (select-window outwin)
      (enlarge-window (- ff-window-height (window-height)))
      (select-window inwin))
    b))

(defun save-position ()
  (cons (current-line-number) (current-column)))

(defun restore-position (state)
  (goto-line (car state))
  (move-to-column (cdr state)))

(defun ff-edit ()
  (interactive)
  (let (s
	(b (get-empty-buffer "*firefly*"))
	(f (buffer-file-name (current-buffer)))
	(state (save-position)))
    (progn
      (save-excursion
	(call-process  "sh" nil b t "-c" (concat *HBIN* "/fly/ff edit " f))
	(set-buffer b)
	(setq s (format "%s : %s" f
			(buffer-substring 1 (buffer-size)))))
      (revert-buffer t t)
      (restore-position state)
      (message s))))

(defun ff-delget (comment)
  (interactive "sComment: ")
  (let (s
	(b (get-empty-buffer "*firefly*"))
	(f (buffer-file-name (current-buffer)))
	(state (save-position)))
    (if (buffer-modified-p (current-buffer))
	(if (not (y-or-n-p (format "Save %s? " f)))
	    (progn (message "Try again when you've made up your mind")
		   (setq f nil))))
    (if f
	(progn
	  (save-excursion
	    (call-process  "sh" nil b t "-c"
			   (concat *HBIN* "/firefly/ff delget " f " -c " comment))
	    (set-buffer b)
	    (setq s (format "%s : %s" f
			    (buffer-substring 1 (buffer-size)))))
	  (revert-buffer t t)
	  (restore-position state)
	  (message s))
      (and f (message (format "%s is not under Firefly version control" f))))))

(defun ff-unedit ()
  (interactive)
  (let (s
	(f (buffer-file-name (current-buffer)))
	(state (save-position)))

    (if (y-or-n-p (format "Unedit %s? " f))
	(let ((b (get-empty-buffer "*firefly*")))
	  (if f
	      (progn
		(save-excursion
		  (call-process "sh" nil b t "-c"
				(concat *HBIN* "/firefly/ff unedit " f))
		  (set-buffer b)
		  (setq s (format "%s : %s" f
				  (buffer-substring 1 (buffer-size)))))
		(revert-buffer t t)
		(restore-position state)
		(message s))
	    (message (format "%s is not under Firefly version control" f)))))))

(defun ff-get ()
  (interactive)
  (let (s
	(f (buffer-file-name (current-buffer)))
	(state (save-position)))

    (if (y-or-n-p (format "Get %s? " f))
	(let ((b (get-empty-buffer "*firefly*")))
	  (if f
	      (progn
		(save-excursion
		  (call-process "sh" nil b t "-c"
				(concat *HBIN* "/firefly/ff get " f))
		  (set-buffer b)
		  (setq s (format "%s : %s" f
				  (buffer-substring 1 (buffer-size)))))
		(revert-buffer t t)
		(restore-position state)
		(message s))
	    (message (format "%s is not under Firefly  version control" f)))))))

(defun ff-create ()
  (interactive)
  (let (s
	(state (save-position)))
    (save-excursion
      (let ((f (buffer-file-name (current-buffer))))
	(if (y-or-n-p (format "Create %s? " f))
	    (let ((b (get-empty-buffer "*firefly*")))
	      (call-process  "sh" nil b t "-c"
			     (concat *HBIN* "/firefly/ff create " f))
	      (set-buffer b)
	      (setq s (format "%s : %s" f
			      (buffer-substring 1 (buffer-size))))))))
    (revert-buffer t t)
    (restore-position state)
    (message s)))

(defun ff-diffs ()
  (interactive)
  "Print out firefly version diffs."
  (let (s
	(f (buffer-file-name (current-buffer)))
	(b (get-empty-buffer "*firefly*")))
    (if (buffer-modified-p (current-buffer))
	(if (not (y-or-n-p (format "Save %s? " f)))
	    (progn (message "Try again when you've made up your mind")
		   (setq f nil))))
    (if f
	(call-process "sh" nil b t "-c" (concat *HBIN* "/firefly/ff diffs " f)))))

(defun ff-list-out ()
  (interactive)
  "List firefly checked out files."
  (let ((old-compile compile-command)
	(old-dir default-directory))
    (progn
      (setq default-directory (concat (getenv "TOPDIR") "\\"))
      (compile "ff list-out")
      (setq compile-command old-compile)
      (setq default-directory old-dir))))


(defun ff-history ()
  (interactive)
  (let (s
	(f (buffer-file-name (current-buffer)))
	(b (get-empty-buffer "*firefly*")))
    (call-process "sh" nil b t "-c" (concat *HBIN* "/firefly/ff hist " f))))

