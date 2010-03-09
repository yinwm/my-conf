;; horrible debug hacks - i wish i knew better!

(defvar trace-start "// AUTO-TRACE")
(defvar trace-end "// END AUTO-TRACE")

(defvar java-func-regexp
  (concat
   "^    [a-zA-Z0-9]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"	; more than 5 tokens, right?
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"   
   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; name
   ))

(defun java-line (s)
  (interactive "sLine: ")
  (newline)
  (previous-line 1)
  (insert (concat " " s))
  (c-indent-command)
  (forward-line 1))

(defun java-println (s)
  (java-line (concat "System.out.println(" s ");")))

(defun java-debug-line (s)
  (java-line "if (DEBUG) {")
  (java-println s)
  (java-line "}"))

(defun next-java-function ()
  (interactive)
  (re-search-forward java-func-regexp nil t))

(defun trace-java-function (prefix)
  (let ((prev) (next) (func) (str nil))
    (setq next (- (point) 1))
    (backward-word 1)
    (setq func (buffer-substring (point) next))
    (forward-word 1)
    (skip-chars-forward " ")
    (forward-char 1)
    (while (not (eq (following-char) ?\)))
      (progn
	(forward-word 1)
	(forward-char 1)
	(setq next (point))
	(forward-word 1)
	(setq str (concat (if (eq str nil)
			      "'\""
			    (concat str " + \"','\""))
			  " + "
			  (buffer-substring (point) next)))
	(skip-chars-forward "[] ")))
    (setq str (concat func "("
		      (if (eq str nil)
			  ")"
			(concat str " + \"')"))))
    (search-forward "{")
    (forward-line 1)
    (if (looking-at "[ \t]+super(")
	(forward-line 1))
    (if (looking-at "[ \t]+this(")
	(forward-line 1))
    (beginning-of-line)
    (java-line trace-start)
    (message func)
    (java-println (concat "\"" prefix str "\""))
    (java-line trace-end)))
    
    

(defun trace-java-buffer (s)
  (interactive "sTrace prefix: ")
  (beginning-of-buffer)
  (while (next-java-function)
    (trace-java-function s)))

(defun untrace-java-buffer ()
  (interactive)
  (beginning-of-buffer)  
  (while (search-forward trace-start nil t)
    (beginning-of-line)
    (let ((beg (point)))
      (re-search-forward trace-end nil t)
      (forward-line 1)
      (if (> (count-lines beg (point)) 5)
	  (progn
	    (search-backward trace-start nil t)
	    (error "Trace appears to be corrupted (> 5 lines)")))
      (delete-region beg (point)))))

(defun echo-var (var)
  (interactive "sVariable: ")
  (java-println (concat "\"" var " = '\" + " var " + \"'\"")))

;;; Kaiyang -- add a default copyright header for your class
(defun hansky-file-header ()
  (interactive)
  (beginning-of-buffer)
  (java-line "// Copyright 2000-2003, Hansky Inc. All Rights Reserved.")
  (java-line "// Confidential and Proprietary Information of Hansky, Inc.")
  (java-line "// %Z%%M%, %I%, %G%"))

;;; Kaiyang -- add a default header for your class
(defun hansky-class-header ()
  (interactive)
  (beginning-of-buffer)  
  (search-forward-regexp " class \\| interface ")
  (beginning-of-line)
  (java-line "/**")
  (java-line "*")
  (java-line "*")
  (java-line "* @author	Kaiyang Liu")
  (java-line "* @version	%I%, %G%")
  (java-line "*/"))



