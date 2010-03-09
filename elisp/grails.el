;;;
;;----------------------------------------------------------------------
;; grail.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; License: LGPL-v3
;;----------------------------------------------------------------------

;; Grail is an attempt to seek the Holy Grail of Emacs Theory:
;; unification of all the various essential bits of elisp in a sane
;; deployment.
;;
;; Grail on a more basic level is a grand load-path hack for the Elisp
;; coder who can't leave anything alone.
;;
;; This file serves as the entry-point for a Emacs configuration. You
;; should link .emacs to this file at the root of the directory
;; grail-elisp-root.

(defconst grail-release-version "0.0.2"
  "the release number of grail.el")

(defvar grail-elisp-root
  (or (getenv "USER_ELISP") (concat (getenv "HOME") "/system/emacs/"))
  "The root of the user's elisp tree")

;; This code will assume a FS structure like this:
;;
;; $HOME/.emacs.d/            | all of emacs scribbles here by default so I treat
;;                              it like /var
;;
;;  user-elisp-root           | all of my emacs customization and elisp. I keep
;;                              it outside of .emacs.d and under version control
;;                              so that emacs session state does not mix with
;;                              source which is very different in lifetime and
;;                              content management.

;; all of the following paths are relative to user-elisp-root. Files that
;; do not exist are silently ignored.

;; grail.el                   | entry point for emacs startup and phase #1
;;                              of the configuration.
;;
;; grail-fn.el                | library of functions essential to phase #1
;;
;; elisp.el                   | user elisp functions, should not be (interactive) only:
;;                            | loaded by --script

;; user.el                    | user customization of Emacs.

;; commands.el                | user commands only loaded in interactive Emacs
;; keys.el                    | user key-binding customization
;; interface.el               | modify the mainline Emacs interface, gui agnostic.
;; gui.el                     | only loaded with a window-system

;; linux.el                   | only loaded on gnu/linux.
;; darwin.el                  | only loaded on darwin.
;;
;; local/(*)                  | elisp maintained by the user.
;;      emacs/                | local elisp that modifies or replaces packages/files distributed
;;                              with the mainline.
;;      elisp/                | elisp maintained by the user that complements the mainline.
;;
;;      patches/              | patches against distributed emacs files required
;;                              by my config.
;;      styles/               | modules that combine loading/deploying required packages
;;                              with configuration that blends those packages into a
;;                              harmonious "style" of using Emacs.

;; dist/(*)
;;     elisp/                 | elisp maintained and distributed by a Third Party.
;;                              Usually this is for projects you contribute to where
;;                              you want to keep a VCS checkout.
;;     elpa/                  | elisp maintained by ELPA.

;; The files user-elisp-root are not in the load-path and must be loaded with explicit paths.

(defvar grail-local-dir
  (concat grail-elisp-root "local/")
  "The directory containing the user's local modifications to emacs
   and elisp.

   grail-local-emacs and grail-local-elisp are the preferred
   variables for accessing user specific elisp paths.")

(defvar grail-local-emacs
  (concat grail-local-dir "emacs/")
  "The directory containing Emacs packages that over-ride the packages
   distributed with Emacs.")

(defvar grail-local-elisp
  (concat grail-local-dir "elisp/")
  "The directory containing Emacs libraries created and maintained by the
   user.")

(defvar grail-local-styles
  (concat grail-local-dir "styles/")
  "The directory containing Emacs style modules.")

(defvar grail-dist-dir
  (concat grail-elisp-root "dist/"))

(defvar grail-dist-elisp
  (concat grail-dist-dir "elisp/")
  "The directory containing third-party elisp extensions of Emacs.")

(defvar grail-dist-elpa
  (concat grail-dist-dir "elpa/")
  "ELPA managed third party elisp.")

(require 'cl)

;; FIXME: need setter functions when paths need to be re-computed as
;;        side-effects.

;;----------------------------------------------------------------------
;; define a robust loading command.
;;----------------------------------------------------------------------

(defun robust-load-elisp ( path )
  (condition-case nil
    (load path)
    (error (progn
             ;; duplicate the message to both *Messages* as a log
             ;; and to the *scratch* buffer where it is highly visible.
             (message "grail.el: errors loading %s\n" path)
             (with-current-buffer "*scratch*"
               (goto-char (point-max))
               (insert (format "; grail.el: errors loading %s\n" path)))
             nil)) ))

(defun file-if-readable ( file )
  "if file is a readable path return file or nil"
  (if (file-readable-p file)
    file))

(defun load-elisp-if-exists ( path )
  (lexical-let
    ((accessible-path  (file-if-readable path)))

    (when accessible-path
      (robust-load-elisp accessible-path)) ))

(defun load-user-elisp ( path )
  (when path
    (load-elisp-if-exists (concat grail-elisp-root path))))

;;----------------------------------------------------------------------
;; load the rest of the loader, and any customizations.
;;----------------------------------------------------------------------

(load-user-elisp "grail-fn.el")         ;; library used by the loader.
(load-user-elisp "grail-cfg.el")        ;; file for user to change paths

(setq load-filter-dot-dirs "^\\.")

(let
  ((extended-load-path
     (condition-case nil
       (apply 'append
         (seq-filter-nil
           ;; overide distributed elisp with local modifications by
           ;; inserting a "local" directory at the beginning of the
           ;; load list
           (if (file-accessible-directory-p grail-local-emacs)
             (list grail-local-emacs))

           load-path

           (if (file-accessible-directory-p grail-local-elisp)
             (cons grail-local-elisp
               (filter-ls grail-local-elisp t
                 (type ?d)
                 (!name load-filter-dot-dirs))))

           (if (file-accessible-directory-p grail-dist-elisp)
             (cons grail-dist-elisp
               (filter-ls grail-dist-elisp t
                 (type ?d)
                 (!name load-filter-dot-dirs))))))

       (error nil)) ))

  (if (and extended-load-path (listp extended-load-path))
    (setq load-path extended-load-path)
    (message "grail.el: unable to form an extended load-path. check for problems loading grail-fn.el.") ))

;;----------------------------------------------------------------------
;; Host specific adaptation
;;
;; each host system has a site file that normalizes the platform
;; and extends the library search space for extra libraries it manages.
;;----------------------------------------------------------------------
(load-user-elisp
  (cond
    ((string-equal "gnu/linux" system-type)  "linux.el")
    ((string-equal "darwin"    system-type)  "darwin.el")))

(load-elpa-when-installed)

(load-user-elisp "elisp.el")

;; Annoying Emacs.app 0.9-rc2 compat.
(unless (functionp 'window-system)
  (defun window-system ()
    "grail.el replacement for window system function."
    window-system))

(unless noninteractive
  ;; only loaded when there is an active terminal.
  (load-user-elisp "keys.el")
  (load-user-elisp "commands.el")
  (load-user-elisp "interface.el")

  (load-user-elisp "user.el")

  (if (window-system)
    (load-user-elisp "gui.el"))

  (load-requested-styles))
