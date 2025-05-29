;; persemacs — Emacs configuration and functionality
;; Copyright (C) 2024—2025 — Erwann Rogard
;; Released under GPL 3.0
;; See https://www.gnu.org/licenses/gpl-3.0.en.html

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
					      'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(straight-use-package 'org)
(eval-and-compile
  (setq use-package-always-ensure t))
;; (setq use-package-always-defer t))

(let ((meta-delegate-list '((:file "../org/config.org" :lang "emacs-lisp"))))
  (let ((dir-name (file-name-directory
		   (or (file-truename load-file-name) buffer-file-name))))
    (dolist (pair meta-delegate-list)
      (let ((source-file (plist-get pair :file))
	  (source-lang (plist-get pair :lang)))
      (with-current-buffer (find-file-noselect (expand-file-name source-file dir-name))
	(message "Visiting buffer %s" (buffer-name))
	(let ((target-list (org-babel-tangle nil nil source-lang)))
	  (dolist (target-file target-list)
	    (load-file target-file)))))))
)
