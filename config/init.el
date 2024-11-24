(defgroup erw/config nil "erw's config"
  :prefix "erw/config")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Ensure Org is loaded early
(straight-use-package 'org)
(straight-use-package 'use-package)
;;  (unless (package-installed-p 'use-package)
;;    (package-refresh-contents)
;;    (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t)) 
;; (setq use-package-always-defer t))
(defcustom erw/this-directory "/home/erwann/github/rogard/persemacs/" "where this package is located"
  :group 'erw/config)
(let* ((remote-emacs-dir (expand-file-name "config" erw/this-directory)))
  (org-babel-load-file (expand-file-name "main.org" remote-emacs-dir)))
