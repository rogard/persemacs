(defgroup erw/config nil "erw's config"
  :prefix "erw/config")
(defcustom erw/config-install-url
  "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
  "URL to download the straight.el bootstrap script."
  :group 'erw/config)
(defcustom erw/config-manager-version
  7
  "Bootstrap version for straight.el."
  :group 'erw/config)
(let ((bootstrap-file
       (expand-file-name
        erw/config-bootstrap-relative-path
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version erw/config-manager-version))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         erw/config-install-url
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
(dolist (file '("shared.org" "package.org"))
  (org-babel-load-file (expand-file-name file)))
