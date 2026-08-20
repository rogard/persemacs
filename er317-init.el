;;; erw1-init.el --- Emacs init (source) -*- lexical-binding: t; -*-
;;; Copyright (C) 2024-26 Erwann Rogard
;;; Released under GPL 3.0
;;; See https://www.gnu.org/licenses/gpl-3.0.en.html
;;;
;;; Commentary:
;;; - sets up straight
;;; - delegates to config.el extra.el
;;;
;;; Code:

;; https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started
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

;; This makes :straight t
;; - redundant in use-package declarations
;; - *except* for non-standard sources
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package use-package)

(let ((config-path '"/home/erwann/github/rogard/persemacs/er317-config.el")
      (extra-path '"/home/erwann/github/rogard/persemacs/er317-extra.el"))
(let ((dir-name default-directory))
  (load (expand-file-name config-path dir-name))
  (load (expand-file-name extra-path dir-name)))
)

(let ((init-source '"/home/erwann/github/rogard/persemacs/er317-init.el"))
(eval-after-load init-source
  (makunbound 'er317/resolve-entry))
)

(provide 'er317-init.el)
;;; er317-init.el ends here
