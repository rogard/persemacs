;; init.el --- Emacs init file
;; Adapted from:
;; - https://github.com/ianyepan/yay-evil-emacs
;; https://emacs.stackexchange.com/a/76249
;; (setq package-enable-at-startup nil) 
(defgroup erw/config nil "erw's config"
  :prefix "erw/")
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
(setq use-package-always-ensure t)) 
;; (setq use-package-always-defer t))
(defcustom erw/this-directory "/home/erwann/github/rogard/persemacs/" "where this package is located"
  :group 'erw/config)
(let* ((remote-emacs-dir (expand-file-name ".emacs.d" erw/this-directory)))
  (org-babel-load-file (expand-file-name "config.org" remote-emacs-dir)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
