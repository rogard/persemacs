;; init.el --- Emacs init file
;; Attribution:
;; - https://github.com/ianyepan/yay-evil-emacs
;; (setq package-enable-at-startup nil) ;; not sure about this
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
;; (package-initialize) ;; Emacs calls package-initialize before evaluating your init file (since 27.1)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; workaround bug in Emacs 26.2
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
(setq use-package-always-ensure t)) 
;; (setq use-package-always-defer t))
;; I tried default-directory, but ...
(defcustom erw/routinel-directory "/home/erwann/github/rogard/routinel/" "where this package is located")
(message "debug: %s" erw/routinel-directory)
(let* ((remote-emacs-dir (expand-file-name ".emacs.d" erw/routinel-directory)))
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
