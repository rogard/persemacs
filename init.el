;; init.el --- Emacs init file
;; Attribution:
;; - https://github.com/ianyepan/yay-evil-emacs
;; (setq package-enable-at-startup nil) ;; moved to early-init.el

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
;; (package-initialize) ;; Emacs calls package-initialize before evaluating your init file (since 27.1)

;; workaround bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
(setq use-package-always-ensure t)) 
;; (setq use-package-always-defer t))

(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init)
