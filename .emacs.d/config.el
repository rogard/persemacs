(defcustom erw/config-capture-target "/home/erwann/src/org/capture.org" "capture target location"
  :group 'erw/config )
(defcustom erw/config-capture-template "/home/erwann/.emacs.d/capture_core_tpl" "capture template location"
  :group 'erw/config)
(defcustom erw/config-agenda-files (list (symbol-value 'erw/config-capture-target)) "agenda files"
  :group 'erw/config)
(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :straight t
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
(use-package corfu
  :straight t
  :ensure
  :demand
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 0)
  (corfu-separator ?\s)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode +1))
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        ;;          dashboard-banner-logo-title "Yay Evil!"
        dashboard-banner-logo-title "Misterwann!"
        dashboard-items nil
        dashboard-set-footer nil))
(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme ;;
   ;;   'doom-flatwhite
   ;;   'doom-homage-white
   ;;   'doom-feather-light
   ;;   'doom-one-light
   'doom-wilmersdorf t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs owners
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package ekg
  :disabled t)
(use-package emacs ;; pseudo-package
  :config
  (setq frame-title-format '("Misterwann")
        ring-bell-function 'ignore       ; minimize distraction
        frame-resize-pixelwise t
        default-directory "~/")

  (set-face-attribute 'default nil :height 180)
  
  ;; Omit default startup screen
  (setq inhibit-startup-screen t)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;; better scrolling experience
  (setq scroll-margin 0
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  )
;;	  scroll-conservatively 101 ; >100
(setq package-quickstart t)
(use-package flymake-shellcheck
  :straight t
  :ensure nil ;; built-in
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))
(use-package lsp-mode
  :straight t
  :hook ((sh-mode python-mode) . lsp-deferred)
  :commands lsp
  )
(use-package dash
  :straight t)
(use-package markdown-mode
  :straight t
  :hook (markdown-mode . visual-line-mode))

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))
(use-package org
  :straight t
  :custom
  (org-read-date-force-compatible-dates nil) ;; extends calendar
  (org-log-into-drawer t)
  (org-capture-templates
   '(("c" "Core" entry
      (file+headline (symbol-value 'erw/config-capture-target) "Capture")
      (file (symbol-value 'erw/config-capture-template)))))
  (org-refile-targets '((nil :regexp . "^:refile_bool: true$")))
  (org-agenda-files (symbol-value 'erw/config-agenda-files))
  (org-fold-core-style 'overlays) ;; https://lists.nongnu.org/archive/html/emacs-orgmode/2024-04/msg00497.html
  )
(use-package org
  :straight t
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode)))
(use-package org
  :straight t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (org . t)
     (python . t)
     (shell . t)
     (lua . t)))
  )
;; Enable vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))
;; A few more useful configurations...
(use-package emacs
  :straight t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(defconst erw/const-owner-at-hostname (concat (getenv "OWNER") "@" (system-name)))
(defcustom erw/config-owner 'erw/const-owner-at-hostname
  "constant expanding to owner"
  :type 'string
  :group 'erw/config)
