;; erw1-config.el --- Packages' configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2024—2026 — Erwann Rogard
;; Released under GPL 3.0
;; See https://www.gnu.org/licenses/gpl-3.0.en.html

;;; Commentary:
;; Initially adapted from:
;; - https://github.com/ianyepan/yay-evil-emacs
;;; Code:

;; ── auctex ───────────────────────────────────────────────────────────────────

;; https://emacs.stackexchange.com/a/81504/41724
(use-package auctex)

;; ── citar ────────────────────────────────────────────────────────────────────

(use-package citar
  ;; :no-require
  :after (org bibtex)
  :custom
  (org-cite-global-bibliography '("~/src/bib/academ.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/src/org/")))
;; optional: org-cite-insert is also bound to C-c C-x C-@
;;    :bind
;;    (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

;; ── corfu ────────────────────────────────────────────────────────────────────

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode +1)
  
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 0)
  (corfu-separator ?\s)
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package emacs ; pseudo package
  :init
  (column-number-mode 1)
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate
   #'command-completion-default-include-p))

(use-package emacs
  :init
  ;; CRM indicator for completing-read-multiple
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple
              :filter-args
              #'crm-indicator)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  (enable-recursive-minibuffers t)
  (read-extended-command-predicate
   #'command-completion-default-include-p))

;; ── dash ─────────────────────────────────────────────────────────────────────

(use-package dash
  :disabled)

;; ── dashboard ────────────────────────────────────────────────────────────────

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)

  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title "Misterwann!")
  (dashboard-items nil)
  (dashboard-set-footer nil))

;; ── doomemacs ────────────────────────────────────────────────────────────────

(use-package doom-themes
  :init
  (load-theme 'doom-wilmersdorf t)

  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")

  :config
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  (set-face-background 'show-paren-match "yellow")
  (set-face-foreground 'show-paren-match "black"))

;; ── ekg ──────────────────────────────────────────────────────────────────────

(use-package ekg
  :disabled)

;; ── emacs ────────────────────────────────────────────────────────────────────

(let ((trash-path '"/home/erwann/.local/share/Trash"))
;; :init
;; → made irrelevant by straight
;; (setq package-quickstart t)
(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (ring-bell-function #'ignore)
  (frame-resize-pixelwise t)

  (scroll-margin 0)
  (scroll-conservatively 101) ; >100 avoids recentering
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)

  (confirm-kill-processes nil)
  (create-lockfiles nil)      ; avoids .# files (can break npm workflows)
  (make-backup-files nil)
  (trash-directory trash-path) ; trash-path defined in foonote

  :config
  (setq frame-title-format '("Misterwann")
	default-directory "~/")

  (set-face-attribute 'default nil :height 180)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
)

;; ── flymake ──────────────────────────────────────────────────────────────────

(use-package flymake-shellcheck
  :disabled ; lsp overlap
  ;; :hook (sh-mode . flymake-shellcheck-load))
  )

;;; M-x flycheck-select-checker
(use-package flycheck
:init (global-flycheck-mode))

;; ── lsp-mode ─────────────────────────────────────────────────────────────────

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (sh-mode . lsp-deferred))
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-diagnostics-provider :auto))

(use-package lsp-pyright
  :after lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.2))

(use-package pipenv
:hook (python-mode . pipenv-mode))

;; ── elisp ────────────────────────────────────────────────────────────────────

;; Core Emacs Lisp editing enhancements
(use-package emacs ; pseudo package
  :hook
  ((emacs-lisp-mode . eldoc-mode)
   (emacs-lisp-mode . flymake-mode)))

;; Better navigation to definitions
(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))

;; Macro debugging helpers
(use-package macrostep
  :commands macrostep-expand)

;; Built-in debugging helpers (kept for reference)
;; M-x toggle-debug-on-error
;; M-x edebug-defun

;; ── markdown ─────────────────────────────────────────────────────────────────

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))

  :custom
  (web-mode-markup-indent-offset 2)  ; HTML
  (web-mode-css-indent-offset 2)     ; CSS
  (web-mode-code-indent-offset 2)    ; JS/JSX/TS/TSX
  (web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'"))))

;; ── org ──────────────────────────────────────────────────────────────────────

(let ((target-path '"/home/erwann/src/org/capture.org")
      (template '"* %^{heading}
:PROPERTIES:
:created_on: %^T
:uname:    %(eval (concat (getenv \"OWNER\") \"@\" (system-name)))
:END:"))
(use-package org
  :init ; don't change to :custom
  (setq org-capture-templates
        `(("c" "Core" entry
           (file+headline ,target-path "Inbox")
           ,template)))
  (setq org-agenda-files (list target-path)))
)

(use-package org
  :custom
  (org-read-date-force-compatible-dates nil) ; extends calendar
  (org-log-into-drawer t)
  (org-fold-core-style 'overlays) ; https://lists.nongnu.org/archive/html/emacs-orgmode/2024-04/msg00497.html
  (tex-fontify-script nil)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)))

;; ── babel ────────────────────────────────────────────────────────────────────

(use-package ob-json
  :straight
  (:host github :repo "sgpthomas/ob-json" :files ("ob-json.el"))
  :after org)

(use-package ob-yaml
  :straight
  (:host github :repo "llhotka/ob-yaml" :files ("ob-yaml.el"))
  :after org)

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (org . t)
     (java . t)
     (python . t)
     (shell . t)
     (lua . t)
     (yaml . t)
     (json . t)
     )))

;; ── ql ───────────────────────────────────────────────────────────────────────

(use-package org-ql
  :straight (:host github :repo "alphapapa/org-ql"))

;; ── vertico ──────────────────────────────────────────────────────────────────

;; Enable vertico
(use-package vertico
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
  ;;    :straight t
  :init
  (savehist-mode))

;; ── web-mode ─────────────────────────────────────────────────────────────────

(use-package web-mode
  ;;    :straight t
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

;; ── wolfram ──────────────────────────────────────────────────────────────────

(use-package ob-mathematica
  :disabled
  :straight (:host github :repo "tririver/ob-mathematica")
  :after org)

(use-package wolfram-mode
  :disabled
  :mode (("\\.m\\'" . wolfram-mode)
	 ("\\.nb\\'" . wolfram-mode))

  :custom
  (wolfram-program
   "/usr/local/Wolfram/WolframEngine/14.0/SystemFiles/Kernel/Binaries/Linux-x86-64/WolframKernel")

  :config
  (require 'ob-mathematica))
(use-package wolfram-mode
  :disabled
  ;; :commands (wolfram-mode run-wolfram) ;; Uncomment if needed
  :mode (("\\.m\\'" . wolfram-mode)
         ("\\.nb\\'" . wolfram-mode))
  :init
  (setq wolfram-program "/usr/local/Wolfram/WolframEngine/14.0/SystemFiles/Kernel/Binaries/Linux-x86-64/WolframKernel")
  ;; Uncomment and adjust the following line if you need to set wolfram-path
  ;; (setq wolfram-path "/Owners/yourownername/Library/WolframEngine/12.3/Applications")
  :config
  (require 'ob-mathematica "/home/erwann/github/ob-mathematica/ob-mathematica.el"))
