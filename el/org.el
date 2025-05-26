;;  persemacs-extension — Emacs Lisp functionality
;;  Copyright (C) 2024—2025 — Erwann Rogard
;;  Released under GPL 3.0
;;  See https://www.gnu.org/licenses/gpl-3.0.en.html
  (cl-defun er317/org-subtree-paste
      (&key (target-file (buffer-file-name))
            (target-pos (point))
            (level-fn (lambda (level) (+ level 1))))
    "Paste into TARGET, the subtree in the clipboard.
  The level is set by LEVEL-FN; by default = point level +1.
Tip: use `org-cut-subtree` prior to this one."
    (interactive)
    (unless (org-kill-is-subtree-p)
      (user-error "Clipboard does not contain a valid Org subtree"))
    (let (level)
      (with-current-buffer (find-file-noselect target-file)
        (goto-char target-pos)
        (setq level (funcall level-fn (org-current-level)))
        (org-end-of-subtree)
        (org-paste-subtree level nil nil t)
        (save-buffer))))
