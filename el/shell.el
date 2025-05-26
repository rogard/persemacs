;;  persemacs-extension — Emacs Lisp functionality
;;  Copyright (C) 2024—2025 — Erwann Rogard
;;  Released under GPL 3.0
;;  See https://www.gnu.org/licenses/gpl-3.0.en.html
(cl-defun er317/jq-file (&key filter file options)
  "Apply a jq FILTER to a JSON FILE and return the result."
  (let* ((parts (delq nil (append (list "jq") options (list (format "'%s'" filter) file)))) (command (string-join parts " ")))
    ;;    (message "DEBUG: parts: %s" parts)
    ;;    (message "DEBUG: command: %s" command)
    (shell-command-to-string command)))
(cl-defun er317/jq-string (&key filter string options)
  "Apply a jq filter to a JSON string and return the result."
  (let* ((temp-file (make-temp-file nil nil ".json"))
         (result (progn
                   (with-temp-file temp-file
                     (insert string))
                   (er317/jq-file :filter filter :file temp-file :options options))))
    (delete-file temp-file)
    (format "%s" result)))
