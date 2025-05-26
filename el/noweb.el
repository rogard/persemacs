;;  persemacs-extension — Emacs Lisp functionality
;;  Copyright (C) 2024—2025 — Erwann Rogard
;;  Released under GPL 3.0
;;  See https://www.gnu.org/licenses/gpl-3.0.en.html
(cl-defun er317/noweb-ref-expand (&key regex)
  "Expands all source blocks whose :noweb-ref matches REGEX and returns their expanded contents, joined by SEP."
  (let ((results '()))
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (src)
        (let* ((begin (org-element-property :begin src))
               (info (save-excursion
                       (goto-char begin)
                       (org-babel-get-src-block-info t)))
               (params (nth 2 info))  ; header arguments parsed by org-babel
               (ref (cdr (assoc :noweb-ref params))))
          ;; (message "DEBUG parsed params: %S" params)
          (when (and ref (string-match-p regex ref))
            (let ((expanded (org-babel-expand-noweb-references info)))
              (push expanded results))))))
    (nreverse results)))
(cl-defun er317/noweb-ref-collect (&key head ref-list tail)
  "Builds a regex from REF-LIST with optional HEAD and TAIL anchors, then forwards to `er317/noweb-ref-expand`."
  (let* ((regex-body (mapconcat #'identity ref-list "\\|"))
         (regex (concat (or head "") "\\(" regex-body "\\)" (or tail "")))
         (args `(:regex ,regex)))
    (apply #'er317/noweb-ref-expand args)))
(cl-defun er317/noweb-ref-assemble
    (&key key-list head ref-list tail parse-fn encode-fn)
  "Expand noweb REF-LIST, each surrounded by HEAD and TAIL.
PARSE-FN parses the raw strings; 
ENCODE-FN turns parsed data into final output.
When provided, KEY-LIST is passed to ENCODE-FN."
  (let* ((raw-blocks (er317/noweb-ref-collect :head head :ref-list ref-list :tail tail))
         (parsed-list (mapcar parse-fn raw-blocks)))
    (when (and key-list
               (/= (length key-list) (length parsed-list)))
      (error "key-list and parsed-list must be of equal length"))
    (funcall encode-fn
             (if key-list
                 (cl-pairlis key-list parsed-list)
               parsed-list))))
