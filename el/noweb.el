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
(cl-defun er317/noweb-ref-collect
    (&key head ref-list tail (results '()) (order :buffer))
  "Collect source blocks matching REF-LIST in ORDER (:buffer or :ref-list)."
  (pcase order
    (:ref-list
     ;; One-by-one regex match per ref
     (if (null ref-list)
         results
       (let* ((ref (car ref-list))
              (regex (concat (or head "") ref (or tail "")))
              (segment-results (apply #'er317/noweb-ref-expand `(:regex ,regex))))
         (er317/noweb-ref-collect
          :head head
          :ref-list (cdr ref-list)
          :tail tail
          :results (append results segment-results)
          :order order))))
    (:buffer
     ;; Collapse ref-list into a single regex, match in buffer order
     (let* ((regex-body (mapconcat #'identity ref-list "\\|"))
            (regex (concat (or head "") "\\(" regex-body "\\)" (or tail "")))
            (segment-results (apply #'er317/noweb-ref-expand `(:regex ,regex))))
       (append results segment-results)))
    (_ (error "Unknown :order %s" order))))
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
