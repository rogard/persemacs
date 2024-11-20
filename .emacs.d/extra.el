;;  persemacs-extra — extra elisp functionality
;;  Copyright (C) 2024 — Erwann Rogard
;;  Released under GPL 3.0
;;  See https://www.gnu.org/licenses/gpl-3.0.en.html
(defgroup erw/extra nil "erw's extra elisp functionality"
  :prefix "erw/")
(defun erw/function-string-wrap-single-quotes (filename)
  "Wraps STRING with single quotes if absent."
  (if (string-match-p "^'.*'$" filename)
     string
    (concat "'" filename "'")))
(defalias 'erw/wrap-single-quotes 'erw/function-string-wrap-single-quotes)
(defun erw/function-table-field-address (index)
  "Table address for field INDEX"
  (format "@1$%d..@>$%d" index index))
(defalias 'erw/field-address 'erw/function-table-field-address)
(defun erw/function-table-get-range-at-file (tbl-id range-address &optional file-name)
  "Get list of values in RANGE-ADDRESS from TBL-ID at FILE-NAME.
     Credits: https://redd.it/r2nig7"
  (let ((file-name (or file-name (buffer-file-name (current-buffer)))))
    (with-current-buffer (find-file-noselect file-name)
      (org-table-get-remote-range tbl-id range-address))))
(defalias 'erw/table-range 'erw/function-table-get-range-at-file)
  (defun erw/function-table-lookup (tbl-id key &optional file-name key-index value-index match-predicate)
    "Lookup field KEY-INDEX and return corresponding entry in field VALUE-INDEX from table TBL-ID."
    (interactive)
    (let ((key-address (erw/field-address (or key-index 1)))
          (value-address (erw/field-address (or value-index 2)))
          (file-name (or file-name (buffer-file-name (current-buffer))))
          (match-predicate (or match-predicate 'string-match-p)))
      (let ((key-range (erw/table-range tbl-id key-address file-name))
            (value-range (erw/table-range tbl-id value-address file-name)))
        (org-lookup-first key key-range value-range 'string-match-p))))
(defalias 'erw/table-lookup 'erw/function-table-lookup)
(defun erw/table-to-path (tbl-name key &optional child)
  (let* ((parent (erw/table-lookup tbl-name key))
         (parent (erw/ensure-directory parent))
         (args (if child
                   (list child parent)
                 (list parent))))
    (message "debug: parent is %s" parent)
    (apply #'expand-file-name args)))
(defun erw/ensure-directory (path)
  "Ensures PATH ends with a slash"
  (if (not (string-match-p "/$" path))
      (concat path "/")
    path))
