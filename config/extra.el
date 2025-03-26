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
      (let ((result-with-properties
             (org-table-get-remote-range tbl-id range-address)))
        (mapcar (lambda (s)
                  (substring-no-properties (substring s 1 -1)))
                result-with-properties)))))
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
(defun erw/function-filter-elements (type regex)
  "Filter elements of the given TYPE from the current Org buffer by matching their name with REGEX."
  (let* ((parsed-buffer (org-element-parse-buffer))
         (elements (org-element-map parsed-buffer type 
                                  (lambda (elem) (org-element-property :name elem)))))
         (-filter (lambda (elem) (string-match-p regex elem)) elements)))
(defalias 'erw/filter-elements 'erw/function-filter-elements)
(defun erw/filter-block-names (regex &optional file)
  "Filter the source block names using REGEX in FILE."
  (let ((block-names (reverse (org-babel-src-block-names file))))
    (cl-remove-if-not (lambda (block) (string-match-p regex block)) block-names)))
(defun erw/noweb-expand (name)
  "Expands block NAME"
  (let* ((block (org-babel-find-named-block name))
	 (info (when block
		 (save-excursion
                   (goto-char block)
                   (org-babel-get-src-block-info t)))))
    (when info
      (org-babel-expand-noweb-references info))))
(defun __erw/noweb-concat-rest (separator &rest names)
  "Concatenate the blocks NAMEs using SEPARATOR"
  (mapconcat #'erw/noweb-expand names separator))
(defun __erw/noweb-concat-list (separator &optional names)
  "Concatenate the blocks NAMEs using SEPARATOR"
  (apply #'__erw/noweb-concat-rest separator names))
(defun erw/noweb-concat (separator &rest names)
  "Concatenate the blocks NAMEs using SEPARATOR."
  (if (and names
           (listp (car names))
           (null (cdr names)))
      (__erw/noweb-concat-list separator (car names))
    (__erw/noweb-concat-rest separator names)))
