;;  persemacs-extra — extra elisp functionality
;;  Copyright (C) 2024 — Erwann Rogard
;;  Released under GPL 3.0
;;  See https://www.gnu.org/licenses/gpl-3.0.en.html
(use-package dash
:ensure t)
(defgroup erw/extra nil "erw's extra elisp functionality"
  :prefix "erw/")
(defun erw/function-string-wrap-single-quotes (string)
  "Wraps STRING with single quotes if absent."
  (if (string-match-p "^'.*'$" string)
     string
    (concat "'" string "'")))
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
(defun erw/filter-block-names (regex &optional file)
  "Filter the source block names using REGEX in FILE."
  (let ((block-names (reverse (org-babel-src-block-names file))))
    (-filter (lambda (block) (string-match-p regex block)) block-names)))
(defun erw/compose (arg &rest functions)
  (-reduce-r (lambda (fn acc) (funcall fn acc)) (append (reverse functions) (list arg))))
(defun erw/noweb-expand (name)
  "Expands block NAME"
  (let* ((block (org-babel-find-named-block name))
	 (info (when block
		 (save-excursion
                   (goto-char block)
                   (org-babel-get-src-block-info t)))))
    (when info
      (org-babel-expand-noweb-references info))))
(defun __erw/noweb-concat-rest (separator &optional fn &rest names)
  "Implementation for REST"
  (let ((fn (or fn #'identity)))
    (mapconcat (lambda (name) (funcall fn (erw/noweb-expand name))) names separator)))
(defun __erw/noweb-concat-list (separator &optional fn names)
  "Implementation for LIST"
  (apply #'__erw/noweb-concat-rest separator fn names))
(defun erw/noweb-concat (separator &optional fn &rest names)
  "Expand, pass to a function, and concatenate blocks using SEPARATOR, FN, and NAMES.
Dispatches based on whether NAMES is a list or individual arguments."
  (when names
    (if (and (listp (car names)) (null (cdr names))) ;; Single list argument case
        (__erw/noweb-concat-list separator fn (car names))
      (apply #'__erw/noweb-concat-rest separator fn names))))
(defun erw/src-block-info (name &optional no-eval)
  "Gets info of block NAME"
  (let ((block (org-babel-find-named-block name)))
	 (when block
		 (save-excursion
                   (goto-char block)
                   (org-babel-get-src-block-info no-eval)))))
(defun erw/src-block-element (name) "Return the whole block element"
       (save-excursion
	 (goto-char (org-babel-find-named-block name))
	 (org-element-at-point)))
(defun erw/src-block-properties (name &rest properties)
  "Return block properties from the named block element. Defaults to :value if no properties are given."
  (let* ((element (erw/src-block-element name))  ;; Use erw/src-block-element to get the block
         (props (if properties
                    properties
                  '(:value))))  ;; Default to :value if no properties are provided
    (mapcar (lambda (prop)
              (org-element-property prop element))  ;; Get each property using org-element-property
            props)))
(defun erw/function-filter-elements (type regex)
  "Filter elements of the given TYPE from the current Org buffer by matching their name with REGEX."
  (let* ((parsed-buffer (org-element-parse-buffer))
         (elements (org-element-map parsed-buffer type 
                                  (lambda (elem) (org-element-property :name elem)))))
         (-filter (lambda (elem) (string-match-p regex elem)) elements)))
(defalias 'erw/filter-elements 'erw/function-filter-elements)
