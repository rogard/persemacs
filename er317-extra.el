;; erw1-extra.el --- Extra functionality -*- lexical-binding: t; -*-
;; Copyright (C) 2024—2026 — Erwann Rogard
;; Released under GPL 3.0
;; See https://www.gnu.org/licenses/gpl-3.0.en.html

;;; Commentary:
;;; Code:

(defun er317/code-banner (title comment-symb &optional depth)
  "Insert a 80-character banner: COMMENT-SYMB TITLE and continued with a line.

DEPTH:
0 = top-level
1 = major section
2 = subsection
3 = ordinary note"

  (interactive
   (list
    (read-string "Title: ")
    (read-string "Comment-Symb: " "%")
    (read-number "Depth (0-3): " 3)))

  (let* ((depth (max 0 (min 3 (or depth 3))))
         (char (pcase depth
                 (0 ?█)
                 (1 ?▬)
                 (2 ?═)
                 (_ ?─)))
         (title (if (= depth 0)
                   (upcase title)
                 title))
         (prefix (format "%s %c%c " comment-symb char char))
         (content (concat title " "))
         (total-width 80)
         (fill (max 0
                    (- total-width
                       (length prefix)
                       (length content))))
         (line (concat prefix
                       content
                       (make-string fill char))))
    (insert line "\n")))

;; Operations on custom id's set as follows:
;; `M-x org-set-property-and-value RET custom_id:ID`

(cl-defun er317/org-dependency--format-item
    (curr-cid dep-cid
              &key
              (fmt "%f [[#%d][%l]]")
              (true "✅")
              (false "❎")
              (precedes (er317/org-custom-id-precedes-p dep-cid curr-cid))
              (flag (if precedes true false))
	      (spec `((?f . ,flag)
		      (?d . ,dep-cid)
		      (?l . ,dep-cid))))
  (format-spec fmt spec))

(cl-defun er317/org-custom-id-precedes-p (before-cid after-cid)
  "Returns t is position BEFORE-CID before that of AFTER-CID in the current buffer."
  (let ((before-pos (er317/org-custom-id->position before-cid))
        (after-pos (er317/org-custom-id->position after-cid)))
    (and before-pos
         after-pos
         (< before-pos after-pos))))
(defun er317/org-custom-id-current ()
  "Returns the custom id of the current subtree."
  (org-with-point-at (point)
    (org-back-to-heading t)
    (org-entry-get (point) "CUSTOM_ID")))
(cl-defun er317/org-dependency--format-item
    (curr-cid dep-cid
              &key
              (fmt "%f [[#%d][%l]]")
              (true "✅")
              (false "❎")
              (precedes (er317/org-custom-id-precedes-p dep-cid curr-cid))
              (flag (if precedes true false))
	      (spec `((?f . ,flag)
		      (?d . ,dep-cid)
		      (?l . ,dep-cid))))
  (format-spec fmt spec))
