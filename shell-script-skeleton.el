;;; shell-script-skeleton.el --- template for shell script              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hiroyuki Bessho

;; Author: Hiroyuki Bessho <Bessho@genetec.co.jp>
;; Keywords: files, unix, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; template for shell script
;;
;; (add-to-list 'auto-insert-alist
;;              '(sh-mode . shell-script-skeleton))
;;
;; snippet for getopt can be inserted by
;; M-x -shell-script-getopt-skeleton
;;


;;; Code:
(define-skeleton shell-script-getopt-skeleton
  "Template for shell script snippet to call getopt(1)"
  nil
  (make-shell-script-getopt-snippet (skeleton-read "Options: ")))

(define-skeleton shell-script-skeleton
  "Template for shell script."
  ""
  '(setq v1 (skeleton-read "Options string to getopt: "))
  "#!/bin/sh" \n
  "#" \n
  "#" \n
  (make-shell-script-getopt-snippet v1)
  \n _ \n)

;;;
;;; helper function to expand skeleton to a string.
;;; XXX: Are there any official functions to do this task?
;;;
(defun shsskel:expand-skeleton-as-string (mode skeleton)
  "Expand SKELETON in a temporary buffer with MODE"
  (with-temp-buffer
    (funcall mode)
    (skeleton-insert skeleton)
    (buffer-string)))

;;;
;;; "ab:c:d" -> (a (b) (c) d)
;;;
(defun shsskel:opt-string-to-list (opt)
  (shsskel:opt-to-list (append (vconcat opt) nil)))

(defun shsskel:opt-to-list (lis)
  (cond
   ((null lis) nil)
   ((null (cdr lis)) lis)   ; last element
   ((eq (cadr lis) ?:) (cons (list (car lis)) (shsskel:opt-to-list (cddr lis))))
   (t (cons (car lis) (shsskel:opt-to-list (cdr lis))))))
    
(defun shsskel-case-statement-skeleton (opt)
  (mapcan #'identity
	  (mapcar #'(lambda (x) (list x '> ?\n))
		  `("case $1 in"
		    ,@(mapcar #'(lambda (c)
				  (if (consp c)
				      (format "-%c) %c_arg=$2; shift;;" (car c) (car c))
				   (format "-%c) ;;" c)))
			       (shsskel:opt-string-to-list opt))
		    "--) shift; break;;"
		    "esac"
		    "shift"))))

;;;
;;; returns shell script snippet to process options.
;;; It returns a string, not a template, to avoid repeated calls.
;;; XXX: Is there any better way to call template recursively but only once?
;;;
(defun make-shell-script-getopt-snippet (opt)
  "return a shell script snippet to call getopt(1)"
  (shsskel:expand-skeleton-as-string 'sh-mode
   (and (not (string= opt ""))
	`("" nil
	  "args=$(getopt " ,opt " $*)\n"
	  "if [ $? -ne 0 ]; then\n"
	  > "echo >&2 \"Usage: $0 ...\";" ?\n
	  > "exit 2" ?\n
	  "fi\n"
	  "set -- $args\n"
	  "while [ $# -ne 0 ]; do\n"
	  ,@(shsskel-case-statement-skeleton opt)
	  "done\n"
	  )
	)))

(provide 'shell-script-skeleton)
;;; shell-script-skeleton.el ends here
