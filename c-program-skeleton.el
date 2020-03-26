;;; c-program-skeleton.el --- auto-insert template for C programs     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hiroyuki Bessho

;; Author: Hiroyuki Bessho <Bessho@genetec.co.jp>
;; Keywords: c

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

;; 

;;; Code:
(defun my-netbsd-copyright ()
  "get contents of netbsd copyright template"
  (let ((copyrightfile (my-elisp-file "templates/template.netbsd-copyright")))
    (with-temp-buffer
      (insert-file-contents copyrightfile)
      (buffer-string))))

(define-skeleton netbsd-c-skeleton
  "Template for C program for NetBSD."
  ""
  '(setq v1 (my-netbsd-copyright))
  "/* $NetBSD$ */\n"
  \n
  v1
  \n
  "/*\n"
  " * " _ \n
  > "*/\n"
  "#include <sys/cdefs.h>\n"
  \n
  "#ifndef lint\n"
  "__RCSID(\"$NetBSD$\");\n"
  "#endif\n")

(define-skeleton netbsd-kern-c-skeleton
  "Template for C program for netbsd."
  ""
  '(setq v1 (my-netbsd-copyright))
  "/* $NetBSD$ */\n"
  \n
  v1
  \n
  
  "/*\n"
  " * " _ \n
  > "*/\n"
  "#include <sys/cdefs.h>\n"
  "__KERNEL_RCSID(0, \"$NetBSD$\");\n")

(define-skeleton plain-c-skeleton
  "Template for simple C program"
  ""
  "/*\n"
  " * "_ \n
  > "*/\n")

(define-skeleton c-header-skeleton
  "Template for C header file."
  (cps:select-include-guard (buffer-file-name))
  '(setq v1 (if (netbsd-top-dir (file-name-directory (buffer-file-name)))
		(my-netbsd-copyright)
	      "/*\n *\n */\n"))
  v1
  '(setq v1 (cps:make-include-guard1 (eval str)))
  '(setq v2 (cps:make-include-guard2 str))
  v1
  \n
  _ \n
  v2 \n)

(defun netbsd-top-dir-p (dir)
  (let ((build-sh (expand-file-name "build.sh" dir))
	(building (expand-file-name "BUILDING" dir))
	(share-mk (expand-file-name "share/mk" dir)))
    (and (file-readable-p build-sh)
	 (file-readable-p building)
	 (file-directory-p share-mk))))

(defun cps:make-include-guard1 (str)
  (if (null str)
      ""
    (concat "#ifndef " str "\n#define " str "\n")))

(defun cps:make-include-guard2 (str)
  (if (or (null str) (string= "" str))
      ""
    (concat "#endif  /*" str " */")))

(defun cps:root-directory-p (dir)
  (string= dir (file-name-directory (directory-file-name dir))))

(defun cps:include-guard (filename)
  "make a symbol for include guard. up to 4 directories can be included in the symbol"
  (skeleton-read prompt
  (concat "_"
	  (upcase
	   (or
	    (and (netbsd-top-dir (file-name-directory filename))
		 (let ((filename (expand-file-name filename)))
		   (if (string-match "/sys/\\(arch/\\)?" filename)
		       (replace-regexp-in-string "[/.]" "_" (substring filename (match-end 0))))))
	    (concat (file-name-nondirectory (file-name-sans-extension filename))
		    "_"
		    (file-name-extension (buffer-file-name)))))))


(defun netbsd-top-dir (&optional dir)
  "Top directory of the NetBSD source tree including directory DIR"
  (let ((dir (expand-file-name (or dir (directory-file-name default-directory))))
	rootp)
    (while (not (or (setq rootp (cps:root-directory-p dir))
		    (netbsd-top-dir-p dir)))
      (setq dir (directory-file-name (file-name-directory dir))))
    (if rootp nil dir)))
  

(defun c-program-skeleton ()
  ""
  (let ((ext (file-name-extension (buffer-file-name))))
    (if (cl-some #'(lambda (e) (string= e ext))
		 '("h" "hpp" "hxx"))
	(c-header-skeleton)
      (if (netbsd-top-dir)
	  (if (string-match "sys/" default-directory)
	      (netbsd-kern-c-skeleton)
	    (netbsd-c-skeleton))
	(plain-c-skeleton)))))


(defun cps:make-prompt-for-candidates (lis)
  (setq lis (cons "none" lis))
  (let* ((n -1)
	 (s (mapconcat #'(lambda (s)
			   (setq n (1+ n))
			   (format "(%d) %s" n s))
		       lis
		       ", ")))
    (concat "Select guard symbol: " s ": ")))

(defun cps:select-include-guard (filename)
  (let ((nbtop  (netbsd-top-dir (file-name-directory filename))))
    (if nbtop
	(setq filename (substring filename (length nbtop))))
    (let* ((lis (cps:include-guard-candidates filename))
	   (prompt (cps:make-prompt-for-candidates lis))
	   (n -1)
	   )
      ;; XXX dont'be too long.
      (while (or (< n 0) (> n (length lis)))
	(setq n (- (read-char prompt) ?0)))
      (and (/= n 0)
	   (nth (- n 1) lis)))))

(defun cps:include-guard-candidates (filename)
  (let (lis)
    (while (not (or (null filename)
		    (cps:root-directory-p filename)))
      (let ((f (file-name-nondirectory filename)))
	(if (not (or (string= f ".")
		     (string= f "..")))
	    (setq lis (cons f lis)))
	(setq filename (file-name-directory filename))
	(setq filename (and filename
			    (directory-file-name filename)))
	(if (or (string= filename ".")
		(string= filename ".."))
	    (setq filename nil))))
    (setq lis (mapcar #'(lambda (s)
			  (upcase (replace-regexp-in-string "[^_[:alnum:]]" "_" s)))
		      lis))
    (reverse (cl-maplist #'(lambda (l)
			     (mapconcat #'identity (cons "" l) "_")) lis))))

(provide 'c-program-skeleton)
;;; x.el ends here
