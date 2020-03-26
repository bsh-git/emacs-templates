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
  (cps:include-guard (buffer-file-name))
  '(setq v1 (if (netbsd-top-dir (file-name-directory (buffer-file-name)))
		(my-netbsd-copyright)
	      ""))
  v1
  "#ifdef  "  str \n
  "#define " str \n
  \n
  _ \n
  "#endif /* " str " */" \n)

(defun netbsd-top-dir-p (dir)
  (let ((build-sh (expand-file-name "build.sh" dir))
	(building (expand-file-name "BUILDING" dir))
	(share-mk (expand-file-name "share/mk" dir)))
    (and (file-readable-p build-sh)
	 (file-readable-p building)
	 (file-directory-p share-mk))))

(defun cps:root-directory-p (dir)
  (string= dir (file-name-directory (directory-file-name dir))))

(defun cps:include-guard (filename)
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


(provide 'c-program-skeleton)
;;; x.el ends here
