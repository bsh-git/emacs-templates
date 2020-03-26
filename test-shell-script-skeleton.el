;;; test-shell-script-skeleton.el --- Ert for shell-script-skeleton.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hiroyuki Bessho

;; Author: Hiroyuki Bessho <Bessho@genetec.co.jp>
;; Keywords: maint

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

(ert-deftest shsskel-test:opt-to-list ()
  "test shsskel:opt-string-to-list"
  (should (eq (shsskel:opt-string-to-list "") nil))
  (should (equal (shsskel:opt-string-to-list "a") '(?a)))
  (should (equal (shsskel:opt-string-to-list "abc") '(?a ?b ?c)))
  (should (equal (shsskel:opt-string-to-list "a:bc") '((?a) ?b ?c)))
  (should (equal (shsskel:opt-string-to-list "a:bc:") '((?a) ?b (?c)))))
    

(ert-deftest shsskel-test:case-statement-skeleton ()
    "test shsskel-case-statment-skeleton"
    (should (equal (shsskel-case-statement-skeleton "ab:")
		   '("case $1 in" > ?\n
		     "-a) ;;" > ?\n
		     "-b) b_arg=$2; shift;;" > ?\n
		     "--) shift; break;;" > ?\n
		     "esac"  > ?\n
		     "shift" > ?\n))))

;;; test-shell-script-skeleton.el ends here
