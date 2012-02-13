;;; perlbrew.el --- A perlbrew wrapper for Emacs

;; Copyright (C) 2011 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Keywords: Emacs, Perl

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

;; (require 'perlbrew)
;; (perlbrew-switch "perl-5.12.3") ;; initialize perl version to use

;;; Code:

(defvar perlbrew-command-path "perlbrew")
(defvar perlbrew-current-perl-path nil)

(eval-when-compile
  (require 'cl))

(defun perlbrew (args)
  (interactive "M$ perlbrew ")
  (let* ((command (perlbrew-command args))
         (result (perlbrew-trim (shell-command-to-string command))))
    (if (interactive-p)
        (unless (string-match "^\\s*$" result) (message result))
      result)))

(defun perlbrew-switch (version)
  (interactive (list (completing-read "Version: " (perlbrew-list))))
  (perlbrew (perlbrew-join (list "switch" version)))
  (perlbrew-set-current-perl-path))

(defun perlbrew-command (args)
  (perlbrew-join (list perlbrew-command-path args)))

(defun perlbrew-list ()
  (let* ((perls (split-string (perlbrew "list"))))
    (remove-if
     (lambda (i)
       (not (string-match "^\\(perl\\|[0-9]\\|system\\)" i)))
     (append perls '("system")))))

(defun perlbrew-get-current-perl-path ()
  perlbrew-current-perl-path)

(defun perlbrew-set-current-perl-path ()
  (setq perlbrew-current-perl-path (perlbrew-trim (shell-command-to-string "which perl"))))

(defun perlbrew-join (list)
  (mapconcat 'identity list " "))

(defun perlbrew-trim (string)
  (replace-regexp-in-string "\n+$" "" string))

(provide 'perlbrew)
;;; perlbrew.el ends here
