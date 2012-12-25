;;; perlbrew.el --- A perlbrew wrapper for Emacs

;; Copyright (C) 2011 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Version: 0.2
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

;;; Initialize
;; (require 'perlbrew)
;; (perlbrew-use "perl-5.12.3") ;; initialize perl version to use

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;; `perlbrew-dir'
;; your perlbrew directory
;; default = ~/perl5/perlbrew

;;; Code:

(defgroup perlbrew nil
  "perlbrew"
  :group 'perlbrew)

(defcustom perlbrew-dir (concat (getenv "HOME") "/perl5/perlbrew")
  "your perlbrew directory"
  :group 'perlbrew)

(defvar perlbrew-perls-dir nil)
(defvar perlbrew-command-path nil)

(defvar perlbrew-current-perl-dir nil)
(defvar perlbrew-current-perl-path nil)

(eval-when-compile
  (require 'cl))

(defun perlbrew (args)
  (interactive "M$ perlbrew ")
  (let* ((command (perlbrew-command args))
         (result (perlbrew-trim (shell-command-to-string command))))
    (if (called-interactively-p 'interactive)
        (unless (string-match "^\\s*$" result) (message result))
      result)))

(defun perlbrew-use (version)
  (interactive (list (completing-read "Version: " (perlbrew-list))))
  (setq perlbrew-perls-dir (concat perlbrew-dir "/perls"))

  (cond ((equal version "system")
         (perlbrew-clean-exec-path)
         (setq perlbrew-current-perl-path
               (perlbrew-trim (shell-command-to-string "which perl"))))
        (t
         (perlbrew-set-current-perl-path version)
         (perlbrew-set-current-exec-path))))

(defun perlbrew-switch (version)
  (interactive (list (completing-read "Version: " (perlbrew-list))))
  (perlbrew-use version))

(defun perlbrew-command (args)
  (setq perlbrew-command-path (concat perlbrew-dir "/bin/perlbrew"))
  (perlbrew-join (list perlbrew-command-path args)))

(defun perlbrew-list ()
  (let* ((perls (split-string (perlbrew "list"))))
    (remove-if
     (lambda (i)
       (not (string-match "^\\(perl\\|[0-9]\\|system\\)" i)))
     (append perls '("system")))))

(defun perlbrew-get-current-perl-path ()
  perlbrew-current-perl-path)

(defun perlbrew-set-current-perl-path (version)
  (setq perlbrew-current-perl-dir (concat perlbrew-perls-dir "/" version))
  (setq perlbrew-current-perl-path (concat perlbrew-current-perl-dir "/bin/perl")))

(defun perlbrew-set-current-exec-path ()
  (let* ((bin-dir (concat perlbrew-current-perl-dir "/bin")))
    (perlbrew-clean-exec-path)

    ;; setting for PATH
    (setenv "PATH" (concat bin-dir ":" (getenv "PATH")))

    ;; setting for exec-path
    (add-to-list 'exec-path bin-dir)))

(defun perlbrew-clean-exec-path ()
  (setenv "PATH"
          (mapconcat
           'identity
           (perlbrew-remove-all-perl-path (split-string (getenv "PATH") ":"))
           ":"))
  (setq exec-path (perlbrew-remove-all-perl-path exec-path)))

(defun perlbrew-join (list)
  (mapconcat 'identity list " "))

(defun perlbrew-trim (string)
  (replace-regexp-in-string "\n+$" "" string))

(defun perlbrew-remove-all-perl-path (path-list)
  (remove-if
   (lambda (i)
     (string-match (concat "^" perlbrew-perls-dir) i))
   path-list))

(provide 'perlbrew)
;;; perlbrew.el ends here
