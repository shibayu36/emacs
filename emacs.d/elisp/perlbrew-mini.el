;;; perlbrew-mini.el --- A simple perlbrew wrapper for Emacs

;; Copyright (C) 2011 Damien Krotkine

;; Author: Damien Krotkine <dams@cpan.org>
;; Keywords: Emacs, Perl, perlbrew

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

;;; Simple Usage:

;;  This will look for available perls in the default perlbrew directory
;;  ($HOME/perl5/perlbrew/perls/) and load the latest version

;; (require 'perlbrew-mini)
;; (perlbrew-mini-use-latest)


;; More options:

;; (require 'perlbrew-mini)

;; the directory containing all the perls (defaults to $HOME/perl5/perlbrew/perls/)
;; (perlbrew-mini-set-perls-dir "/home/username/perl5/perlbrew/perls/")

;; the version to use
;; (perlbrew-mini-use "perl-5.12.3") ;; initialize perl version to use

;; Later on, the following lisp functions will be available :
;;   perlbrew-mini-get-current-perl-path ;; returns the current perl path
;;   perlbrew-mini-get-current-version ;; returns the current perl version in use

;; These functions are especially useful with flymake.

;;; Code:

(defvar perlbrew-mini-perls-dir (concat (getenv "HOME") "/perl5/perlbrew/perls/"))
(defvar perlbrew-mini-current-perl-path nil)
(defvar perlbrew-mini-current-version nil)

(defun perlbrew-mini-use (version)
  (perlbrew-mini-set-current-version version )
  (perlbrew-mini-set-current-perl-path)
  (perlbrew-mini-set-current-exec-path)
)

(defun perlbrew-mini-use-latest ()
  (let (latest)
    (setq latest (car (last (sort (directory-files perlbrew-mini-perls-dir) 'string<))))
    (perlbrew-mini-use latest)
    )
)

(defun perlbrew-mini-set-perls-dir (path)
  (setq perlbrew-mini-perls-dir path)
)

(defun perlbrew-mini-get-current-perl-path ()
  perlbrew-mini-current-perl-path)

(defun perlbrew-mini-get-current-version ()
  perlbrew-mini-current-version)

(defun perlbrew-mini-set-current-exec-path ()

  ;; Prepend perlbrew paths to exec-path
  (add-to-list 'exec-path (concat perlbrew-mini-perls-dir perlbrew-mini-current-version "/bin"))

  ;; set PATH to be the same as exec-path, clobber the old PATH value.
  (setenv "PATH"
          (reduce
           (lambda (a b) (concatenate 'string a ":" b))
           exec-path))
  )

(defun perlbrew-mini-set-current-perl-path ()
  (setq perlbrew-mini-current-perl-path
	(perlbrew-mini-join2
	 (list perlbrew-mini-perls-dir perlbrew-mini-current-version "/bin/perl")
	)
  )
)

(defun perlbrew-mini-set-current-version (version)
  (setq perlbrew-mini-current-version version))

(defun perlbrew-mini-join2 (list)
  (mapconcat 'identity list ""))

(provide 'perlbrew-mini)
;;; perlbrew-mini.el ends here
