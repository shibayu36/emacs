;;; outputz.el --- post number of letters to Outputz
;; -*- coding: utf-8; mode:emacs-lisp -*-

;; Copyright (C) 2008 Kentaro Kuribayashi
;; Author: Kentaro Kuribayashi <kentarok@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; * Description
;;
;; outputz.el simply counts numbers of letters in a buffer and posts
;; it to Outputz when the buffer saved.
;;
;; See http://outputz.com/
;;
;; * Usage
;;
;; Just put the code like below into your .emacs:
;;
;; (require 'outputz)
;; (setq outputz-key "Your Private Key")
;; ;; `%s' is replaced by `major-mode' string
;; (setq outputz-uri "http://example.com/%s")
;; (global-outputz-mode t)
;;

;;; Change Log:

;; 2008-11-21:
;;  * Applied a patch from id:hayamiz++. Some bugs are fixed.
;;    http://d.hatena.ne.jp/hayamiz/20081121/1227228535
;;  * `outpuz-url' now can be format template. `outputz' command adds
;;    `major-mode' string into url when posting.
;;    http://d.hatena.ne.jp/higepon/20081120/1227192709
;;
;; 2008-11-20:
;;  * Initial import

(defvar outputz-key nil)
(defvar outputz-url nil)

;;; customize-variables
(defgroup outputz nil
  ""
  :group 'outputz)

(defcustom outputz-modes
  '(emacs-lisp-mode lisp-interaction-mode
    c-mode c++-mode java-mode
    perl-mode cperl-mode python-mode ruby-mode
    ecmascript-mode javascript-mode php-mode css-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode
    text-mode simple-hatena-mode)
  "A list of major mode in which `outputz-mode' should be enabled."
  :type '(list symbol)
  :group 'outputz)

(require 'url)

(unless (fboundp 'mailcap-parse-mailcaps)
  (block nil
    (dolist (path load-path)
      (when (or (file-exists-p (expand-file-name "mailcap.el" path))
                (file-exists-p (expand-file-name "mailcap.elc" path)))
        (load (expand-file-name "mailcap" path)))
      (when (fboundp 'mailcap-parse-mailcaps)
        (return)))))

(require 'easy-mmode)

(define-minor-mode outputz-mode
  "Outputz mode"
  :lighter " Outputz"
  (if outputz-mode
      (progn
        (set (make-local-variable 'outputz-count) (- (point-max) (point-min)))
        (run-hooks 'outputz-mode-hook))))

(defun outputz-mode-maybe ()
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode outputz-modes))
      (outputz-mode 1)))

(if (fboundp 'define-global-minor-mode)
    (define-global-minor-mode global-outputz-mode
      outputz-mode outputz-mode-maybe))

(add-hook 'find-file-hook
          (lambda ()
            (when outputz-mode
              (setq outputz-count
                    (max 0 (- (point-max) (point-min)))))))

(add-hook 'after-save-hook 'outputz)

(defun outputz ()
  (interactive)
  (when outputz-mode
    (let ((current-count (- (point-max) (point-min))))
      (when (and (> current-count 0)
                 (> current-count outputz-count))
        (outputz-post (- current-count outputz-count)))
      (setq outputz-count current-count))))

(defun outputz-post (count)
  (let ((url "http://outputz.com/api/post")
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat "key="   outputz-key
                 "&uri="  (format outputz-uri major-mode)
                 "&size=" (int-to-string count))))
    (url-retrieve url 'outputz-callback)))

(defun outputz-callback (status)
  (url-mark-buffer-as-dead (current-buffer))
  (message status))

(provide 'outputz)

;;; outputz.el ends here
