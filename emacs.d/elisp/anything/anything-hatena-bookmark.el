;;; anything-hatena-bookmark.el --- Hatena::Bookmark anything.el interface
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2008 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 1.0.9
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;; Thanks to tomoya for using migemo and multiline advice.(1.0.9)

;;; Commentary:
;; `anything-hatena-bookmark' is `anything' interface of your Hatena::Bookmark.
;; `anything-c-source-hatena-bookmark' is a source for your Hatena::Bookmark.
;; `anything-hatena-bookmark-get-dump' is function to get dump your Hatena::Bookmark.

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'anything-hatena-bookmark)
;; (setq anything-hatena-bookmark-samewindow t)
;;
;; And, you should execute `anything-hatena-bookmark-get-dump to reflesh dump file.

;;; Change Log
;; 1.0.9:use migemo and multiline.
;; 1.0.8:suport new dump rss format (2009/01/21). modify trim function.
;; 1.0.7:suport new dump rss format.
;; 1.0.6:require xml.el.
;; 1.0.5:(let ((url-request-extra-headers nil)))
;; 1.0.4:add summary(bookmark comment).add some action.
;; 1.0.3:need not be initialized `anything-hatena-bookmark-get-dump'
;; 1.0.2:use `push' for setting `url-request-extra-headers'
;; 1.0.1:use read-passwd. set "less" option.
;; 1.0.0:

;;; Code:

(eval-when-compile (require 'cl))
(require 'anything)
(require 'url)
(require 'xml)
(require 'sha1)

(defvar anything-hatena-bookmark-file "~/.hatenabookmark")
(defvar anything-hatena-bookmark-candidate-number-limit 9999)
(defvar anything-hatena-bookmark-requires-pattern 3)
(defvar anything-hatena-bookmark-samewindow anything-samewindow)

(defun anything-hatena-bookmark-get-dump ()
  "Get Hatena::Bookmark dump file."
  (interactive)
  (let
      ((created (format-time-string "%Y-%m-%dT%TZ" (current-time)))
       (nonce (sha1 (format-time-string "%Y-%m-%dT%T%z" (current-time))))
       (url "http://b.hatena.ne.jp/dump")
       (url-request-extra-headers nil)
       (x-wsse "")
       (x-wsse-list nil)
       (entry-list nil)
       (id (read-string "Hatena ID: "))
       (password (read-passwd "Password: ")))
    (setq x-wsse (concat "UsernameToken Username=\"" id "\", PasswordDigest=\"" (base64-encode-string (sha1-binary (concat nonce created password))) "\", Nonce=\"" (base64-encode-string nonce) "\", Created=\"" created "\""))
    (setq x-wsse-list (cons "X-WSSE" x-wsse))
    (setq url-request-extra-headers (list x-wsse-list))
    (switch-to-buffer (url-retrieve-synchronously url))
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (delete-region (point-min) (1+ (point)))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "> +<" nil t)
      (replace-match "><"))
    (setq entry-list (xml-get-children (car (xml-parse-region (point-min) (point-max))) 'entry))
    (delete-region (point-min) (point-max))
    (loop for elm in entry-list
          do (insert
              (concat
               (apply 'concat (loop for elmelm in (xml-get-children elm 'dc:subject) collect (concat "[" (nth 2 elmelm) "]")))
               " "
               (let ((title (nth 2 (car (xml-get-children elm 'title)))))
                 (while (string-match "[\n\t]" title)
                     (setq title (replace-match "" nil nil title)))
                 title)
               (concat " [summary:" (nth 2 (car (xml-get-children elm 'summary))))
               (concat "][href:" (xml-get-attribute (car (xml-get-children elm 'link)) 'href))
               "]\n")))
    (write-file anything-hatena-bookmark-file)
    (kill-buffer (current-buffer))))

(defvar anything-c-source-hatena-bookmark
  `((name . "Hatena::Bookmark")
    (init
     . (lambda ()
           (with-current-buffer (anything-candidate-buffer 'global)
             (insert-file-contents anything-hatena-bookmark-file))))
    (candidates-in-buffer)
    (candidate-number-limit . ,anything-hatena-bookmark-candidate-number-limit)
    (requires-pattern . ,anything-hatena-bookmark-requires-pattern)
    (migemo)
    (multiline)
    (action
     ("Browse URL" . (lambda (candidate)
                       (string-match "\\[href:\\(.+\\)\\]$" candidate)
                       (browse-url (match-string 1 candidate))))
     ("Show URL" . (lambda (candidate)
                     (string-match "\\[href:\\(.+\\)\\]$" candidate)
                     (message (match-string 1 candidate))))
     ("Show Summary" . (lambda (candidate)
                         (string-match "\\[summary:\\(.+\\)\\]\\[" candidate)
                         (message (match-string 1 candidate)))))))

(defun anything-hatena-bookmark ()
  "Search Hatena::Bookmark using `anything'."
  (interactive)
  (let ((anything-samewindow anything-hatena-bookmark-samewindow))
    (unless (file-exists-p anything-hatena-bookmark-file)
      (anything-hatena-bookmark-get-dump))
    (anything
     '(anything-c-source-hatena-bookmark) nil "Find Bookmark: " nil nil)))

(provide 'anything-hatena-bookmark)

;;; end
;;; anything-hatena-bookmark.el ends here
