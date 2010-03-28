;;; dabbrev-highlight.el --- Highlight strings refered `dabbrev-expand'.

;; Copyright (C) 2002 Hideyuki SHIRAI <shirai@meadowy.org>

;; Authors: Hideyuki SHIRAI <shirai@meadowy.org>,
;;          Satoru Takabayashi <satoru@namazu.org>
;; Keywords: abbrev expand completion convenience highlight

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file provides the extension to highlight strings refered by
;; `dabbrev-expand'.

;; The latest version of this program can be downloaded from
;; http://namazu.org/~tsuchiya/elisp/dabbrev-highlight.el.


;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'dabbrev-highlight)


;;; History:

;; Original idea was posted by Satoru Takabayashi <satoru@namazu.org>
;; to Farm Mailing List in (ELF:01490), and
;; Hideyuki SHIRAI <shirai@meadowy.org> impremented it.  His version
;; can be downloaded from
;; http://namazu.org/~satoru/diary/?200204a&to=200204031#200204031.

;; The following program was slightly simplified by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.


;;; Code:

(eval-when-compile
  (require 'custom)
  (require 'dabbrev)
  (if (featurep 'xemacs)
      (require 'overlay)))

(defcustom dabbrev-highlight-face 'highlight
  "*Face to highlight last expanded string."
  :group 'dabbrev
  :type 'face)

(defvar dabbrev-highlight-overlay nil
  "Interanal variable keeps an overlay highlighting the last expanded string.")

(let (current-load-list)
  (defadvice dabbrev-expand
    (after dabbrev-expand-highlight activate)
    "Advised by dabbrev-highlight.el.
Highlight last expanded string."
    (dabbrev-highlight)))

(defun dabbrev-highlight ()
  (let ((start dabbrev--last-expansion-location)
	(end)
	(len (length dabbrev--last-expansion))
	(buf (or dabbrev--last-buffer (current-buffer)))
	(mini-p (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name))))
    (save-selected-window
      (save-excursion
	(if (eq buf (current-buffer))
	    (if (> start (point))
		(setq end start
		      start (- end len))
	      (setq end (+ start len)))
	  (set-buffer buf)
	  (setq end start
		start (- end len)))
	(if (and (get-buffer-window buf)
		 (select-window (get-buffer-window buf))
		 (pos-visible-in-window-p start)
		 (pos-visible-in-window-p end))
	    (progn
	      ;; Highlight the string used for the last expansion.
	      (if dabbrev-highlight-overlay
		  (move-overlay dabbrev-highlight-overlay start end)
		(setq dabbrev-highlight-overlay (make-overlay start end)))
	      (overlay-put dabbrev-highlight-overlay
			   'face dabbrev-highlight-face)
	      (add-hook 'pre-command-hook 'dabbrev-highlight-done))
	  (unless mini-p
	    ;; Display one-line summary in minibuffer.
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char start)
		(let ((str (buffer-substring-no-properties start end))
		      (bol (progn (forward-line 0) (point)))
		      (eol (progn (end-of-line) (point))))
		  (if (or (featurep 'xemacs)
			  (<= emacs-major-version 20))
		      (setq str (concat " *" str "* "))
		    (put-text-property 0 (length str)
				       'face dabbrev-highlight-face str))
		  (message "%s(%d): %s%s%s"
			   (buffer-name buf)
			   (count-lines (point-min) start)
			   (buffer-substring-no-properties bol start)
			   str
			   (buffer-substring-no-properties end eol)))))))))))

(defun dabbrev-highlight-done ()
  (remove-hook 'pre-command-hook 'dabbrev-highlight-done)
  (if dabbrev-highlight-overlay
      (delete-overlay dabbrev-highlight-overlay)))

(provide 'dabbrev-highlight)

;;; dabbrev-highlight.el ends here
