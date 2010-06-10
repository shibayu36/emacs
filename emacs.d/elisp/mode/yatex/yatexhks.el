;;; -*- Emacs-Lisp -*-
;;; Hooks for YaTeX

;;; 野鳥に関連する記述(たとえばアドイン関数)は yatexhks.el という名前の
;;; ファイルに入れてください。起動時に自動的にロードします。

;;; All the private definitions for YaTeX can be stuffed into the file
;;; named `yatexhks.el'.  The file `yatexhks.el' will be automatically
;;; loaded at the end of loading `yatex.el'.

;Private definitions begin from here.

;;97/1/27
(define-key YaTeX-user-extensional-map "v" 'YaTeX-section-overview)
;;initial version
(define-key YaTeX-user-extensional-map "0"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "part")))
(define-key YaTeX-user-extensional-map "1"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "chapter")))
(define-key YaTeX-user-extensional-map "2"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "section")))
(define-key YaTeX-user-extensional-map "3"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "subsection")))
(define-key YaTeX-user-extensional-map "4"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "subsubsection")))
(define-key YaTeX-user-extensional-map "5"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "paragraph")))
(define-key YaTeX-user-extensional-map "6"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "subparagraph")))
(define-key YaTeX-user-extensional-map "r"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "ref")))
(define-key YaTeX-user-extensional-map "i"
  '(lambda () (interactive) (YaTeX-make-singlecmd "item")))
(define-key YaTeX-user-extensional-map "\C-b"
  '(lambda () (interactive) (YaTeX-make-singlecmd "leftarrow")))
(define-key YaTeX-user-extensional-map "l"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "label")))
(define-key YaTeX-user-extensional-map "f"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "frac")))
(define-key YaTeX-user-extensional-map "S"
  '(lambda () (interactive) (YaTeX-make-section nil nil nil "setlength")))
(define-key YaTeX-user-extensional-map "b"
  '(lambda () (interactive) (YaTeX-make-fontsize nil "bf")))
(define-key YaTeX-user-extensional-map "I" 'YaTeX-browse-info)

(defun YaTeX-browse-info ()
  "Browse YaTeX's info"
  (interactive)
  (require 'info)
  (Info-goto-node (if YaTeX-japan "(yatexj)Top" "(yatexe)Top")))


;
;;; End of yatexhks.el
(provide 'yatexhks)
