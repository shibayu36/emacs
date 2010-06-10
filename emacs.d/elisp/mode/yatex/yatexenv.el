;;; -*- Emacs-Lisp -*-
;;; YaTeX environment-specific functions.
;;; yatexenv.el
;;; (c) 1994-2006 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Sat Jun 24 08:14:11 2006 on firestorm
;;; $Id: yatexenv.el,v 1.73 2006/12/24 06:17:15 yuuji Rel $

;;;
;; Functions for tabular environment
;;;

;; Showing the matching column of tabular environment.
(defun YaTeX-array-what-column-internal ()
  "Return the cons of matching column and its title of array environment.
When calling from a program, make sure to be in array/tabular environment."
  (let ((p (point)) beg eot bor (nlptn "\\\\\\\\") (andptn "[^\\]&")
	(n 0) j
	(firsterr "This line might be the first row."))
    (save-excursion
      (YaTeX-beginning-of-environment)
      (search-forward "{" p) (up-list 1)
      (search-forward "{" p) (up-list 1)
      ;;(re-search-forward andptn p)
      (while (progn (search-forward "&" p)
		    (equal (char-after (1- (match-beginning 0))) ?\\ )))
      (setq beg (1- (point)))		;beg is the point of the first &
      (or (re-search-forward nlptn p t)
	  (error firsterr))
      (setq eot (point))		;eot is the point of the first \\
      (goto-char p)
      (or (re-search-backward nlptn beg t)
	  (error firsterr))
      (setq bor (point))		;bor is the beginning of this row.
      (while (< (1- (point)) p)
	(if (equal (following-char) ?&)
	    (forward-char 1)
	  (re-search-forward andptn nil 1))
	(setq n (1+ n)))		;Check current column number
      (goto-char p)
      (cond				;Start searching \multicolumn{N}
       ((> n 1)
	(re-search-backward andptn)	;Sure to find!
	(while (re-search-backward "\\\\multicolumn{\\([0-9]+\\)}" bor t)
	  (setq n (+ n (string-to-int
			(buffer-substring (match-beginning 1)
					  (match-end 1)))
		     -1)))))
      (message "%s" n)
      (goto-char (1- beg))
      (beginning-of-line)
      (setq j n)
      (while (> j 1)
	(or (re-search-forward andptn p nil)
	    (error "This column exceeds the limit."))
	(setq j (1- j)))
      (skip-chars-forward "\\s ")
      (list n
	    (buffer-substring
	     (point)
	     (progn
	       (re-search-forward (concat andptn "\\|" nlptn) eot)
	       (goto-char (match-beginning 0))
	       (if (looking-at andptn)
		   (forward-char 1))
	       (skip-chars-backward "\\s ")
	       (point)))))))

(defun YaTeX-array-what-column ()
  "Show matching column title of array environment.
When calling from a program, make sure to be in array/tabular environment."
  (apply 'message
	 "This is the column(#%d) of: %s"
	 (YaTeX-array-what-column-internal)))

;;;###autoload
(defun YaTeX-what-column ()
  "Show which kind of column the current position is belonging to."
  (interactive)
  (cond
   ((YaTeX-quick-in-environment-p '("tabular" "tabular*" "array" "array*"))
    (YaTeX-array-what-column))
   (t (message "Not in array/tabular environment."))))

(defun YaTeX-tabular-parse-format-count-cols (beg end)
  (goto-char beg)
  (let (elt (cols 0))
    (while (< (point) end)
	(setq elt (following-char))
	(cond
	 ((string-match (char-to-string elt) "clr") ;normal indicators.
	  (setq cols (1+ cols))
	  (forward-char 1))
	 ((equal elt ?|)		;vertical
	  (forward-char 1))
	 ((string-match (char-to-string elt) "p@") ;p or @ expression
	  (setq cols (+ (if (eq elt ?p) 1 0) cols))
	  ;;(skip-chars-forward "^{" p)
	  (skip-chars-forward "^{" end)
	  (forward-list 1))
	 ((equal elt ?*)		;*{N}{EXP} -> Repeat EXP N times
	  (skip-chars-forward "^{" end)
	  (setq cols (* (string-to-int
			 (buffer-substring
			  (1+ (point))
			  (progn (forward-list 1) (1- (point)))))
			(YaTeX-tabular-parse-format-count-cols
			 (progn (skip-chars-forward "^{" end) (1+ (point)))
			 (progn (forward-list 1) (1- (point)))))))
	 (t (forward-char 1))		;unknown char
	 ))
    cols))

(defun YaTeX-tabular-parse-format (&optional type)
  "Parse `tabular' format.
Return the list of (No.ofCols PointEndofFormat)"
  (let ((p (point)) boform eoform (cols 0))
    (save-excursion
      (if (null (YaTeX-beginning-of-environment t))
	  (error "Beginning of tabular not found."))
      (skip-chars-forward "^{")
      (forward-list 1)
      (cond
       ((eq type 'tabular*)
	(skip-chars-forward "^{")
	(forward-list 1)))
      (skip-chars-forward "^{" p)
      (if (/= (following-char) ?\{) (error "Tabular format not found."))
      (setq boform (1+ (point))
	    eoform (progn (forward-list 1) (1- (point))))
      (if (> eoform p) (error "Non-terminated tabular format."))
      (goto-char boform)
      (setq cols
	    (cond
	     ((eq type 'alignat)
	      (max
	       1
	       (1-
		(* 2
		   (string-to-int
		    (buffer-substring
		     (point)
		     (progn (up-list -1) (forward-list 1) (1- (point)))))))))
	     (t
	      (YaTeX-tabular-parse-format-count-cols (point) eoform))))
      (list cols (1+ eoform)))))

;; Insert &
(defun YaTeX-intelligent-newline-tabular (&optional type)
  "Parse current tabular format and insert that many `&'s."
  (let*((p (point)) (format (YaTeX-tabular-parse-format type))
	(cols (car format)) (beg (car (cdr format)))
	space hline)
    (cond
     ((search-backward "&" beg t)
      (goto-char p)
      (setq hline (search-backward "\\hline" beg t))
      (setq space (if (search-backward "\t&" beg t) "\t" " "))
      (goto-char p))
     (t ;;(insert "\\hline\n")
	(setq space " ")))
    (goto-char p)
    (while (> (1- cols) 0)
      (insert "&" space)
      (setq cols (1- cols)))
    (insert "\\\\")
    (if hline (insert " \\hline"))
    (goto-char p)
    (YaTeX-indent-line)))

(defun YaTeX-intelligent-newline-tabular* ()
  "Parse current tabular* format and insert that many `&'s."
  (YaTeX-intelligent-newline-tabular 'tabular*))

(fset 'YaTeX-intelligent-newline-array 'YaTeX-intelligent-newline-tabular)
(fset 'YaTeX-intelligent-newline-supertabular 'YaTeX-intelligent-newline-tabular)

(defun YaTeX-intelligent-newline-alignat ()
  (YaTeX-intelligent-newline-tabular 'alignat))
(fset 'YaTeX-intelligent-newline-alignat* 'YaTeX-intelligent-newline-alignat)

(defun YaTeX-intelligent-newline-align ()
  "Intelligent newline function for align.
Count the number of & in the first align line and insert that many &s."
  (let*((p (point)) (cols 0))
    (save-excursion
      (YaTeX-beginning-of-environment)
      (catch 'done
	(while (YaTeX-re-search-active-forward
		"\\(&\\)\\|\\(\\\\\\\\\\)" YaTeX-comment-prefix p t)
	  (if (match-beginning 1) (setq cols (1+ cols)) (throw 'done t)))))
    (save-excursion
      (if (= cols 0)
	  (insert "&")
	(while (>= (setq cols (1- cols)) 0)
	  (insert "& "))))
    (YaTeX-indent-line)))

(mapcar
 '(lambda (s)
    (fset (intern (concat  "YaTeX-intelligent-newline-"
			   (symbol-name s)))
	  'YaTeX-intelligent-newline-align))
 '(align* flalign  flalign* matrix pmatrix bmatrix Bmatrix vmatrix Vmatrix
   cases))

;;;
;; Functions for tabbing environment
;;;
(defun YaTeX-intelligent-newline-tabbing ()
  "Check the number of \\= in the first line and insert that many \\>."
  (let ((p (point)) begenv tabcount)
    (save-excursion
      (YaTeX-beginning-of-environment)
      (setq begenv (point-end-of-line))
      (if (YaTeX-search-active-forward "\\\\" YaTeX-comment-prefix p t)
	  (progn
	    (setq tabcount 0)
	    (while (> (point) begenv)
	      (if (search-backward "\\=" begenv 1)
		  (setq tabcount (1+ tabcount)))))))
    (YaTeX-indent-line)
    (if tabcount
	(progn
	  (save-excursion
	    (while (> tabcount 0)
	      (insert "\\>\t")
	      (setq tabcount (1- tabcount))))
	  (forward-char 2))
      (insert "\\= \\\\")
      (forward-char -5)))
)

;;;
;; Functions for itemize/enumerate/list environments
;;;

(defun YaTeX-intelligent-newline-itemize ()
  "Insert '\\item '."
  (insert "\\item ")
  (YaTeX-indent-line)
)
(fset 'YaTeX-intelligent-newline-enumerate 'YaTeX-intelligent-newline-itemize)
(fset 'YaTeX-intelligent-newline-list 'YaTeX-intelligent-newline-itemize)

(defun YaTeX-intelligent-newline-description ()
  (insert "\\item[] ")
  (forward-char -2)
  (YaTeX-indent-line)
)

(defun YaTeX-intelligent-newline-thebibliography ()
  "Insert '\\bibitem '."
  (YaTeX-indent-line)
  (YaTeX-make-section nil nil nil "bibitem")
  (YaTeX-indent-line)
)

;;;
;; Intelligent newline
;;;
;;;###autoload
(defun YaTeX-intelligent-newline (arg)
  "Insert newline and environment-specific entry.
`\\item'	for some itemizing environment,
`\\> \\> \\'	for tabbing environemnt,
`& & \\ \hline'	for tabular environment."
  (interactive "P")
  (let*(env func)
    (end-of-line)
    (setq env (YaTeX-inner-environment))
    (if arg (setq env (YaTeX-read-environment "For what environment? ")))
    (setq func (intern-soft (concat "YaTeX-intelligent-newline-" env)))
    (end-of-line)
    (newline)
    (undo-boundary)
    (if (and env func (fboundp func))
	(funcall func))))

;;;
;; Environment-specific line indenting functions
;;;
;;;###autoload
(defun YaTeX-indent-line-equation ()
  "Indent a line in equation family."
  (let ((p (point)) (l-r 0) right-p peol depth (mp YaTeX-environment-indent))
    (if (save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (looking-at "\\\\right\\b"))
	(progn (YaTeX-reindent
		(save-excursion (YaTeX-goto-corresponding-leftright)
				(- (current-column) 0))))
      (save-excursion
	(forward-line -1)
	(while (and (not (bobp)) (YaTeX-on-comment-p))
	  (forward-line -1))
	;;(beginning-of-line)	;must be unnecessary
	(skip-chars-forward " \t")
	(if (eolp) (error "Math-environment can't have a null line!!"))
	(setq depth (current-column)
	      peol (point-end-of-line))
	(while (re-search-forward
		"\\\\\\(\\(left\\)\\|\\(right\\)\\)\\b" peol t)
	  (setq l-r (+ l-r (if (match-beginning 2) 1 -1))))
	(cond
	 ((progn (beginning-of-line)
		 (re-search-forward "\\\\\\\\\\s *$" (point-end-of-line) t))
	  ;;If previous line has `\\', this indentation is always normal.
	  (setq depth (+ (YaTeX-current-indentation) mp)))
	 ((> l-r 0)
	  (beginning-of-line)
	  (search-forward "\\left" peol nil l-r)
	  (goto-char (1+ (match-beginning 0)))
	  (setq depth (current-column)))
	 ((< l-r 0)
	  (goto-char (match-beginning 0))	;should be \right
	  (YaTeX-goto-corresponding-leftright)
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  ;(setq depth (+ (current-column) mp)) ;+mp is good?
	  (setq depth (current-column)))
	 (t				;if \left - \right = 0
	  (cond
	   ((re-search-forward "\\\\\\\\\\s *$" peol t)
	    (setq depth (+ (YaTeX-current-indentation) mp)))
	   ((re-search-forward "\\\\end{" peol t)
	    nil)			;same indentation as previous line's
	   ((re-search-forward "\\\\begin{" peol t)
	    (setq depth (+ depth mp)))
	   (t
	    (or (bobp) (forward-line -1))
	    (cond
	     ((re-search-forward
	       "\\\\\\\\\\s *$\\|\\\\begin{" (point-end-of-line) t)
	      (setq depth (+ depth mp)))
	     )))))
	(goto-char p))
      (YaTeX-reindent depth))))

;;;###autoload
(defun YaTeX-goto-corresponding-leftright ()
  "Go to corresponding \left or \right."
  (let ((YaTeX-struct-begin "\\left%1")
	(YaTeX-struct-end "\\right%1")
	(YaTeX-struct-name-regexp "[][(){}\\.|]")
	(in-leftright-p t))
    (YaTeX-goto-corresponding-environment t)))

;;;
;; Functions for formatting region being enclosed with environment
;;;
; These functions must take two argument; region-beginning, region-end.

(defun YaTeX-enclose-equation (beg end)
  (goto-char beg)
  (save-restriction
    (let (m0 bsl)
      (narrow-to-region beg end)
      (while (YaTeX-re-search-active-forward
	      "\\(\\$\\)" YaTeX-comment-prefix nil t)
	(goto-char (setq m0 (match-beginning 0)))
	(setq bsl 0)
	(if (and (not (bobp)) (= (char-after (1- (point))) ?\\ ))
	    (while (progn (forward-char -1) (= (char-after (point)) ?\\ ))
	      (setq bsl (1+ bsl))))
	(goto-char m0)
	(if (= 0 (% bsl 2))
	    (delete-char 1)
	  (forward-char 1))))))

(fset 'YaTeX-enclose-eqnarray 'YaTeX-enclose-equation)
(fset 'YaTeX-enclose-eqnarray* 'YaTeX-enclose-equation)

(defun YaTeX-enclose-verbatim (beg end)) ;do nothing when enclose verbatim
(fset 'YaTeX-enclose-verbatim* 'YaTeX-enclose-verbatim)

(provide 'yatexenv)
