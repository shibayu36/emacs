;;; -*- Emacs-Lisp -*-
;;; YaTeX add-in functions.
;;; yatexadd.el rev.18
;;; (c)1991-2006 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Fri Sep 21 11:44:42 2007 on firestorm
;;; $Id: yatexadd.el,v 1.74 2009/09/28 01:54:43 yuuji Rel $

;;;
;;Sample functions for LaTeX environment.
;;;
(defvar YaTeX:tabular-default-rule
  "@{\\vrule width 1pt\\ }c|c|c@{\\ \\vrule width 1pt}"
  "*Your favorite default rule format.")

(defvar YaTeX:tabular-thick-vrule "\\vrule width %s"
  "*Vertical thick line format (without @{}). %s'll be replaced by its width.")

(defvar YaTeX:tabular-thick-hrule "\\noalign{\\hrule height %s}"
  "*Horizontal thick line format.  %s will be replaced by its width.")

(defun YaTeX:tabular ()
  "YaTeX add-in function for tabular environment.
Notice that this function refers the let-variable `env' in
YaTeX-make-begin-end."
  (let ((width "") bars (rule "") (and "") (j 1) loc ans (hline "\\hline"))
    (if (string= YaTeX-env-name "tabular*")
	(setq width (concat "{" (read-string "Width: ") "}")))
    (setq loc (YaTeX:read-position "tb")
	  bars (string-to-int
		(read-string "Number of columns(0 for default format): " "3")))
    (if (<= bars 0)
	(setq				;if 0, simple format
	 rule YaTeX:tabular-default-rule
	 and "& &")
      (while (< j bars)			;repeat bars-1 times
	(setq rule (concat rule "c|")
	      and (concat and "& ")
	      j (1+ j)))
      (setq rule (concat rule "c"))
      (message "(N)ormal-frame or (T)hick frame? [nt]")
      (setq ans (read-char))
      (cond
       ((or (equal ans ?t) (equal ans ?T))
	(setq ans (read-string "Rule width: " "1pt")
	      rule (concat
		    "@{" (format YaTeX:tabular-thick-vrule ans) "}"
		    rule
		    "@{\\ " (format YaTeX:tabular-thick-vrule ans) "}")
	      hline (format YaTeX:tabular-thick-hrule ans)))
       (t (setq rule (concat "|" rule "|")
		hline "\\hline"))))

    (setq rule (read-string "rule format: " rule))
    (setq YaTeX-single-command "hline")

    (format "%s%s{%s}" width loc rule)))

(fset 'YaTeX:tabular* 'YaTeX:tabular)
(fset 'YaTeX:supertabular 'YaTeX:tabular)
(defun YaTeX:alignat ()
  (concat "{" (read-string "Number of columns: ") "}"))
(defun YaTeX:array ()
  (concat (YaTeX:read-position "tb")
	  "{" (read-string "Column format: ") "}"))
(defun YaTeX:subequations ()
  (message (if YaTeX-japan "分かりやすいコメントに変えるとref補完が楽よ"
	     "Changing comment string reduces effort at `ref' completion"))
  (concat " " YaTeX-comment-prefix
	  (YaTeX::ref-default-label "%H:%M")
	  (if YaTeX-japan "の式群" "equations")))

(defun YaTeX:read-oneof (oneof &optional quick allow-dup)
  (let ((pos "") loc (guide ""))
    (and (boundp 'name) name (setq guide (format "%s " name)))
    (catch 'quick
      (while (not (string-match
		   (setq loc (read-key-sequence
			      (format "%s position (`%s') [%s]: "
				      guide oneof pos));name is in YaTeX-addin
			 loc (if (fboundp 'events-to-keys)
				 (events-to-keys loc) loc))
		   "\r\^g\n"))
	(cond
	 ((string-match loc oneof)
	  (if (or allow-dup (not (string-match loc pos)))
	      (setq pos (concat pos loc)))
	  (if quick (throw 'quick t)))
	 ((and (string-match loc "\C-h\C-?") (> (length pos) 0))
	  (setq pos (substring pos 0 (1- (length pos)))))
	 (t
	  (ding)
	  (message "Please input one of `%s'." oneof)
	  (sit-for 3)))))
    (message "")
    pos))

(defun YaTeX:read-position (oneof)
  "Read a LaTeX (optional) position format such as `[htbp]'."
  (let ((pos (YaTeX:read-oneof oneof)))
    (if (string= pos "")  "" (concat "[" pos "]"))))

(defun YaTeX:table ()
  "YaTeX add-in function for table environment."
  (cond
   ((eq major-mode 'yatex-mode)
    (setq YaTeX-env-name "tabular"
	  YaTeX-section-name "caption")
    (YaTeX:read-position "htbp"))
   ((eq major-mode 'texinfo-mode)
    (concat " "
	    (completing-read
	     "Highlights with: "
	     '(("@samp")("@kbd")("@code")("@asis")("@file")("@var"))
	     nil nil "@")))))

(fset 'YaTeX:figure 'YaTeX:table)
(fset 'YaTeX:figure* 'YaTeX:table)


(defun YaTeX:description ()
  "Truly poor service:-)"
  (setq YaTeX-single-command "item[]")
  "")

(defun YaTeX:itemize ()
  "It's also poor service."
  (setq YaTeX-single-command "item")
  "")

(fset 'YaTeX:enumerate 'YaTeX:itemize)

(defun YaTeX:picture ()
  "Ask the size of coordinates of picture environment."
  (concat (YaTeX:read-coordinates "Picture size")
	  (YaTeX:read-coordinates "Initial position")))

(defun YaTeX:equation ()
  (YaTeX-jmode-off)
  (if (fboundp 'YaTeX-toggle-math-mode)
      (YaTeX-toggle-math-mode t)))		;force math-mode ON.

(mapcar '(lambda (f) (fset f 'YaTeX:equation))
	'(YaTeX:eqnarray YaTeX:eqnarray* YaTeX:align YaTeX:align*
	  YaTeX:split YaTeX:multline YaTeX:multline* YaTeX:gather YaTeX:gather*
	  YaTeX:aligned* YaTeX:gathered YaTeX:gathered*
	  YaTeX:alignat YaTeX:alignat* YaTeX:xalignat YaTeX:xalignat*
	  YaTeX:xxalignat YaTeX:xxalignat*))

(defun YaTeX:alignat ()
  (YaTeX:equation)
  (concat "{" (read-string "Number of cols: ") "}"))



(defun YaTeX:list ()
  "%\n{} %default label\n{} %formatting parameter")

(defun YaTeX:minipage ()
  (concat (YaTeX:read-position "cbt")
	  "{" (read-string "Width: ") "}"))

(defun YaTeX:thebibliography ()
  (setq YaTeX-section-name "bibitem")
  (concat "{" (read-string "Longest label: ") "}"))

(defun YaTeX:multicols ()
  (concat "{" (read-string "Number of columns: ") "}"))

;;;
;;Sample functions for section-type command.
;;;
(defun YaTeX:multiput ()
  (concat (YaTeX:read-coordinates "Pos")
	  (YaTeX:read-coordinates "Step")
	  "{" (read-string "How many times: ") "}"))

(defun YaTeX:put ()
  (YaTeX:read-coordinates "Pos"))

(defun YaTeX:makebox ()
  (cond
   ((YaTeX-in-environment-p "picture")
    (concat (YaTeX:read-coordinates "Dimension")
	    (YaTeX:read-position "lsrtb")))
   (t
    (let ((width (read-string "Width: ")))
      (if (string< "" width)
	  (progn
	    (or (equal (aref width 0) ?\[)
		(setq width (concat "[" width "]")))
	    (concat width (YaTeX:read-position
			   (if YaTeX-use-LaTeX2e "lrs" "lr")))))))))

;; (defun YaTeX:framebox ()
;;   (if (YaTeX-quick-in-environment-p "picture")
;;       (YaTeX:makebox)))
(fset 'YaTeX:framebox 'YaTeX:makebox)

(defun YaTeX:parbox ()
  (YaTeX:read-position "tbc"))

(defun YaTeX:dashbox ()
  (concat "{" (read-string "Dash dimension: ") "}"
	  (YaTeX:read-coordinates "Dimension")))

(defun YaTeX:savebox (argp)
  (cond
   ((= argp 1) (read-string "Saved into name: " "\\"))
   ((= argp 2) (read-string "Text: "))))

(defvar YaTeX-minibuffer-quick-map nil)
(if YaTeX-minibuffer-quick-map nil
  (setq YaTeX-minibuffer-quick-map
	(copy-keymap minibuffer-local-completion-map))
  (let ((ch (1+ ? )))
    (while (< ch 127)
      (define-key YaTeX-minibuffer-quick-map (char-to-string ch)
	'YaTeX-minibuffer-quick-complete)
      (setq ch (1+ ch)))))

(defvar YaTeX:left-right-delimiters
   '(("(" . ")") (")" . "(") ("[" . "]") ("]" . "[")
     ("\\{" . "\\}") ("\\}" . "\\{") ("|") ("\\|")
     ("\\lfloor" . "\\rfloor") ("\\lceil" . "\\rceil")
     ("\\langle" . "\\rangle") ("/") (".")
     ("\\rfloor" . "\\rfloor") ("\\rceil" . "\\lceil")
     ("\\rangle" . "\\langle") ("\\backslash")
     ("\\uparrow") ("\\downarrow") ("\\updownarrow") ("\\Updownarrow"))
   "TeX math delimiter, which can be completed after \\right or \\left.")

(defvar YaTeX:left-right-default nil "Default string of YaTeX:right.")

(defun YaTeX:left ()
  (let ((minibuffer-completion-table YaTeX:left-right-delimiters)
	delimiter (leftp (string= YaTeX-single-command "left")))
    (setq delimiter
	  (read-from-minibuffer
	   (format "Delimiter%s: "
		   (if YaTeX:left-right-default
		       (format "(default=`%s')" YaTeX:left-right-default)
		     "(SPC for menu)"))
	   nil YaTeX-minibuffer-quick-map))
    (if (string= "" delimiter) (setq delimiter YaTeX:left-right-default))
    (setq YaTeX-single-command (if leftp "right" "left")
	  YaTeX:left-right-default
	  (or (cdr (assoc delimiter YaTeX:left-right-delimiters)) delimiter))
    delimiter))

(fset 'YaTeX:right 'YaTeX:left)

(defun YaTeX:langle ()
  (setq YaTeX-single-command "rangle")
  nil)

(defun YaTeX:read-coordinates (&optional mes varX varY)
  (concat
   "("
   (read-string (format "%s %s: " (or mes "Dimension") (or varX "X")))
   ","
   (read-string (format "%s %s: " (or mes "Dimension") (or varY "Y")))
   ")"))

(defun YaTeX:itembox ()
  (concat "{" (read-string "Item heading string: ") "}"))

;;;
;;Sample functions for maketitle-type command.
;;;
(defun YaTeX:sum ()
  "Read range of summation."
  (YaTeX:check-completion-type 'maketitle)
  (concat (YaTeX:read-boundary "_") (YaTeX:read-boundary "^")))

(fset 'YaTeX:int 'YaTeX:sum)

(defun YaTeX:lim ()
  "Insert limit notation of \\lim."
  (YaTeX:check-completion-type 'maketitle)
  (let ((var (read-string "Variable: ")) limit)
    (if (string= "" var) ""
      (setq limit (read-string "Limit ($ means infinity): "))
      (if (string= "$" limit) (setq limit "\\infty"))
      (concat "_{" var " \\rightarrow " limit "}"))))

(defun YaTeX:gcd ()
  "Add-in function for \\gcd(m,n)."
  (YaTeX:check-completion-type 'maketitle)
  (YaTeX:read-coordinates "\\gcd" "(?,)" "(,?)"))

(defun YaTeX:read-boundary (ULchar)
  "Read boundary usage by _ or ^.  _ or ^ is indicated by argument ULchar."
  (let ((bndry (read-string (concat ULchar "{???} ($ for infinity): "))))
    (if (string= bndry "") ""
      (if (string= bndry "$") (setq bndry "\\infty"))
      (concat ULchar "{" bndry "}"))))

(defun YaTeX:verb ()
  "Enclose \\verb's contents with the same characters."
  (let ((quote-char (read-string "Quoting char: " "|"))
	(contents (read-string "Quoted contents: ")))
    (concat quote-char contents quote-char)))

(fset 'YaTeX:verb* 'YaTeX:verb)

(defun YaTeX:footnotemark ()
  (setq YaTeX-section-name "footnotetext")
  nil)

(defun YaTeX:cite ()
  (let ((comment (read-string "Comment for citation: ")))
    (if (string= comment "") ""
      (concat "[" comment "]"))))

(defun YaTeX:bibitem ()
  (let ((label (read-string "Citation label for bibitem: ")))
    (if (string= label "") ""
      (concat "[" label "]"))))

(defun YaTeX:item ()
  (cond
   ((eq major-mode 'yatex-mode)
    (YaTeX-indent-line)
    (setq YaTeX-section-name "label"))
   ((eq major-mode 'texinfo-mode)
    (setq YaTeX-section-name "dots"))) ;??
  " ")
(fset 'YaTeX:item\[\] 'YaTeX:item)
(fset 'YaTeX:subitem 'YaTeX:item)
(fset 'YaTeX:subsubitem 'YaTeX:item)

(defun YaTeX:linebreak ()
  (let (obl)
    (message "Break strength 0,1,2,3,4 (default: 4): ")
    (setq obl (char-to-string (read-char)))
    (if (string-match "[0-4]" obl)
	(concat "[" obl "]")
      "")))
(fset 'YaTeX:pagebreak 'YaTeX:linebreak)

;;;
;;Subroutine
;;;

(defun YaTeX:check-completion-type (type)
  "Check valid completion type."
  (if (not (eq type YaTeX-current-completion-type))
      (error "This should be completed with %s-type completion." type)))


;;;
;;;		[[Add-in functions for reading section arguments]]
;;;
;; All of add-in functions for reading sections arguments should
;; take an argument ARGP that specify the argument position.
;; If argument position is out of range, nil should be returned,
;; else nil should NOT be returned.

;;
; Label selection
;;
(defvar YaTeX-label-menu-other
  (if YaTeX-japan "':他のバッファのラベル\n" "':LABEL IN OTHER BUFFER.\n"))
(defvar YaTeX-label-menu-repeat
  (if YaTeX-japan ".:直前の\\refと同じ\n" "/:REPEAT LAST \ref{}\n"))
(defvar YaTeX-label-menu-any
  (if YaTeX-japan "*:任意の文字列\n" "*:ANY STRING.\n"))
(defvar YaTeX-label-buffer "*Label completions*")
(defvar YaTeX-label-guide-msg "Select label and hit RETURN.")
(defvar YaTeX-label-select-map nil
  "Key map used in label selection buffer.")
(defun YaTeX::label-setup-key-map ()
  (if YaTeX-label-select-map nil
    (message "Setting up label selection mode map...")
    ;(setq YaTeX-label-select-map (copy-keymap global-map))
    (setq YaTeX-label-select-map (make-keymap))
    (suppress-keymap YaTeX-label-select-map)
    (substitute-all-key-definition
     'previous-line 'YaTeX::label-previous YaTeX-label-select-map)
    (substitute-all-key-definition
     'next-line 'YaTeX::label-next YaTeX-label-select-map)
    (define-key YaTeX-label-select-map "\C-n"	'YaTeX::label-next)
    (define-key YaTeX-label-select-map "\C-p"	'YaTeX::label-previous)
    (define-key YaTeX-label-select-map "<"	'beginning-of-buffer)
    (define-key YaTeX-label-select-map ">"	'end-of-buffer)
    (define-key YaTeX-label-select-map "\C-m"	'exit-recursive-edit)
    (define-key YaTeX-label-select-map "\C-j"	'exit-recursive-edit)
    (define-key YaTeX-label-select-map " "	'exit-recursive-edit)
    (define-key YaTeX-label-select-map "\C-g"	'abort-recursive-edit)
    (define-key YaTeX-label-select-map "/"	'isearch-forward)
    (define-key YaTeX-label-select-map "?"	'isearch-backward)
    (define-key YaTeX-label-select-map "'"	'YaTeX::label-search-tag)
    (define-key YaTeX-label-select-map "."	'YaTeX::label-search-tag)
    (define-key YaTeX-label-select-map "*"	'YaTeX::label-search-tag)
    (message "Setting up label selection mode map...Done")
    (let ((key ?A))
      (while (<= key ?Z)
	(define-key YaTeX-label-select-map (char-to-string key)
	  'YaTeX::label-search-tag)
	(define-key YaTeX-label-select-map (char-to-string (+ key (- ?a ?A)))
	  'YaTeX::label-search-tag)
	(setq key (1+ key))))))

(defun YaTeX::label-next ()
  (interactive) (forward-line 1) (message YaTeX-label-guide-msg))
(defun YaTeX::label-previous ()
  (interactive) (forward-line -1) (message YaTeX-label-guide-msg))
(defun YaTeX::label-search-tag ()
  (interactive)
  (let ((case-fold-search t)
	(tag (regexp-quote (char-to-string last-command-char))))
    (cond
     ((save-excursion
	(forward-char 1)
	(re-search-forward (concat "^" tag) nil t))
      (goto-char (match-beginning 0)))
     ((save-excursion
	(goto-char (point-min))
	(re-search-forward (concat "^" tag) nil t))
      (goto-char (match-beginning 0))))
    (message YaTeX-label-guide-msg)))

; (defun YaTeX::ref (argp &optional labelcmd refcmd)
;   (cond
;    ((= argp 1)
;     (let ((lnum 0) e0 label label-list (buf (current-buffer))
; 	  (labelcmd (or labelcmd "label")) (refcmd (or refcmd "ref"))
; 	  (p (point)) initl line cf)
;       (message "Collecting labels...")
;       (save-window-excursion
; 	(YaTeX-showup-buffer
; 	 YaTeX-label-buffer (function (lambda (x) (window-width x))))
; 	(if (fboundp 'select-frame) (setq cf (selected-frame)))
; 	(if (eq (window-buffer (minibuffer-window)) buf)
; 	    (progn
; 	      (other-window 1)
; 	      (setq buf (current-buffer))
; 	      (set-buffer buf)
; 	      ;(message "cb=%s" buf)(sit-for 3)
; 	      ))
; 	(save-excursion
; 	  (set-buffer (get-buffer-create YaTeX-label-buffer))
; 	  (setq buffer-read-only nil)
; 	  (erase-buffer))
; 	(save-excursion
; 	  (goto-char (point-min))
; 	  (let ((standard-output (get-buffer YaTeX-label-buffer)))
; 	    (princ (format "=== LABELS in [%s] ===\n" (buffer-name buf)))
; 	    (while (YaTeX-re-search-active-forward
; 		    (concat "\\\\" labelcmd "\\b")
; 		    (regexp-quote YaTeX-comment-prefix) nil t)
; 	      (goto-char (match-beginning 0))
; 	      (skip-chars-forward "^{")
; 	      (setq label
; 		    (buffer-substring
; 		     (1+ (point))
; 		     (prog2 (forward-list 1) (setq e0 (1- (point)))))
; 		    label-list (cons label label-list))
; 	      (or initl
; 		  (if (< p (point)) (setq initl lnum)))
; 	      (beginning-of-line)
; 	      (skip-chars-forward " \t\n" nil)
; 	      (princ (format "%c:{%s}\t<<%s>>\n" (+ (% lnum 26) ?A) label
; 			     (buffer-substring (point) (point-end-of-line))))
; 	      (setq lnum (1+ lnum))
; 	      (message "Collecting \\%s{}... %d" labelcmd lnum)
; 	      (goto-char e0))
; 	    (princ YaTeX-label-menu-other)
; 	    (princ YaTeX-label-menu-repeat)
; 	    (princ YaTeX-label-menu-any)
; 	    );standard-output
; 	  (goto-char p)
; 	  (or initl (setq initl lnum))
; 	  (message "Collecting %s...Done" labelcmd)
; 	  (if (fboundp 'select-frame) (select-frame cf))
; 	  (YaTeX-showup-buffer YaTeX-label-buffer nil t)
; 	  (YaTeX::label-setup-key-map)
; 	  (setq truncate-lines t)
; 	  (setq buffer-read-only t)
; 	  (use-local-map YaTeX-label-select-map)
; 	  (message YaTeX-label-guide-msg)
; 	  (goto-line (1+ initl)) ;goto recently defined label line
; 	  (switch-to-buffer (current-buffer))
; 	  (unwind-protect
; 	      (progn
; 		(recursive-edit)
; 		(set-buffer (get-buffer YaTeX-label-buffer)) ;assertion
; 		(beginning-of-line)
; 		(setq line (1- (count-lines (point-min)(point))))
; 		(cond
; 		 ((= line -1) (setq label ""))
; 		 ((= line lnum) (setq label (YaTeX-label-other)))
; 		 ((= line (1+ lnum))
; 		  (save-excursion
; 		    (switch-to-buffer buf)
; 		    (goto-char p)
; 		    (if (re-search-backward
; 			 (concat "\\\\" refcmd "{\\([^}]+\\)}") nil t)
; 			(setq label (YaTeX-match-string 1))
; 		      (setq label ""))))
; 		 ((>= line (+ lnum 2))
; 		  (setq label (read-string (format "\\%s{???}: " refcmd))))
; 		 (t (setq label (nth (- lnum line 1) label-list)))))
; 	    (bury-buffer YaTeX-label-buffer)))
; 	label)))))

(defvar YaTeX-ref-default-label-string "%H%M%S_%d%b%y"
  "*Default \\ref time string format.
This format is like strftime(3) but allowed conversion char are as follows;
%y -> Last 2 digit of year,  %b -> Month name,  %m -> Monthe number(1-12),
%d -> Day,  %H -> Hour,  %M -> Minute,  %S -> Second,
%qx -> alphabetical-decimal conversion of yymmdd.
%qX -> alphabetical-decimal conversion of HHMMSS.
Beware defualt label-string should be always unique.  So this format string
should have both time part (%H+%M+%S or %qX) and date
part (%y+(%b|%m)+%d or %qx).")

(defun YaTeX::ref-alphabex (n)
  (let ((alphabex ""))
    (while (> n 0)
      (setq alphabex (concat (char-to-string (+ ?a (% n 26))) alphabex)
	    n (/ n 26)))
    alphabex))

(defun YaTeX::ref-default-label (&optional format)
  "Default auto-genarated label string."
  ;; We do not use (format-time-string) for emacs-19
  (let*((ts (substring (current-time-string) 4))
	(y (substring ts -2))
	(b (substring ts 0 3))
	(d (format "%d" (string-to-int (substring ts 4 6))))
	(H (substring ts 7 9))
	(M (substring ts 10 12))
	(S (substring ts 13 15))
	(HMS (+ (* 10000 (string-to-int H))
		(* 100 (string-to-int M))
		(string-to-int S)))
	(talphabex (YaTeX::ref-alphabex HMS))
	(mnames "JanFebMarAprMayJunJulAugSepOctNovDec")
	(m (format "%02d" (/ (string-match b mnames) 3)))
	(ymd (+ (* 10000 (string-to-int y))
		(* 100 (string-to-int m))
		(string-to-int d)))
	(dalphabex (YaTeX::ref-alphabex ymd)))
    (YaTeX-replace-formats
     (or format YaTeX-ref-default-label-string)
     (list (cons "y" y)
	   (cons "b" b)
	   (cons "m" m)
	   (cons "d" d)
	   (cons "H" H)
	   (cons "M" M)
	   (cons "S" S)
	   (cons "qX" talphabex)
	   (cons "qx" dalphabex)))))

(defvar YaTeX-ref-generate-label-function 'YaTeX::ref-generate-label
  "*Function to generate default label for unnamed \\label{}s.
The function pointed to this value should take two arguments.
First argument is LaTeX macro's name, second is macro's argument.")

(defun YaTeX::ref-generate-label (command arg)
  "Generate a label string which is unique in current buffer."
  (let ((default (condition-case nil
		     (YaTeX::ref-default-label)
		   (error (substring (current-time-string) 4)))))
    (read-string "Give a label for this line: "
		 (if YaTeX-emacs-19 (cons default 1) default))))

(defun YaTeX::ref-getset-label (buffer point &optional noset)
  "Get label string in the BUFFER near the POINT.
Make \\label{xx} if no label.
If optional third argument NOSET is non-nil, do not generate new label."
  ;;Here, we rewrite the LaTeX source.  Therefore we should be careful
  ;;to decide the location suitable for \label.  Do straightforward!
  (let (boundary inspoint cc newlabel (labelholder "label") mathp exp1 env
       (r-escape (regexp-quote YaTeX-comment-prefix))
       command arg alreadysought foundpoint)
    (set-buffer buffer)
    (save-excursion
      (goto-char point)
      (setq cc (current-column))
      (if (= (char-after (point)) ?\\) (forward-char 1))
      (cond
       ((looking-at YaTeX-sectioning-regexp)
	(setq command (YaTeX-match-string 0))
	(skip-chars-forward "^{")
	(setq arg (buffer-substring
		   (1+ (point))
		   (progn (forward-list 1) (1- (point)))))
	(skip-chars-forward " \t\n")
	;(setq boundary "[^\\]")
	(setq inspoint (point))
	(setq boundary
	      (save-excursion
		(if (YaTeX-re-search-active-forward
		     (concat YaTeX-ec-regexp
			     "\\(" YaTeX-sectioning-regexp "\\|"
			     "begin\\|item\\)")
		     r-escape nil 1)
		    (match-beginning 0)
		  (1- (point))))))
       ((looking-at "item\\s ")
	(setq command "item"
	      cc (+ cc 6))
	;(setq boundary (concat YaTeX-ec-regexp "\\(item\\|begin\\|end\\)\\b"))
	(setq boundary
	      (save-excursion
		(if (YaTeX-re-search-active-forward
		     (concat YaTeX-ec-regexp "\\(item\\|begin\\|end\\)\\b")
		     r-escape nil 1)
		    (match-beginning 0)
		  (1- (point))))
	      inspoint boundary))
       ((looking-at "bibitem")
	(setq labelholder "bibitem"	; label holder is bibitem itself
	      command "bibitem")
	(setq boundary
	      (save-excursion
		(if (YaTeX-re-search-active-forward
		     (concat YaTeX-ec-regexp "\\(bibitem\\|end\\)\\b")
		     r-escape nil 1)
		    (match-beginning 0)
		  (1- (point))))
	      inspoint boundary))
       ((string-match YaTeX::ref-nestable-counter-regexp
		      (setq env (or (YaTeX-inner-environment t) "document")))
	(let ((curtop (get 'YaTeX-inner-environment 'point))
	      (end (point-max)) label)
	  (skip-chars-forward " \t\n")
	  (setq inspoint (point)	;initial candidate
		cc (current-column)
		command env
		alreadysought t)
	  (if (condition-case nil
		  (progn
		    (goto-char curtop)
		    (YaTeX-goto-corresponding-environment))
		(error nil))
	      (setq end (point)))
	  (goto-char inspoint)
	  (while (YaTeX-re-search-active-forward
		  (concat YaTeX-ec-regexp "label{\\([^}]+\\)}" )
		  r-escape end t)
	    (setq label (YaTeX-match-string 1))
	    (if (and (equal env (YaTeX-inner-environment t))
		     (= curtop (get 'YaTeX-inner-environment 'point)))
		;;I found the label
		(setq alreadysought label
		      foundpoint (match-end 0))))
	  ))
       ((string-match YaTeX::ref-mathenv-regexp env) ;env is set in above case
	(setq command env
	      mathp t
	      exp1 (string-match YaTeX::ref-mathenv-exp1-regexp env))
	;;(setq boundary (concat YaTeX-ec-regexp "\\(\\\\\\|end{" env "}\\)"))
	(setq boundary
	      (save-excursion
		(if (YaTeX-re-search-active-forward
		     (concat
		      YaTeX-ec-regexp "\\("
		      (if exp1 "" "\\\\\\|")
		      "end{" env "}\\)")
		     r-escape nil 1)
		    (match-beginning 0)
		  (1- (point))))
	      inspoint boundary))
       ((looking-at "footnote\\s *{")
	(setq command "footnote")
	(skip-chars-forward "^{")	;move onto `{'
	(setq boundary
	      (save-excursion
		(condition-case err
		    (forward-list 1)
		  (error (error "\\\\footnote at point %s's brace not closed"
				(point))))
		(1- (point)))
	      inspoint boundary))
       ((looking-at "caption\\|\\(begin\\)")
	(setq command (YaTeX-match-string 0))
	(skip-chars-forward "^{")
	;;;;;;(if (match-beginning 1) (forward-list 1))
	;; caption can be treated as mathenv, is it right??
	(setq arg (buffer-substring
		   (1+ (point))
		   (progn (forward-list 1) (1- (point)))))
	;;(setq boundary (concat YaTeX-ec-regexp "\\(begin\\|end\\)\\b"))
	(setq inspoint (point))
	(setq boundary
	      (save-excursion
		(if (YaTeX-re-search-active-forward
		     (concat YaTeX-ec-regexp "\\(begin\\|end\\)\\b")
		     r-escape nil 1)
		    (match-beginning 0)
		(1- (point))))))
       (t ))
      (if (save-excursion (skip-chars-forward " \t") (looking-at "%"))
	  (forward-line 1))
      (cond
       ((stringp alreadysought)
	(put 'YaTeX::ref-getset-label 'foundpoint foundpoint) ;ugly...
	alreadysought)
       ((and (null alreadysought)
	     (> boundary (point))
	     (save-excursion
	       (YaTeX-re-search-active-forward
		;;(concat "\\(" labelholder "\\)\\|\\(" boundary "\\)")
		labelholder
		(regexp-quote YaTeX-comment-prefix)
		boundary 1))
	     (match-beginning 0))
	  ;; if \label{hoge} found, return it
	(put 'YaTeX::ref-getset-label 'foundpoint (1- (match-beginning 0)))
	(buffer-substring
	 (progn
	   (goto-char (match-end 0))
	   (skip-chars-forward "^{") (1+ (point)))
	 (progn
	   (forward-sexp 1) (1- (point)))))
	;;else make a label
	;(goto-char (match-beginning 0))
       (noset    nil)				;do not set label if noset
       (t
	(goto-char inspoint)
	(skip-chars-backward " \t\n")
	(save-excursion
	  (setq newlabel
		(funcall YaTeX-ref-generate-label-function command arg)))
	(delete-region (point) (progn (skip-chars-backward " \t") (point)))
	(if mathp nil 
	  (insert "\n")
	  (YaTeX-reindent cc))
	(insert (format "\\label{%s}" newlabel))
	newlabel)))))

(defvar YaTeX::ref-labeling-regexp-alist-default
  '(("\\\\begin{\\(java\\|program\\)}{\\([^}]+\\)}" . 2)
    ("\\\\label{\\([^}]+\\)}" . 1))
  "Alist of labeling regexp vs. its group number points to label string.
This alist is used in \\ref's argument's completion.")
(defvar YaTeX::ref-labeling-regexp-alist-private nil
  "*Private extesion to YaTeX::ref-labeling-regexp-alist.
See the documetation of YaTeX::ref-labeling-regexp-alist.")
(defvar YaTeX::ref-labeling-regexp-alist
  (append YaTeX::ref-labeling-regexp-alist-default
	  YaTeX::ref-labeling-regexp-alist-private))
(defvar YaTeX::ref-labeling-regexp
  (mapconcat 'car YaTeX::ref-labeling-regexp-alist "\\|"))
(defvar YaTeX::ref-mathenv-regexp
  ;; See also YaTeX-ams-math-begin-alist in yatex.el
  ;; Define only envs which has counter.(without *)
  "equation\\|eqnarray\\|align\\(at\\)?\\|flalign\\|gather\\|xx?alignat\\|multline")
(defvar YaTeX::ref-mathenv-exp1-regexp
  "\\(equation\\|multline\\)\\b"
  "*Regexp of math-envname which has only one math-expression.")
(defvar YaTeX::ref-enumerateenv-regexp
  "enumerate")
(defvar YaTeX::ref-nestable-counter-regexp
  "subequations")

(defvar YaTeX::ref-labeling-section-level 2
  "*ref補完で収集するセクショニングコマンドの下限レベル
YaTeX-sectioning-levelの数値で指定.")

(defun YaTeX::ref (argp &optional labelcmd refcmd predf)
  (setplist 'YaTeX::ref-labeling-regexp nil) ;erase memory cache
  (require 'yatexsec)
  (cond
   ((= argp 1)
    (let*((lnum 0) m0 e0 x cmd label match-point point-list boundary
	  (buf (current-buffer))
	  (llv YaTeX::ref-labeling-section-level)
	  (mathenvs YaTeX::ref-mathenv-regexp) envname endrx
	  (enums YaTeX::ref-enumerateenv-regexp)
	  (counter
	   (or labelcmd
	       (concat
		YaTeX-ec-regexp "\\(\\("
		(mapconcat
		 'concat
		 (delq nil
		       (mapcar
			(function
			 (lambda (s)
			   (if (>= llv (cdr s))
			       (car s))))
			YaTeX-sectioning-level))
		 "\\|")
		"\\|caption\\(\\[[^]]+\\]\\)?\\|footnote\\){"
		"\\|\\(begin{\\(" mathenvs "\\|" enums  "\\)}\\)"
		(if YaTeX-use-AMS-LaTeX
		    (concat
		     "\\|\\(begin{"
		     YaTeX::ref-nestable-counter-regexp "}\\)"))
		"\\)")))
	  (regexp (concat "\\(" counter
			  "\\)\\|\\(" YaTeX::ref-labeling-regexp "\\)"))
	  (itemsep (concat YaTeX-ec-regexp
			   "\\(\\(bib\\)?item\\|begin\\|end\\)"))
	  (refcmd (or refcmd "\\(page\\)?ref"))
	  (p (point)) initl line cf
	  (percent (regexp-quote YaTeX-comment-prefix))
	  (output
	   (function
	    (lambda (label p)
	      (while (setq x (string-match "[\n\t]" label))
		(aset label x ? ))
	      (while (setq x (string-match "  +" label))
		(setq label (concat
			     (substring label 0 (1+ (match-beginning 0)))
			     (substring label (match-end 0)))))
	      (princ (format "%c: <<%s>>\n" (+ (% lnum 26) ?A) label))
	      (setq point-list (cons p point-list))
	      (message "Collecting labels... %d" lnum)
	      (setq lnum (1+ lnum)))))
	  (me (if (boundp 'me) me 'YaTeX::ref))
	  )
      (message "Collecting labels...")
      (save-window-excursion
	(YaTeX-showup-buffer
	 YaTeX-label-buffer (function (lambda (x) (window-width x))))
	(if (fboundp 'select-frame) (setq cf (selected-frame)))
	(if (eq (window-buffer (minibuffer-window)) buf)
	    (progn
	      (other-window 1)
	      (setq buf (current-buffer))
	      (set-buffer buf)))
	(save-excursion
	  (set-buffer (get-buffer-create YaTeX-label-buffer))
	  (condition-case ()
	      (if (and YaTeX-use-font-lock (fboundp 'font-lock-mode))
		  (font-lock-mode 1))
	    (error nil))
	  (setq buffer-read-only nil)
	  (erase-buffer))
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-min))
	  (let ((standard-output (get-buffer YaTeX-label-buffer)) existlabel)
	    (princ (format "=== LABELS in [%s] ===\n" (buffer-name buf)))
	    (while (YaTeX-re-search-active-forward
		    regexp ;;counter
		    percent nil t)
	      ;(goto-char (match-beginning 0))
	      (setq e0 (match-end 0))
	      (cond
	       ;; 
	       ;;2005/10/21 Skip it if predicate function returns nil
	       ((and predf
		     (let ((md (match-data)))
		       (prog1
			   (condition-case nil
			       (not (funcall predf))
			     (error nil))
			 (store-match-data md)))))
	       ((YaTeX-literal-p) nil)
	       ((YaTeX-match-string 1)
		;;if standard counter commands found 
		(setq cmd (YaTeX-match-string 2)
		      m0 (match-beginning 0))
		(setq match-point (match-beginning 0))
		(or initl
		    (if (< p (point)) (setq initl lnum)))
		(cond
		 ;; In any case, variables e0 should be set
		 ((and YaTeX-use-AMS-LaTeX
		       (string-match YaTeX::ref-nestable-counter-regexp cmd))
		  (let (label)
		    (skip-chars-forward "}")
		    (setq label (buffer-substring
				 (point) (min (+ 80 (point)) (point-max))))
		    ;; to skip (maybe)auto-generated comment
		    (skip-chars-forward " \t")
		    (if (looking-at YaTeX-comment-prefix)
			(forward-line 1))
		    (setq e0 (point))
		    (skip-chars-forward " \t\n")
		    (if (looking-at "\\\\label{\\([^}]+\\)}")
			(setq label (format "(labe:%s)" (YaTeX-match-string 1))
			      e0 (match-end 1)))
		    (funcall output (format "--subequation--%s" label) e0)))
		 ((string-match mathenvs cmd) ;;if matches mathematical env
		  ;(skip-chars-forward "} \t\n")
		  ;(forward-line 1) ;2004/1/25
		  (skip-chars-forward "}")
		  (setq x (point)
			envname (substring
				 cmd (match-beginning 0) (match-end 0)))
		  (save-restriction
		    (narrow-to-region
		     m0
		     (save-excursion
		       (YaTeX-re-search-active-forward
			(setq endrx (format "%send{%s}" YaTeX-ec-regexp
					    (regexp-quote envname)))
			percent nil t)))
		    (catch 'scan
		      (while (YaTeX-re-search-active-forward
			      (concat
			       "\\\\end{\\(" (regexp-quote envname) "\\)";;(1)
			       (if YaTeX-use-AMS-LaTeX
				   "\\|\\\\\\(notag\\)") ;;2
			       (if (string-match
				    YaTeX::ref-mathenv-exp1-regexp  cmd)
				   "" "\\|\\\\\\\\$")
			       )
			      percent nil t)
			(let*((quit (match-beginning 1))
			      (notag (match-beginning 2))
			      (label ".......................") l2
			      (e (point)) (m0 (match-beginning 0))
			      (ln (YaTeX-string-width label)))
			  (cond
			   (notag
			    (YaTeX-re-search-active-forward
			     "\\\\\\\\" percent nil 1)
			    (setq x (point)))
			   (t
			    (if (YaTeX-re-search-active-backward
				 YaTeX::ref-labeling-regexp
				 percent x t)
				;; if \label{x} in math-expression, display it
				;; because formula source is hard to recognize
				(progn
				  (goto-char (match-end 0))
				  (setq l2 (format "\"label:%s\""
						   (buffer-substring
						    (1- (point))
						    (progn (forward-sexp -1)
							   (1+ (point))))))
				  (setq label
					(if (< (YaTeX-string-width l2) ln)
					    (concat
					     l2
					     (substring
					      label
					      0 (- ln (YaTeX-string-width l2))))
					  l2))
				  (goto-char e)))
			    (funcall output
				     (concat
				      label " "
				      (buffer-substring x m0))
				     x)
			    (cond
			     ((YaTeX-quick-in-environment-p
			       YaTeX-math-gathering-list)
			      ;; if here is inner split/cases/gathered env.,
			      ;; counter for here is only one.
			      ;; Go out this environment and,
			      (YaTeX-end-of-environment)
			      ;; search next expression unit boundary.
			      (YaTeX-re-search-active-forward
			       (concat endrx "\\|\\\\begin{")
			       percent nil 1)
			      (end-of-line)))
			    (if quit (throw 'scan t)))))
			(setq x (point)))))
		  (setq e0 (point)))
		 ((string-match enums cmd)
		  ;(skip-chars-forward "} \t\n")
		  (save-restriction
		    (narrow-to-region
		     (point)
		     (save-excursion
		       (YaTeX-goto-corresponding-environment) (point)))
		    (forward-line 1)
		    (while (YaTeX-re-search-active-forward
			    (concat YaTeX-ec-regexp "item\\s ")
			    percent nil t)
		      (setq x (match-beginning 0))
		      (funcall
		       output
		       (concat
			existlabel
			(buffer-substring
			 (match-beginning 0)
			 (if (re-search-forward itemsep nil t)
			     (progn (goto-char (match-beginning 0))
				    (skip-chars-backward " \t")
				    (1- (point)))
			   (point-end-of-line))))
		       x))
		    (setq e0 (point-max))))
		 ((string-match "bibitem" cmd) ;maybe generated by myself
		  (setq label "")
		  (skip-chars-forward " \t")
		  (if (looking-at "{")	;sure to be true!!
		      (forward-list 1))
		  (let ((list '(30 10 65))
			(delim ";") q lim len l str)
		    (save-excursion
		      (setq lim (if (re-search-forward itemsep nil 1)
				    (match-beginning 0) (point))))
		    (while list
		      (skip-chars-forward " \t\n\\")
		      (setq q (looking-at "[\"'{]")
			    len (car list)
			    str
			    (buffer-substring
			     (point)
			     (progn
			       (if q (forward-sexp 1)
				 (search-forward delim lim 1)
				 (forward-char -1))
			       (point))))
		      (if (> (setq l (YaTeX-string-width str)) len)
			  (setq str (concat
				     (YaTeX-truncate-string-width
				      str (- len (if q 5 4)))
				     "... "
				     (if q (substring str -1)))))
		      (if (< (setq l (YaTeX-string-width str)) len)
			  (setq str (concat str (make-string (- len l) ? ))))
		      (if (looking-at delim) (goto-char (match-end 0)))
		      (setq label (concat label " " str)
			    list (cdr list)))
		    (funcall output label match-point)))
		 ;;else, simple section-type counter
		 ((= (char-after (1- (point))) ?{)
		  (setq label (buffer-substring
			       (match-beginning 0)
			       (progn (forward-char -1)
				      (forward-list 1)
				      (point))))
		  (funcall output label match-point)
		  ;; Skip preceding label if exists
		  (if (YaTeX::ref-getset-label (current-buffer) match-point t)
		      (goto-char (get 'YaTeX::ref-getset-label 'foundpoint)))
		  (if (save-excursion
			(skip-chars-forward "\t \n")
			(looking-at YaTeX::ref-labeling-regexp))
		      (setq e0 (match-end 0))))
		 (t
		  (skip-chars-forward " \t")
		  (setq label (buffer-substring
			       (match-beginning 0)
			       (if (re-search-forward
				    itemsep
				    nil t)
				   (progn
				     (goto-char (match-beginning 0))
				     (skip-chars-backward " \t")
				     (1- (point)))
				 (point-end-of-line))))
		  (funcall output label match-point)
		  (if (save-excursion
			(skip-chars-forward "\t \n")
			(looking-at YaTeX::ref-labeling-regexp))
		      (setq e0 (match-end 0)))))
		) ;;put label buffer
	       ;;
	       ;; if user defined label found
	       (t
		;; memorize line number and label into property
		(goto-char (match-beginning 0))
		(let ((list YaTeX::ref-labeling-regexp-alist)
		      (cache (symbol-plist 'YaTeX::ref-labeling-regexp)))
		  (while list
		    (if (looking-at (car (car list)))
			(progn
			  (setq label (YaTeX-match-string 0))
			  (put 'YaTeX::ref-labeling-regexp lnum
			       (YaTeX-match-string (cdr (car list))))
			  (funcall output label 0) ;;0 is dummy, never used
			  (setq list nil)))
		    (setq list (cdr list))))
		))
	      (goto-char e0))
	    (princ YaTeX-label-menu-other)
	    (princ YaTeX-label-menu-repeat)
	    (princ YaTeX-label-menu-any)
	    );standard-output
	  (goto-char p)
	  (or initl (setq initl lnum))
	  (message "Collecting labels...Done")
	  (if (fboundp 'select-frame) (select-frame cf))
	  (YaTeX-showup-buffer YaTeX-label-buffer nil t)
	  (YaTeX::label-setup-key-map)
	  (setq truncate-lines t)
	  (setq buffer-read-only t)
	  (use-local-map YaTeX-label-select-map)
	  (message YaTeX-label-guide-msg)
	  (goto-line (1+ initl)) ;goto recently defined label line
	  (switch-to-buffer (current-buffer))
	  (unwind-protect
	      (progn
		(recursive-edit)

		(set-buffer (get-buffer YaTeX-label-buffer)) ;assertion
		(beginning-of-line)
		(setq line (1- (count-lines (point-min)(point))))
		(cond
		 ((= line -1) (setq label ""))
		 ((= line lnum) (setq label (YaTeX-label-other)))
		 ((= line (1+ lnum))
		  (save-excursion
		    (switch-to-buffer buf)
		    (goto-char p)
		    (if (re-search-backward
			 (concat "\\\\" refcmd "{") nil t)
			(setq label (YaTeX-buffer-substring
				     (progn (goto-char (1- (match-end 0)))
					    (1+ (point)))
				     (progn (forward-list 1)
					    (1- (point)))))
		      (setq label ""))))
		 ((>= line (+ lnum 2))
		  (setq label (read-string (format "\\%s{???}: " refcmd))))
		 (t ;(setq label (nth (- lnum line 1) label-list))
		  (setq label
			(or (get 'YaTeX::ref-labeling-regexp line)
			    (YaTeX::ref-getset-label
			     buf (nth (- lnum line 1) point-list))))
		  )))
	    (bury-buffer YaTeX-label-buffer)))
	label)))))

(fset 'YaTeX::pageref 'YaTeX::ref)
(defun YaTeX::tabref (argp)	    ; For the style file of IPSJ journal
  (YaTeX::ref
   argp nil nil
   (function
    (lambda ()
      (YaTeX-quick-in-environment-p "table")))))
(defun YaTeX::figref (argp)	    ; For the style file of IPSJ journal
  (YaTeX::ref
   argp nil nil
   (function
    (lambda ()
      (YaTeX-quick-in-environment-p "figure")))))

(defun YaTeX::cite-collect-bibs-external (bibptn &rest files)
  "Collect bibentry from FILES(variable length argument) ;
and print them to standard output."
  ;;Thanks; http://icarus.ilcs.hokudai.ac.jp/comp/biblio.html
  (let*((tb (get-buffer-create " *bibtmp*"))
	(bibitemsep "^\\s *@[A-Za-z]")
	(target (if (string< "" bibptn) bibptn bibitemsep))
	(checkrx (concat "\\(" bibptn "\\)\\|" bibitemsep))
	beg
	(searchnext
	 (if (string< "" bibptn)
	     (function
	      (lambda()
		(setq beg (point))
		(and
		 (prog1
		     (re-search-forward target nil t)
		   (end-of-line))
		 (re-search-backward bibitemsep beg t))))
	   (function
	    (lambda()
	      (re-search-forward target nil t)))))
	)
    (save-excursion
      (set-buffer tb)
      (princ (format "%sbegin{thebibliography}\n" YaTeX-ec))
      (while files
	(erase-buffer)
	(cond
	 ((file-exists-p (car files))
	  (insert-file-contents (car files)))
	 ((file-exists-p (concat (car files) ".bib"))
	  (insert-file-contents (concat (car files) ".bib"))))
	(save-excursion
	  (goto-char (point-min))
	  (while (funcall searchnext)
	    (skip-chars-forward "^{,")
	    (setq beg (point))
	    (if (= (char-after (point)) ?{)
		(princ (format "%sbibitem{%s}%s\n"
			       YaTeX-ec
			       (buffer-substring
				(1+ (point))
				(progn (skip-chars-forward "^,\n")
				       (point)))
			       (mapconcat
				(function
				 (lambda (kwd)
				   (goto-char beg)
				   (if (re-search-forward
					(concat kwd "\\s *=") nil t)
				       (buffer-substring
					(progn
					  (goto-char (match-end 0))
					  (skip-chars-forward " \t\n")
					  (point))
					(progn
					  (if (looking-at "[{\"]")
					      (forward-sexp 1)
					    (forward-char 1)
					    (skip-chars-forward "^,}"))
					  (point))))))
				'("author" "year" "title" )
				";"))))
	    (and (re-search-forward bibitemsep nil t)
		 (forward-line -1))))
	(setq files (cdr files)))
      (princ (format "%sbegin{thebibliography}\n" YaTeX-ec)))))

(defvar YaTeX::cite-bibitem-macro-regexp "bibitem\\|harvarditem"
  "*Regexp of macro name of bibitem definition")

(defun YaTeX::cite-collect-bibs-internal (bibptn)
  "Collect bibentry in the current buffer and print them to standard output."
  (let ((ptn (concat YaTeX-ec-regexp
		     "\\(" YaTeX::cite-bibitem-macro-regexp "\\)\\b"))
	(lim (concat YaTeX-ec-regexp
		     "\\(" YaTeX::cite-bibitem-macro-regexp "\\b\\)"
		     "\\|\\(end{\\)"))
	(pcnt (regexp-quote YaTeX-comment-prefix)))
    ;; Using bibptn not yet implemented.
    ;; Do you need it?? 2005/11/22
    (save-excursion
      (while (YaTeX-re-search-active-forward ptn pcnt nil t)
	(skip-chars-forward "^{\n")
	(or (eolp)
	    (princ (format "%sbibitem%s %s\n"
			   YaTeX-ec
			   (buffer-substring
			    (point)
			    (progn (forward-sexp 1) (point)))
			   (buffer-substring
			    (progn (skip-chars-forward "\n \t") (point))
			    (save-excursion
			      (if (YaTeX-re-search-active-forward
				   lim pcnt nil t)
				  (progn
				    (goto-char (match-beginning 0))
				    (skip-chars-backward "\n \t")
				    (point))
				(point-end-of-line)))))))))))

(defun YaTeX::cite (argp &rest dummy)
  (cond
   ((eq argp 1)
    (let* ((cb (current-buffer))
	   (f (file-name-nondirectory buffer-file-name))
	   (d default-directory)
	   (hilit-auto-highlight nil)
	   (pcnt (regexp-quote YaTeX-comment-prefix))
	   (bibrx (concat YaTeX-ec-regexp "bibliography{\\([^}]+\\)}"))
	   (bibptn (read-string "Pattern: "))
	   (bbuf (get-buffer-create " *bibitems*"))
	   (standard-output bbuf)
	   (me 'YaTeX::cite)		;shuld set this for using YaTeX::ref
	   bibs files)
      (set-buffer bbuf)(erase-buffer)(set-buffer cb)
      (save-excursion
	(goto-char (point-min))
	;;(1)search external bibdata
	(while (YaTeX-re-search-active-forward bibrx pcnt nil t)
	  (apply 'YaTeX::cite-collect-bibs-external
		 bibptn
		 (YaTeX-split-string
		  (YaTeX-match-string 1) ",")))
	;;(2)search direct \bibitem usage
	(YaTeX::cite-collect-bibs-internal bibptn)
	(if (progn
	      (YaTeX-visit-main t)
	      (not (eq (current-buffer) cb)))
	    (save-excursion
	      (goto-char (point-min))
	      ;;(1)search external bibdata
	      (while (YaTeX-re-search-active-forward bibrx pcnt nil t)
		(apply 'YaTeX::cite-collect-bibs-external
		       bibptn
		       (YaTeX-split-string
			(YaTeX-match-string 1) ",")))
	      ;;(2)search internal
	      (YaTeX::cite-collect-bibs-internal bibptn)))
	;;Now bbuf holds the list of bibitem
	(set-buffer bbuf)
	;;;(switch-to-buffer bbuf)
	(if (fboundp 'font-lock-fontify-buffer) (font-lock-fontify-buffer))
	(YaTeX::ref
	 argp 
	 (concat "\\\\\\("
		 YaTeX::cite-bibitem-macro-regexp
		 "\\)\\(\\[.*\\]\\)?")
	 "cite"))))

   (t nil)))

;;; for AMS-LaTeX
(and YaTeX-use-AMS-LaTeX (fset 'YaTeX::eqref 'YaTeX::ref))
;;; for Harvard citation style
(fset 'YaTeX::citeasnoun 'YaTeX::cite)
(fset 'YaTeX::possessivecite 'YaTeX::cite)
(fset 'YaTeX::citeyear 'YaTeX::cite)
(fset 'YaTeX::citename 'YaTeX::cite)
(fset 'YaTeX::citep 'YaTeX::cite)
(fset 'YaTeX::citet 'YaTeX::cite)

(defun YaTeX-yatex-buffer-list ()
  (save-excursion
    (delq nil (mapcar (function (lambda (buf)
				  (set-buffer buf)
				  (if (eq major-mode 'yatex-mode) buf)))
		      (buffer-list)))))

(defun YaTeX-select-other-yatex-buffer ()
  "Select buffer from all yatex-mode's buffers interactivelly."
  (interactive)
  (let ((lbuf "*YaTeX mode buffers*") (blist (YaTeX-yatex-buffer-list))
	(lnum -1) buf rv
	(ff "**find-file**"))
    (YaTeX-showup-buffer
     lbuf (function (lambda (x) 1)))	;;Select next window surely.
    (save-excursion
      (set-buffer (get-buffer lbuf))
      (setq buffer-read-only nil)
      (erase-buffer))
    (let ((standard-output (get-buffer lbuf)))
      (while blist
	(princ
	 (format "%c:{%s}\n" (+ (% (setq lnum (1+ lnum)) 26) ?A)
		 (buffer-name (car blist))))
	(setq blist (cdr blist)))
      (princ (format "':{%s}" ff)))
    (YaTeX-showup-buffer lbuf nil t)
    (YaTeX::label-setup-key-map)
    (setq buffer-read-only t)
    (use-local-map YaTeX-label-select-map)
    (message YaTeX-label-guide-msg)
    (unwind-protect
	(progn
	  (recursive-edit)
	  (set-buffer lbuf)
	  (beginning-of-line)
	  (setq rv
		(if (re-search-forward "{\\([^\\}]+\\)}" (point-end-of-line) t)
		    (buffer-substring (match-beginning 1) (match-end 1)) nil)))
      (kill-buffer lbuf))
    (if (string= rv ff)
	(progn
	  (call-interactively 'find-file)
	  (current-buffer))
      rv)))

(defun YaTeX-label-other ()
  (let ((rv (YaTeX-select-other-yatex-buffer)))
    (cond
     ((null rv) "")
     (t
      (set-buffer rv)
      (funcall me argp labelcmd refcmd)))))

;;
; completion for the arguments of \newcommand
;;
(defun YaTeX::newcommand (&optional argp)
  (cond
   ((= argp 1)
    (let ((command (read-string "Define newcommand: " "\\")))
      (put 'YaTeX::newcommand 'command (substring command 1))
      command))
   ((= argp 2)
    (let ((argc
	   (string-to-int (read-string "Number of arguments(Default 0): ")))
	  (def (read-string "Definition: "))
	  (command (get 'YaTeX::newcommand 'command)))
      ;;!!! It's illegal to insert string in the add-in function !!!
      (if (> argc 0) (insert (format "[%d]" argc)))
      (if (and (stringp command)
	       (string< "" command)
	       (y-or-n-p "Update dictionary?"))
	  (cond
	   ((= argc 0)
	    (YaTeX-update-table
	     (list command)
	     'singlecmd-table 'user-singlecmd-table 'tmp-singlecmd-table))
	   ((= argc 1)
	    (YaTeX-update-table
	     (list command)
	     'section-table 'user-section-table 'tmp-section-table))
	   (t (YaTeX-update-table
	       (list command argc)
	       'section-table 'user-section-table 'tmp-section-table))))
      (message "")
      def				;return command name
      ))
   (t "")))

(defun YaTeX::newcounter (&optional argp)
  (cond
   ((= argp 1)
    (read-string "New counter name: "))
   (t "")))

;;
; completion for the arguments of \pagestyle
;;
(defun YaTeX::pagestyle (&optional argp)
  "Read the pagestyle with completion."
  (completing-read
   "Page style: "
   '(("plain") ("empty") ("headings") ("myheadings") ("normal") nil)))

(fset 'YaTeX::thispagestyle 'YaTeX::pagestyle)

;;
; completion for the arguments of \pagenumbering
;;
(defun YaTeX::pagenumbering (&optional argp)
  "Read the numbering style."
  (completing-read
   "Page numbering style: "
   '(("arabic") ("Alpha") ("alpha") ("Roman") ("roman"))))

;;
; Length
;;
(defvar YaTeX:style-parameters-default
  '(("\\arraycolsep")
    ("\\arrayrulewidth")
    ("\\baselineskip")
    ("\\columnsep")
    ("\\columnseprule")
    ("\\doublerulesep")
    ("\\evensidemargin")
    ("\\footheight")
    ("\\footskip")
    ("\\headheight")
    ("\\headsep")
    ("\\itemindent")
    ("\\itemsep")
    ("\\labelsep")
    ("\\labelwidth")
    ("\\leftmargin")
    ("\\linewidth")
    ("\\listparindent")
    ("\\marginparsep")
    ("\\marginparwidth")
    ("\\mathindent")
    ("\\oddsidemargin")
    ("\\parindent")
    ("\\parsep")
    ("\\parskip")
    ("\\partopsep")
    ("\\rightmargin")
    ("\\tabcolsep")
    ("\\textheight")
    ("\\textwidth")
    ("\\topmargin")
    ("\\topsep")
    ("\\topskip")
    )
  "Alist of LaTeX style parameters.")
(defvar YaTeX:style-parameters-private nil
  "*User definable alist of style parameters.")
(defvar YaTeX:style-parameters-local nil
  "*User definable alist of local style parameters.")

(defvar YaTeX:length-history nil "Holds history of length.")
(put 'YaTeX:length-history 'no-default t)
(defun YaTeX::setlength (&optional argp)
  "YaTeX add-in function for arguments of \\setlength."
  (cond
   ((equal 1 argp)
    ;;(completing-read "Length variable: " YaTeX:style-parameters nil nil "\\")
    (YaTeX-cplread-with-learning
     "Length variable: "
     'YaTeX:style-parameters-default
     'YaTeX:style-parameters-private
     'YaTeX:style-parameters-local
     nil nil "\\")
    )
   ((equal 2 argp)
    (read-string-with-history "Length: " nil 'YaTeX:length-history))))

(fset 'YaTeX::addtolength 'YaTeX::setlength)

(defun YaTeX::settowidth (&optional argp)
  "YaTeX add-in function for arguments of \\settowidth."
  (cond
   ((equal 1 argp)
    (YaTeX-cplread-with-learning
     "Length variable: "
     'YaTeX:style-parameters-default
     'YaTeX:style-parameters-private
     'YaTeX:style-parameters-local
     nil nil "\\"))
   ((equal 2 argp)
    (read-string "Text: "))))

(defun YaTeX::newlength (&optional argp)
  "YaTeX add-in function for arguments of \\newlength"
  (cond
   ((equal argp 1)
    (let ((length (read-string "Length variable: " "\\")))
      (if (string< "" length)
	  (YaTeX-update-table
	   (list length)
	   'YaTeX:style-parameters-default
	   'YaTeX:style-parameters-private
	   'YaTeX:style-parameters-local))
      length))))

;; \multicolumn's arguments
(defun YaTeX::multicolumn (&optional argp)
  "YaTeX add-in function for arguments of \\multicolumn."
  (cond
   ((equal 1 argp)
    (read-string "Number of columns: "))
   ((equal 2 argp)
    (YaTeX:read-oneof "|lrc" nil t))
   ((equal 3 argp)
    (read-string "Item: "))))

(defvar YaTeX:documentstyles-default
  '(("article") ("jarticle") ("j-article")
    ("book") ("jbook") ("j-book")
    ("report") ("jreport") ("j-report")
    ("letter") ("ascjletter"))
  "List of LaTeX documentstyles.")
(defvar YaTeX:documentstyles-private nil
  "*User defined list of LaTeX documentstyles.")
(defvar YaTeX:documentstyles-local nil
  "*User defined list of local LaTeX documentstyles.")
(defvar YaTeX:documentstyle-options-default
  '(("a4j") ("a5j") ("b4j") ("b5j")
    ("twocolumn") ("jtwocolumn") ("epsf") ("epsfig") ("epsbox") ("nfig"))
  "List of LaTeX documentstyle options.")
(defvar YaTeX:documentstyle-options-private nil
  "*User defined list of LaTeX documentstyle options.")
(defvar YaTeX:documentstyle-options-local nil
  "List of LaTeX local documentstyle options.")

(defun YaTeX:documentstyle ()
  (let*((delim ",")
	(dt (append YaTeX:documentstyle-options-local
		    YaTeX:documentstyle-options-private
		    YaTeX:documentstyle-options-default))
	(minibuffer-completion-table dt)
	(opt (read-from-minibuffer
	      "Style options ([opt1,opt2,...]): "
	      nil YaTeX-minibuffer-completion-map nil))
	(substr opt) o)
    (if (string< "" opt)
	(progn
	  (while substr
	    (setq o (substring substr 0 (string-match delim substr)))
	    (or (assoc o dt)
		(YaTeX-update-table
		 (list o)
		 'YaTeX:documentstyle-options-default
		 'YaTeX:documentstyle-options-private
		 'YaTeX:documentstyle-options-local))
	    (setq substr
		  (if (string-match delim substr)
		      (substring substr (1+ (string-match delim substr))))))
	  (concat "[" opt "]"))
      "")))

(defun YaTeX::documentstyle (&optional argp)
  "YaTeX add-in function for arguments of \\documentstyle."
  (cond
   ((equal argp 1)
    (setq YaTeX-env-name "document")
    (let ((sname
	   (YaTeX-cplread-with-learning
	    (format "Documentstyle (default %s): "
		    YaTeX-default-document-style)
	    'YaTeX:documentstyles-default
	    'YaTeX:documentstyles-private
	    'YaTeX:documentstyles-local)))
      (if (string= "" sname) (setq sname YaTeX-default-document-style))
      (setq YaTeX-default-document-style sname)))))

(defun YaTeX::include (argp &optional prompt)
  "Read file name setting default directory to that of main file."
  (cond
   ((= argp 1)
    (save-excursion
      (YaTeX-visit-main t)
      (let*((insert-default-directory)
	    (file (read-file-name (or prompt "Input file: ") "")))
	(setq file (substring file 0 (string-match "\\.tex$" file))))))))

(fset 'YaTeX::input 'YaTeX::include)


;;; -------------------- LaTeX2e stuff --------------------
(defvar YaTeX:documentclass-options-default
  '(("a4paper") ("a5paper") ("b4paper") ("b5paper") ("10pt") ("11pt") ("12pt")
    ("latterpaper") ("legalpaper") ("executivepaper") ("landscape")
    ("oneside") ("twoside") ("draft") ("final") ("leqno") ("fleqn") ("openbib")
    ("tombow") ("titlepage") ("notitlepage") ("dvips")
    ("clock")				;for slides class only
    )
    "Default options list for documentclass")
(defvar YaTeX:documentclass-options-private nil
  "*User defined options list for documentclass")
(defvar YaTeX:documentclass-options-local nil
  "*User defined options list for local documentclass")

(defun YaTeX:documentclass ()
  (let*((delim ",")
	(dt (append YaTeX:documentclass-options-local
		    YaTeX:documentclass-options-private
		    YaTeX:documentclass-options-default))
	(minibuffer-completion-table dt)
	(opt (read-from-minibuffer
	      "Documentclass options ([opt1,opt2,...]): "
	      nil YaTeX-minibuffer-completion-map nil))
	(substr opt) o)
    (if (string< "" opt)
	(progn
	  (while substr

	    (setq o (substring substr 0 (string-match delim substr)))
	    (or (assoc o dt)
		(YaTeX-update-table
		 (list o)
		 'YaTeX:documentclass-options-default
		 'YaTeX:documentclass-options-private
		 'YaTeX:documentclass-options-local))
	    (setq substr
		  (if (string-match delim substr)
		      (substring substr (1+ (string-match delim substr))))))
	  (concat "[" opt "]"))
      "")))

(defvar YaTeX:documentclasses-default
  '(("article") ("jarticle") ("report") ("jreport") ("book") ("jbook")
    ("j-article") ("j-report") ("j-book")
    ("letter") ("slides") ("ltxdoc") ("ltxguide") ("ltnews") ("proc"))
  "Default documentclass alist")
(defvar YaTeX:documentclasses-private nil
  "*User defined documentclass alist")
(defvar YaTeX:documentclasses-local nil
  "*User defined local documentclass alist")
(defvar YaTeX-default-documentclass (if YaTeX-japan "jarticle" "article")
  "*Default documentclass")

(defun YaTeX::documentclass (&optional argp)
  (cond
   ((equal argp 1)
    (setq YaTeX-env-name "document")
    (let ((sname
	   (YaTeX-cplread-with-learning
	    (format "Documentclass (default %s): " YaTeX-default-documentclass)
	    'YaTeX:documentclasses-default
	    'YaTeX:documentclasses-private
	    'YaTeX:documentclasses-local)))
      (if (string= "" sname) (setq sname YaTeX-default-documentclass))
      (setq YaTeX-default-documentclass sname)))))

(defvar YaTeX:latex2e-named-color-alist
  '(("GreenYellow") ("Yellow") ("Goldenrod") ("Dandelion") ("Apricot")
    ("Peach") ("Melon") ("YellowOrange") ("Orange") ("BurntOrange")
    ("Bittersweet") ("RedOrange") ("Mahogany") ("Maroon") ("BrickRed")
    ("Red") ("OrangeRed") ("RubineRed") ("WildStrawberry") ("Salmon")
    ("CarnationPink") ("Magenta") ("VioletRed") ("Rhodamine") ("Mulberry")
    ("RedViolet") ("Fuchsia") ("Lavender") ("Thistle") ("Orchid")("DarkOrchid")
    ("Purple") ("Plum") ("Violet") ("RoyalPurple") ("BlueViolet")
    ("Periwinkle") ("CadetBlue") ("CornflowerBlue") ("MidnightBlue")
    ("NavyBlue") ("RoyalBlue") ("Blue") ("Cerulean") ("Cyan") ("ProcessBlue")
    ("SkyBlue") ("Turquoise") ("TealBlue") ("Aquamarine") ("BlueGreen")
    ("Emerald") ("JungleGreen") ("SeaGreen") ("Green") ("ForestGreen")
    ("PineGreen") ("LimeGreen") ("YellowGreen") ("SpringGreen") ("OliveGreen")
    ("RawSienna") ("Sepia") ("Brown") ("Tan") ("Gray") ("Black") ("White"))
  "Colors defined in $TEXMF/tex/plain/colordvi.tex")

(defvar YaTeX:latex2e-basic-color-alist
  '(("black") ("white") ("red") ("blue") ("yellow") ("green") ("cyan")
    ("magenta"))
  "Basic colors")

(defun YaTeX:textcolor ()
  "Add-in for \\color's option"
  (if (y-or-n-p "Use `named' color? ")
      "[named]"))

(defun YaTeX::color-completing-read (prompt)
  (let ((completion-ignore-case t)
	(namedp (save-excursion
		  (skip-chars-backward "^\n\\[\\\\")
		  (looking-at "named"))))
    (completing-read
     prompt
     (if namedp
	 YaTeX:latex2e-named-color-alist
       YaTeX:latex2e-basic-color-alist)
     nil t)))

(defun YaTeX::textcolor (argp)
  "Add-in for \\color's argument"
  (cond
   ((= argp 1) (YaTeX::color-completing-read "Color: "))
   ((= argp 2) (read-string "Colored string: "))))

(fset 'YaTeX:color 'YaTeX:textcolor)
(fset 'YaTeX::color 'YaTeX::textcolor)
(fset 'YaTeX:colorbox 'YaTeX:textcolor)
(fset 'YaTeX::colorbox 'YaTeX::textcolor)
(fset 'YaTeX:fcolorbox 'YaTeX:textcolor)
(fset 'YaTeX:pagecolor 'YaTeX:textcolor)
(fset 'YaTeX::pagecolor 'YaTeX::textcolor)

(defun YaTeX::fcolorbox (argp)
  (cond
   ((= argp 1) (YaTeX::color-completing-read "Frame color: "))
   ((= argp 2) (YaTeX::color-completing-read "Inner color: "))
   ((= argp 3) (read-string "Colored string: "))))

(defun YaTeX:scalebox ()
  "Add-in for \\scalebox"
  (let ((vmag (read-string
	       (if YaTeX-japan "倍率(負で反転): "
		 "Magnification(Negative for flipped): ")))
	(hmag (read-string (if YaTeX-japan "縦倍率(省略可): "
			     "Vertical magnification(Optional): "))))
    (if (and hmag (string< "" hmag))
	(format "{%s}[%s]" vmag hmag)
      (format "{%s}" vmag))))

(defun YaTeX:rotatebox ()
  "Optional argument add-in for \\rotatebox"
  (message "Rotate origin? (N)one (O)rigin (X)-Y: ")
  (let ((c (read-char)) r (defx "x=mm") x (defy "y=mm") y something)
    (cond
     ((memq c '(?O ?o))
      (if (string< "" (setq r (YaTeX:read-oneof "htbpB")))
	  (concat "[origin=" r "]")))
     ((memq c '(?X ?x ?Y ?y))
      (setq r (read-string "" (if YaTeX-emacs-19 (cons defx 3) defx))
	    x (if (string< "x=" r) r)
	    r (read-string "" (if YaTeX-emacs-19 (cons defy 3) defy))
	    y (if (string< "y=" r) r)
	    something (or x y))
      (format "%s%s%s%s%s"
	      (if something "[" "")
	      (if x x "")
	      (if (and x y) "," "")
	      (if y y "")
	      (if something "]" ""))))))

(defun YaTeX::rotatebox (argp)
  "Argument add-in for \\rotatebox"
  (cond
   ((= argp 1)
    (read-string (if YaTeX-japan "回転角(度; 左回り): "
		   "Angle in degree(unclockwise): ")))
   ((= argp 2)
	(read-string (if YaTeX-japan "テキスト: " "Text: ")))))

(defun YaTeX:includegraphics ()
  "Add-in for \\includegraphics's option"
  (let (width height (scale "") angle str)
    (setq width (read-string "Width: ")
	  height (read-string "Height: "))
    (or (string< width "") (string< "" height)
	(setq scale (read-string "Scale: ")))
    (setq angle (read-string "Angle(0-359): "))
    (setq str
	  (mapconcat
	   'concat
	   (delq nil
		 (mapcar '(lambda (s)
			    (and (stringp (symbol-value s))
				 (string< "" (symbol-value s))
				 (format "%s=%s" s (symbol-value s))))
			 '(width height scale angle)))
	   ","))
    (if (string= "" str) ""
      (concat "[" str "]"))))

(defun YaTeX::includegraphics (argp)
  "Add-in for \\includegraphics"
  (YaTeX::include argp "Image File: "))
 
(defun YaTeX::verbfile (argp)
  "Add-in for \\verbfile"
  (YaTeX::include argp "Virbatim File: "))
 
(defun YaTeX:caption ()
  (setq YaTeX-section-name "label")
  nil)


(defvar YaTeX::usepackage-alist-default
  '(("version") ("plext") ("url") ("fancybox") ("pifont") ("longtable")
    ("ascmac") ("bm") ("graphics") ("graphicx") ("alltt") ("misc") ("eclbkbox")
    ("amsmath") ("amssymb") ("xymtex") ("chemist")
    ("a4j") ("array") ("epsf") ("color") ("epsfig") ("floatfig")
    ("landscape") ("path") ("supertabular") ("twocolumn")
    ("latexsym") ("times") ("makeidx"))
  "Default completion table for arguments of \\usepackage")

(defvar YaTeX::usepackage-alist-private nil
  "*Private completion list of the argument for usepackage")

(defvar YaTeX::usepackage-alist-local nil
  "Directory local  completion list of the argument for usepackage")

(defun YaTeX::usepackage (&optional argp)
  (cond
   ((equal argp 1)
    (setq YaTeX-env-name "document")
    (let ((minibuffer-local-completion-map YaTeX-minibuffer-completion-map)
	  (delim ","))
      (YaTeX-cplread-with-learning
       (if YaTeX-japan "Use package(カンマで区切ってOK): "
	 "Use package(delimitable by comma): ")
       'YaTeX::usepackage-alist-default
       'YaTeX::usepackage-alist-private
       'YaTeX::usepackage-alist-local)))))

(defun YaTeX::mask (argp)
  (cond
   ((equal argp 1)
    (read-string "String: "))
   ((equal argp 2)
    (let (c)
      (while (not (memq c '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K)))
	(message "Mask type(A..K): ")
	(setq c (upcase (read-char))))
      (format "%c" c)))))

(defun YaTeX::maskbox (argp)
  (cond
   ((equal argp 1)
    (read-string "Width: "))
   ((equal argp 2)
    (read-string "Height: "))
   ((equal argp 3)
    (let (c)
      (while (not (memq c '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K)))
	(message "Mask type(A..K): ")
	(setq c (upcase (read-char))))
      (format "%c" c)))
   ((equal argp 4)
    (YaTeX:read-oneof "lcr" 'quick))
   ((equal argp 5)
    (read-string "String: "))))

(defun YaTeX::textcircled (argp)
  (cond
   ((equal argp 1)
    (let ((char (read-string "Circled char: "))
	  (left "") (right "") c)
      (setq c (read-char
	       "Enclose also with (s)mall (t)iny s(C)riptsize (N)one:"))
      (cond
       ((memq c '(?s ?S)) (setq left "{\\small " right "}"))
       ((memq c '(?t ?T)) (setq left "{\\tiny " right "}"))
       ((memq c '(?c ?C)) (setq left "{\\scriptsize " right "}")))
      (format "%s%s%s" left char right)))))

;;; -------------------- math-mode stuff --------------------
(defun YaTeX::tilde (&optional pos)
  "For accent macros in mathmode"
  (cond
   ((equal pos 1)
    (message "Put accent on variable: ")
    (let ((v (char-to-string (read-char))) (case-fold-search nil))
      (message "")
      (cond
       ((string-match "i\\|j" v)
	(concat "\\" v "math"))
       ((string-match "[\r\n\t ]" v)
	"")
       (t v))))
   (nil "")))

(fset 'YaTeX::hat	'YaTeX::tilde)
(fset 'YaTeX::check	'YaTeX::tilde)
(fset 'YaTeX::bar	'YaTeX::tilde)
(fset 'YaTeX::dot	'YaTeX::tilde)
(fset 'YaTeX::ddot	'YaTeX::tilde)
(fset 'YaTeX::vec	'YaTeX::tilde)

(defun YaTeX::widetilde (&optional pos)
  "For multichar accent macros in mathmode"
  (cond
   ((equal pos 1)
    (let ((m "Put over chars[%s ]: ") v v2)
      (message m " ")
      (setq v (char-to-string (read-char)))
      (message "")
      (if (string-match "[\r\n\t ]" v)
	  ""
	(message m v)
	(setq v2 (char-to-string (read-char)))
	(message "")
	(if (string-match "[\r\n\t ]" v2)
	    v
	  (concat v v2)))))
   (nil "")))

(fset 'YaTeX::widehat		'YaTeX::widetilde)
(fset 'YaTeX::overline		'YaTeX::widetilde)
(fset 'YaTeX::overrightarrow	'YaTeX::widetilde)
	
;
; for \frac{}{} region
(defun YaTeX::frac-region (beg end)
  (if (catch 'done
	(while (re-search-forward "\\s *\\(\\\\over\\|/\\)\\s *" end t)
	  (goto-char (match-beginning 0))
	  (if (y-or-n-p
	       (format "Replace this `%s' with `}{'" (YaTeX-match-string 0)))
	      (throw 'done t))
	  (goto-char (match-end 0))))
      (let (p (b0 (match-beginning 0)) e0)
	(replace-match "}{")
	(setq e0 (point))
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char e0)
	  (skip-chars-forward " \t")
	  (setq p (point))
	  (YaTeX-goto-corresponding-paren)
	  (forward-char 1)
	  (skip-chars-forward " \t\r\n")
	  (if (= end (1+ (point)))
	      (progn
		(goto-char p)
		(if (looking-at "\\\\") (forward-char 1))
		(YaTeX-kill-paren nil)))
	  (goto-char beg)
	  (skip-chars-forward " \t")
	  (setq p (point))
	  (YaTeX-goto-corresponding-paren)
	  (forward-char 1)
	  (skip-chars-forward " \t\r\n")
	  (if (>= (point) b0)
	      (progn
		(goto-char p)
		(if (looking-at "\\\\") (forward-char 1))
		(YaTeX-kill-paren nil))))))
  (message ""))

(defun YaTeX::DeclareMathOperator (argp)
  (cond
   ((equal argp 1)
    (read-string "Operator: " "\\"))))

;;;
;; Add-in functions for large-type command.
;;;
(defun YaTeX:em ()
  (cond
   ((eq YaTeX-current-completion-type 'large) "\\/")
   (t nil)))
(fset 'YaTeX:it 'YaTeX:em)

;;; -------------------- End of yatexadd --------------------
(provide 'yatexadd)
; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; coding: sjis
; End:
