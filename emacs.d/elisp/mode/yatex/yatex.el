;;; -*- Emacs-Lisp -*-
;;; Yet Another tex-mode for emacs - //野鳥//
;;; yatex.el rev. 1.74
;;; (c)1991-2009 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Mon Sep 28 10:44:35 2009 on firestorm
;;; $Id: yatex.el,v 1.74 2009/09/28 01:54:43 yuuji Rel $
;;; The latest version of this software is always available at;
;;; http://www.yatex.org/

(require 'comment)
(require 'yatexlib)
(defconst YaTeX-revision-number "1.74"
  "Revision number of running yatex.el")

;---------- Local variables ----------
(defvar YaTeX-prefix "\C-c"
  "*Prefix key to call YaTeX functions.
You can select favorite prefix key by setq in your ~/.emacs.")

(defvar YaTeX-environment-indent 1
  "*Indentation depth at column width in LaTeX environments.")

(defvar YaTeX-fill-prefix nil
  "*fill-prefix used for auto-fill-mode.
The default value is nil.")

(defvar YaTeX-fill-column 72
  "*fill-column used for auto-fill-mode.")

(defvar YaTeX-comment-prefix "%"
  "TeX comment prefix.")

(defvar YaTeX-current-position-register ?3
  "*Position register to keep where the last completion was done.
All of YaTeX completing input store the current position into
the register YaTeX-current-position-register.  So every time you
make a trip to any other part of text other than you are writing, you can
return to the editing paragraph by calling register-to-point with argument
YaTeX-current-position-register.")

;;(defvar YaTeX-tmp-dic-unit 'main-file
;;  "*Default switching unit of temporary dictionary.
;;There are two switching unit:
;;'main-file	: switch tmp-dic according to main-file directory.
;;'directory	: switch tmp-dic dir by dir."
;;)
(defvar YaTeX-use-LaTeX2e t "*Use LaTeX2e or not.  Nil meas latex 2.09")

(defvar tex-command
  (cond
   (YaTeX-use-LaTeX2e "platex")
   (YaTeX-japan "jlatex")
   (t "latex"))
  "*Default command for typesetting LaTeX text.")

(defvar bibtex-command (if YaTeX-japan "jbibtex" "bibtex")
  "*Default command of BibTeX.")

(defvar dvi2-command		;previewer command for your site
  (if YaTeX-dos "dviout -wait=0"
    "xdvi -geo +0+0 -s 4")
  "*Default previewer command including its option.
This default value is for X window system.")

(defvar makeindex-command (if YaTeX-dos "makeind" "makeindex")
  "*Default makeindex command.")

(defvar dviprint-command-format
  (if YaTeX-dos "dviprt %s %f%t"
      "dvi2ps %f %t %s | lpr")
  "*Command line string to print out current file.
Format string %s will be replaced by the filename.  Do not forget to
specify the `from usage' and `to usage' with their option by format string
%f and %t.
  See also documentation of dviprint-from-format and dviprint-to-format.")

(defvar dviprint-from-format
  (if YaTeX-dos "%b-" "-f %b")
  "*`From' page format of dvi filter.  %b will turn to beginning page number.")

(defvar dviprint-to-format
  (if YaTeX-dos "%e" "-t %e")
  "*`To' page format of dvi filter.  %e will turn to end page number.")

(defvar YaTeX-default-document-style
  (concat (if YaTeX-japan "j") "article")
  "*Default LaTeX Documentstyle for YaTeX-typeset-region.")

(defvar YaTeX-need-nonstop nil
  "*T for adding `\\nonstopmode{}' to text before invoking latex command.")

(defvar latex-warning-regexp "line.* [0-9]*"
  "*Regular expression of line number of warning message by latex command.")

(defvar latex-error-regexp "l\\.[1-9][0-9]*"
  "*Regular expression of line number of latex error.
Perhaps your latex command stops at this error message with line number of
LaTeX source text.")

(defvar latex-dos-emergency-message
  "Emergency stop"      ;<- for Micro tex, ASCII-pTeX 1.6
  "Message pattern of emergency stop of typesetting.
Because Demacs (GNU Emacs on DOS) cannot have concurrent process, the
latex command which is stopping on a LaTeX error, is terminated by Demacs.
Many latex command on DOS display some messages when it is terminated by
other process, user or OS.  Define to this variable a message string of your
latex command on DOS shown at abnormal termination.
  Remember Demacs's call-process function is not oriented for interactive
process.")

(defvar NTT-jTeX nil
  "*T for using NTT-jTeX for latex command.
More precisely, setting t to this variables inhibits inter-word break on
typeset document by line-break of source text.  That is, YaTeX automatically
put % after each line at filling.
改行+インデントによって、タイプセット後の字間が空いてしまうのを抑制する場合に
tにする(古いNTT-jTeXで顕著に現れる)。具体的には、fillするときに各行の終わりに
%を付加する。")


(defvar YaTeX-item-regexp
  (concat (regexp-quote "\\") "\\(sub\\|bib\\)*item")
  "*Regular expression of item command.")

(defvar YaTeX-sectioning-regexp
  "\\(part\\|chapter\\*?\\|\\(sub\\)*\\(section\\|paragraph\\)\\)\\(\\*\\|\\b\\)"
  "*LaTeX sectioning commands regexp.")

(defvar YaTeX-paragraph-start
  (concat "^[ \t]*%\\|^[ \t]*$\\|\\'\\|^\C-l\\|\\\\\\\\$\\|^[ \t]*\\\\\\("
	  YaTeX-sectioning-regexp		;sectioning commands
	  "\\|[A-z]*item\\|begin{\\|end{"	;special declaration
	  "\\|\\[\\|\\]"
	  "\\|newpage\\b\\|vspace\\b"
	  "\\)")
  "*Paragraph starting regexp of common LaTeX source.  Use this value
for YaTeX-uncomment-paragraph.")

(defvar YaTeX-paragraph-separate
  (concat "^[ \t]*%\\|^[ \t]*$\\|^\C-l\\|\\\\\\\\$\\|^[ \t]*\\\\\\("
	  YaTeX-sectioning-regexp		;sectioning commands
	  "\\|begin{\\|end{"			;special declaration
	  "\\|\\[\\|\\]"
	  "\\|newpage\\b\\|vspace\\b"
	  "\\)")
  "*Paragraph delimiter regexp of common LaTeX source.  Use this value
for YaTeX-uncomment-paragraph.")

(defvar YaTeX-verbatim-environments 
  '("verbatim" "verbatim*")
  "*Assume these environments of this variable disable LaTeX commands.")
(defvar YaTeX-verb-regexp "verb\\*?\\|path"
  "*Regexp of verb family.  Do not contain preceding \\\\ nor \\(\\).")
(defvar YaTeX-fill-inhibit-environments
  (append '("tabular" "tabular*" "array" "picture" "eqnarray" "eqnarray*"
	    "equation" "equation*" "math" "displaymath")
	  YaTeX-verbatim-environments)
  "*In these environments, YaTeX inhibits fill-paragraph from formatting.
Define those environments as a form of list.")

(defvar YaTeX-itemizing-env-regexp
  "itemize\\|enumerate\\|description\\|list\\|thebibliography"
  "*Regexp of itemizing environments")
(defvar YaTeX-equation-env-regexp
  "array\\*?\\|equation\\*?"
  "*Regexp of environments for equations")
(defvar YaTeX-array-env-regexp
  (concat
   "array\\*?\\|eqnarray\\*?\\|tabbing\\|tabular\\*?\\|"	;LaTeX
   "matrix\\|pmatrix\\|bmatrix\\|vmatrix\\|Vmatrix\\|"		;AMS-LaTeX
   "align\\*?\\|split\\*?\\|aligned\\*?\\|alignat\\*?\\|"	;AMS-LaTeX
   "[bpvV]?matrix\\|smallmatrix\\|cases\\|"			;AMS-LaTeX
   "xalignat\\*?\\|xxalignat\\*?")				;AMS-LaTeX
  "*Regexp of environments where `&' becomes field delimiter.")
(defvar YaTeX-uncomment-once t
  "*T for removing all continuous commenting character(%).
Nil for removing only one commenting character at the beginning-of-line.")

(defvar YaTeX-close-paren-always t
  "*Close parenthesis always when YaTeX-modify-mode is nil.")

(defvar YaTeX-greek-by-maketitle-completion nil
  "*T for greek letters completion by maketitle-type completion.")

(defvar YaTeX-auto-math-mode t
  "*T for changing YaTeX-math mode automatically.")
(defvar YaTeX-use-AMS-LaTeX nil
  "*T for using AMS-LaTeX")

(defvar yatex-mode-hook nil
  "*List of functions to be called at the end of yatex-mode initializations.")

(defvar YaTeX-search-file-from-top-directory t
  "*Non-nil means to search input-files from the directory where main file exists.")

(defvar YaTeX-use-font-lock (and (featurep 'font-lock)
				 (fboundp 'x-color-values)
				 (fboundp 'font-lock-fontify-region))
  "*Use font-lock to fontify buffer or not.")

(defvar YaTeX-use-hilit19 (and (featurep 'hilit19) (fboundp 'x-color-values)
			       (fboundp 'hilit-translate)
			       (not YaTeX-use-font-lock))
  "*Use hilit19 to highlight buffer or not.")

(defvar YaTeX-tabular-indentation 4
  "*Indentation column-depth of continueing line in tabular environment.")

;;-- Math mode values --

(defvar YaTeX-math-key-list-default
  '((";" . YaTeX-math-sign-alist)
    (":" . YaTeX-greek-key-alist))
  "Default key sequence to invoke math-mode's image completion.")

(defvar YaTeX-math-key-list-private nil
  "*User defined alist, math-mode-prefix vs completion alist.")

(defvar YaTeX-math-key-list
  (append YaTeX-math-key-list-private YaTeX-math-key-list-default)
  "Key sequence to invoke math-mode's image completion.")

(defvar YaTeX-skip-default-reader nil
  "Non-nil skips default argument reader of section-type completion.")

(defvar YaTeX-simple-messages nil
  "Non-nil makes minibuffer messages simpler.")

(defvar YaTeX-template-file "~/work/template.tex"
  "*Template TeX source file.  This will be inserted to empty file.")

(defvar YaTeX-addin-prefix "YaTeX:")

(defvar yatex-mode-abbrev-table nil
  "*Abbrev table in use in yatex-mode buffers.")
(define-abbrev-table 'yatex-mode-abbrev-table ())


;------------ Completion table ------------
; Set tex-section-like command possible completion
(defvar section-table
  (append
   '(("part") ("chapter") ("chapter*") ("section") ("section*")
     ("subsection") ("subsection*")
     ("subsubsection") ("paragraph") ("subparagraph")
     ("author") ("thanks") ("documentstyle") ("pagestyle") ("thispagestyle")
     ("title") ("underline") ("label") ("makebox")
     ("footnote") ("footnotetext") ("index")
     ("hspace*") ("vspace*") ("bibliography") ("bibitem") ("cite")
     ("input") ("include") ("includeonly") ("mbox") ("hbox") ("caption")
     ("arabic")
     ("newcounter")
     ("newlength") ("setlength" 2) ("addtolength" 2) ("settowidth" 2)
     ("setcounter" 2) ("addtocounter" 2) ("stepcounter" 2)
     ("newcommand" 2) ("renewcommand" 2)
     ("newenvironment" 3) ("newtheorem" 2)
     ("cline") ("framebox") ("savebox" 2) ("sbox" 2) ("newsavebox") ("usebox")
     ("date") ("put") ("ref") ("pageref") ("tabref") ("figref") ("raisebox" 2)
     ("multicolumn" 3) ("shortstack") ("parbox" 2)
     ;; for mathmode accent
     ("tilde") ("hat") ("check") ("bar") ("dot") ("ddot") ("vec")
     ("widetilde") ("widehat") ("overline") ("overrightarrow")
     ;; section types in mathmode
     ("frac" 2) ("sqrt") ("mathrm") ("mathbf") ("mathit")

     )
   (if YaTeX-use-LaTeX2e
       '(("documentclass") ("usepackage")
	 ("textbf") ("textgt") ("textit") ("textmc") ("textmd") ("textnormal")
	 ("textrm") ("textsc") ("textsf") ("textsl") ("texttt") ("textup")
	 ("mathbf") ("mathcal") ("mathit") ("mathnormal") ("mathrm")
	 ("mathsf") ("mathtt")
	 ("textcircled")
	 ("scalebox" 1) ;is faking of argument position
	 ("rotatebox" 2) ("resizebox" 3) ("reflectbox")
	 ("colorbox" 2) ("fcolorbox" 3) ("textcolor" 2) ("color") ("pagecolor")
	 ("includegraphics") ("includegraphics*")
	 ("bou")			;defined in plext
	 ("url")			;defined in url
	 ("shadowbox") ("doublebox") ("ovalbox") ("Ovalbox")
	 ("fancyoval")			;defined in fancybox
	 ("keytop") ("mask" 2) ("maskbox" 5) ;defined in ascmac
	 ("bm")				;deined in bm
	 ("verbfile") ("listing")	;defined in misc
	 ("slashbox" 2) ("backslashbox" 2) ;defined in slashbox
	 ))
   (if YaTeX-use-AMS-LaTeX
       '(("DeclareMathOperator" 2) ("boldsymbol") ("pmb") ("eqref")
	 ("tag") ("tag*"))))
  "Default completion table for section-type completion.")

(defvar user-section-table nil)
(defvar tmp-section-table nil)
(defvar YaTeX-ams-math-begin-alist
  '(("align") ("align*") ("multline") ("multline*") ("gather") ("gather*")
    ("alignat") ("alignat*") ("xalignat") ("xalignat*")
    ("xxalignat") ("xxalignat*") ("flalign") ("flalign*") ("equation*")))
(defvar YaTeX-ams-math-gathering-alist
  '(("matrix") ("pmatrix") ("bmatrix") ("Bmatrix") ("vmatrix") ("Vmatrix")
    ("split") ("split*") ("aligned") ("aligned*") ("alignedat") ("gathered")
    ("smallmatrix") ("cases") ("subequations")))
;; Prepare list(not alist) for YaTeX::ref in yatexadd.el
(defvar YaTeX-math-begin-list
  (mapcar 'car YaTeX-ams-math-begin-alist))
(defvar YaTeX-math-gathering-list	;used in yatexadd.el#yatex::ref
  (mapcar 'car YaTeX-ams-math-gathering-alist))


(defvar YaTeX-ams-env-table
  (append YaTeX-ams-math-begin-alist YaTeX-ams-math-gathering-alist)
  "*Standard AMS-LaTeX(2e) environment completion table.")

; Set tex-environment possible completion
(defvar env-table
  (append
   '(("quote") ("quotation") ("center") ("verse") ("document")
     ("verbatim") ("itemize") ("enumerate") ("description")
     ("list") ("tabular") ("tabular*") ("table") ("tabbing") ("titlepage")
     ("sloppypar") ("picture") ("displaymath")
     ("eqnarray") ("figure") ("equation") ("abstract") ("array")
     ("thebibliography") ("theindex") ("flushleft") ("flushright")
     ("minipage")
     ("supertabular")
     )
   (if YaTeX-use-LaTeX2e
       '(("comment")			;defined in version
	 ("longtable")			;defined in longtable
	 ("screen") ("boxnote") ("shadebox") ;; ("itembox") ;in ascmac
	 ("alltt")			;defined in alltt
	 ("multicols")			;defined in multicol
	 ("breakbox")))			;defined in eclbkbox
   (if YaTeX-use-AMS-LaTeX YaTeX-ams-env-table))
  "Default completion table for begin-type completion.")

(defvar user-env-table nil)
(defvar tmp-env-table nil)

; Set {\Large }-like completion
(defvar fontsize-table
  '(("rm") ("em") ("bf") ("boldmath") ("it") ("sl") ("sf") ("sc") ("tt")
    ("dg") ("dm")
    ("tiny") ("scriptsize") ("footnotesize") ("small")("normalsize")
    ("large") ("Large") ("LARGE") ("huge") ("Huge")
    ("rmfamily") ("sffamily") ("ttfamily")
    ("mdseries") ("bfseries") ("upshape")
    ("itshape") ("slshape") ("scshape")
    )
  "Default completion table for large-type completion.")

(defvar LaTeX2e-fontstyle-alist
  '(("rm" . "rmfamily")
    ("sf" . "sffamily")
    ("tt" . "ttfamily")
    ("md" . "mdseries")
    ("bf" . "bfseries")
    ("up" . "upshape")
    ("it" . "itshape")
    ("sl" . "slshape")
    ("sc" . "scshape")))

(defvar user-fontsize-table nil)
(defvar tmp-fontsize-table nil)

(defvar singlecmd-table
  (append
   '(("maketitle") ("makeindex") ("sloppy") ("protect")
     ("LaTeX") ("TeX") ("item") ("item[]") ("appendix") ("hline") ("kill")
     ;;("rightarrow") ("Rightarrow") ("leftarrow") ("Leftarrow")
     ("pagebreak") ("nopagebreak") ("tableofcontents")
     ("newpage") ("clearpage") ("cleardoublepage")
     ("footnotemark") ("verb") ("verb*")
     ("linebreak") ("pagebreak") ("noindent") ("indent")
     ("left") ("right") ("dots") ("smallskip") ("medskip") ("bigskip")
     ("displaystyle")
     )
   (if YaTeX-greek-by-maketitle-completion
       '(("alpha") ("beta") ("gamma") ("delta") ("epsilon")
	 ("varepsilon") ("zeta") ("eta") ("theta")("vartheta")
	 ("iota") ("kappa") ("lambda") ("mu") ("nu") ("xi") ("pi")
	 ("varpi") ("rho") ("varrho") ("sigma") ("varsigma") ("tau")
	 ("upsilon") ("phi") ("varphi") ("chi") ("psi") ("omega")
	 ("Gamma") ("Delta") ("Theta") ("Lambda")("Xi") ("Pi")
	 ("Sigma") ("Upsilon") ("Phi") ("Psi") ("Omega")))
   (if YaTeX-use-LaTeX2e
       '(("return") ("Return") ("yen")))	;defined in ascmac
   (if YaTeX-use-AMS-LaTeX
       '(("nonumber")))
   )
  "Default completion table for maketitle-type completion.")

(defvar user-singlecmd-table nil)
(defvar tmp-singlecmd-table nil)

;---------- Key mode map ----------
;;;
;; Create new key map: YaTeX-mode-map
;; Do not change this section.
;;;
(defvar YaTeX-mode-map nil
  "Keymap used in YaTeX mode")

(defvar YaTeX-prefix-map nil
  "Keymap used when YaTeX-prefix key pushed")

(defvar YaTeX-user-extensional-map (make-sparse-keymap)
  "*Keymap used for the user's customization")
(defvar YaTeX-current-completion-type nil
  "Has current completion type.  This may be used in YaTeX addin functions.")

(defvar YaTeX-modify-mode nil
  "*Current editing mode.
When non-nil, each opening parentheses only opens,
nil enters both open/close parentheses when opening parentheses key pressed.")

(defvar YaTeX-math-mode nil
  "Holds whether current mode is math-mode.")
;;;
;; Define key table
;;;
(if YaTeX-mode-map 
    nil
  (setq YaTeX-mode-map (make-sparse-keymap))
  (setq YaTeX-prefix-map (make-sparse-keymap))
  (define-key YaTeX-mode-map "\"" 'YaTeX-insert-quote)
  (define-key YaTeX-mode-map "{" 'YaTeX-insert-braces)
  (define-key YaTeX-mode-map "(" 'YaTeX-insert-parens)
  (define-key YaTeX-mode-map "$" 'YaTeX-insert-dollar)
  (define-key YaTeX-mode-map "|" 'YaTeX-insert-bar)
  (define-key YaTeX-mode-map "&" 'YaTeX-insert-amper)
  (define-key YaTeX-mode-map "[" 'YaTeX-insert-brackets)
  (define-key YaTeX-mode-map YaTeX-prefix YaTeX-prefix-map)
  (define-key YaTeX-mode-map "\M-\C-@" 'YaTeX-mark-environment)
  (define-key YaTeX-mode-map "\M-\C-a" 'YaTeX-beginning-of-environment)
  (define-key YaTeX-mode-map "\M-\C-e" 'YaTeX-end-of-environment)
  (define-key YaTeX-mode-map "\M-\C-m" 'YaTeX-intelligent-newline)
  (define-key YaTeX-mode-map "\C-i" 'YaTeX-indent-line)
  (YaTeX-define-key "%" 'YaTeX-%-menu)
  (YaTeX-define-key "t" 'YaTeX-typeset-menu)
  (YaTeX-define-key "w" 'YaTeX-switch-mode-menu)
  (YaTeX-define-key "'" 'YaTeX-prev-error)
  (YaTeX-define-key "^" 'YaTeX-visit-main)
  (YaTeX-define-key "4^" 'YaTeX-visit-main-other-window)
  (YaTeX-define-key "4g" 'YaTeX-goto-corresponding-*-other-window)
  (YaTeX-define-key "44" 'YaTeX-switch-to-window)
  (and YaTeX-emacs-19 window-system
       (progn
	 (YaTeX-define-key "5^" 'YaTeX-visit-main-other-frame)
	 (YaTeX-define-key "5g" 'YaTeX-goto-corresponding-*-other-frame)
	 (YaTeX-define-key "55" 'YaTeX-switch-to-window)))
  (YaTeX-define-key " " 'YaTeX-do-completion)
  (YaTeX-define-key "v" 'YaTeX-version)

  (YaTeX-define-key "}" 'YaTeX-insert-braces-region)
  (YaTeX-define-key "]" 'YaTeX-insert-brackets-region)
  (YaTeX-define-key ")" 'YaTeX-insert-parens-region)
  (YaTeX-define-key "$" 'YaTeX-insert-dollars-region)
  (YaTeX-define-key "i" 'YaTeX-fill-item)
  (YaTeX-define-key
   "\\" '(lambda () (interactive) (insert "$\\backslash$")))
  (if YaTeX-no-begend-shortcut
      (progn
	(YaTeX-define-key "B" 'YaTeX-make-begin-end-region)
	(YaTeX-define-key "b" 'YaTeX-make-begin-end))
    (YaTeX-define-begend-key "bc" "center")
    (YaTeX-define-begend-key "bd" "document")
    (YaTeX-define-begend-key "bD" "description")
    (YaTeX-define-begend-key "be" "enumerate")
    (YaTeX-define-begend-key "bE" "equation")
    (YaTeX-define-begend-key "bi" "itemize")
    (YaTeX-define-begend-key "bl" "flushleft")
    (YaTeX-define-begend-key "bm" "minipage")
    (YaTeX-define-begend-key "bt" "tabbing")
    (YaTeX-define-begend-key "bT" "tabular")
    (YaTeX-define-begend-key "b\^t" "table")
    (YaTeX-define-begend-key "bp" "picture")
    (YaTeX-define-begend-key "bq" "quote")
    (YaTeX-define-begend-key "bQ" "quotation")
    (YaTeX-define-begend-key "br" "flushright")
    (YaTeX-define-begend-key "bv" "verbatim")
    (YaTeX-define-begend-key "bV" "verse")
    (YaTeX-define-key "B " 'YaTeX-make-begin-end-region)
    (YaTeX-define-key "b " 'YaTeX-make-begin-end))
  (YaTeX-define-key "e" 'YaTeX-end-environment)
  (YaTeX-define-key "S" 'YaTeX-make-section-region)
  (YaTeX-define-key "s" 'YaTeX-make-section)
  (YaTeX-define-key "L" 'YaTeX-make-fontsize-region)
  (YaTeX-define-key "l" 'YaTeX-make-fontsize)
  (YaTeX-define-key "m" 'YaTeX-make-singlecmd)
  (YaTeX-define-key "." 'YaTeX-comment-paragraph)
  (YaTeX-define-key "," 'YaTeX-uncomment-paragraph)
  (YaTeX-define-key ">" 'YaTeX-comment-region)
  (YaTeX-define-key "<" 'YaTeX-uncomment-region)
  (YaTeX-define-key "g" 'YaTeX-goto-corresponding-*)
  (YaTeX-define-key "k" 'YaTeX-kill-*)
  (YaTeX-define-key "c" 'YaTeX-change-*)
  (YaTeX-define-key "a" 'YaTeX-make-accent)
  (YaTeX-define-key "?" 'YaTeX-help)
  (YaTeX-define-key "/" 'YaTeX-apropos)
  (YaTeX-define-key "&" 'YaTeX-what-column)
  (YaTeX-define-key "d" 'YaTeX-display-hierarchy)
  (YaTeX-define-key "x" YaTeX-user-extensional-map)
  (YaTeX-define-key "n"
    '(lambda () (interactive)
       (insert "\\" (if (YaTeX-on-section-command-p "o?oalign") "crcr" "\\"))))
  (if YaTeX-dos
      (define-key YaTeX-prefix-map "\C-r"
	'(lambda () (interactive)
	   (set-screen-height YaTeX-saved-screen-height) (recenter)))))

(defvar YaTeX-section-completion-map nil
  "*Key map used at YaTeX completion in the minibuffer.")
(if YaTeX-section-completion-map nil
  (setq YaTeX-section-completion-map
	(copy-keymap (or (and (boundp 'gmhist-completion-map)
			      gmhist-completion-map)
			 minibuffer-local-completion-map)))
  (define-key YaTeX-section-completion-map
    " " 'YaTeX-minibuffer-complete)
  (define-key YaTeX-section-completion-map
    "\C-i" 'YaTeX-minibuffer-complete)
  (define-key YaTeX-section-completion-map
    "\C-v" 'YaTeX-read-section-with-overview))

(defvar YaTeX-recursive-map nil
  "*Key map used at YaTeX reading arguments in the minibuffer.")
(if YaTeX-recursive-map nil
  (setq YaTeX-recursive-map (copy-keymap global-map))
  (define-key YaTeX-recursive-map YaTeX-prefix YaTeX-prefix-map)
  (mapcar
   (function
    (lambda (key)
      (define-key YaTeX-mode-map (car key) 'YaTeX-math-insert-sequence)
      (define-key YaTeX-recursive-map (car key) 'YaTeX-math-insert-sequence)))
   YaTeX-math-key-list))
;---------- Define other variable ----------
(defvar YaTeX-env-name "document" "*Initial tex-environment completion")
(defvar YaTeX-section-name
  (if YaTeX-use-LaTeX2e "documentclass" "documentstyle")
  "*Initial tex-section completion")
(defvar YaTeX-fontsize-name "large" "*Initial fontsize completion")
(defvar YaTeX-single-command "maketitle" "*Initial LaTeX single command")
(defvar YaTeX-kanji-code (if YaTeX-dos 1 2)
  "*File kanji code used by Japanese TeX.
nil: Do not care (Preserve coding-system)
0: no-converion (mule)
1: Shift JIS
2: JIS
3: EUC
4: UTF-8")

(defvar YaTeX-coding-system nil "File coding system used by Japanese TeX.")
(cond
 (YaTeX-emacs-20
  (setq YaTeX-coding-system
	(cdr (assoc YaTeX-kanji-code YaTeX-kanji-code-alist))))
 ((boundp 'MULE)
  (setq YaTeX-coding-system
	(symbol-value (cdr (assoc YaTeX-kanji-code YaTeX-kanji-code-alist))))))

(defvar YaTeX-mode-syntax-table nil
  "*Syntax table for yatex-mode")

(if YaTeX-mode-syntax-table nil
  (setq YaTeX-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n " " YaTeX-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?\} "){" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?\t " " YaTeX-mode-syntax-table)
  (modify-syntax-entry ?\f ">" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?\n ">" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?$ "$$" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?% "<" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" YaTeX-mode-syntax-table)
  (modify-syntax-entry ?~ " " YaTeX-mode-syntax-table))

;---------- Provide YaTeX-mode ----------
;;;
;; Major mode definition
;;;
(defun yatex-mode ()
  "  Yet Another LaTeX mode: Major mode for editing input files of LaTeX.
-You can invoke processes concerning LaTeX typesetting by
 		\\[YaTeX-typeset-menu]
-Complete LaTeX environment form of `\\begin{env} ... \\end{env}' by
		\\[YaTeX-make-begin-end]
-Enclose region into some environment by
		\\[universal-argument] \\[YaTeX-make-begin-end]
-Complete LaTeX command which takes argument like `\\section{}' by
		\\[YaTeX-make-section]
-Put LaTeX command which takes no arguments like `\\maketitle' by
		\\[YaTeX-make-singlecmd]
-Complete font or character size descriptor like `{\\large }' by
		\\[YaTeX-make-fontsize]
-Enclose region into those descriptors above by
		\\[universal-argument] \\[YaTeX-make-fontsize]
-Enter European accent notations by
		\\[YaTeX-make-accent]
-Toggle various modes of YaTeX by
		\\[YaTeX-switch-mode-menu]
-Change environt name (on the begin/end line) by
		\\[YaTeX-change-*]
-Kill LaTeX command/environment sequences by
		\\[YaTeX-kill-*]
-Kill LaTeX command/environment with its contents 
		\\[universal-argument] \\[YaTeX-kill-*]
-Go to corresponding object (begin/end, file, labels) by
		\\[YaTeX-goto-corresponding-*]   or
		\\[YaTeX-goto-corresponding-*-other-window]   (in other window)
		\\[YaTeX-goto-corresponding-*-other-frame]   (in other frame)
-Go to main LaTeX source text by
		\\[YaTeX-visit-main]   or
		\\[YaTeX-visit-main-other-window]   (in other window)
		\\[YaTeX-visit-main-other-frame]   (in other frame)
-Comment out or uncomment region by
		\\[YaTeX-comment-region]  or  \\[YaTeX-uncomment-region]
-Comment out or uncomment paragraph by
		\\[YaTeX-comment-paragraph]  or  \\[YaTeX-uncomment-paragraph]
-Make an \\item entry hang-indented by
		\\[YaTeX-fill-item]
-Enclose the region with parentheses by
		\\[YaTeX-insert-parens-region]
		\\[YaTeX-insert-braces-region]
		\\[YaTeX-insert-brackets-region]
		\\[YaTeX-insert-dollars-region]
-Look up the corresponding column header of tabular environment by
		\\[YaTeX-what-column]
-Enter a newline and an entry suitable for environment by
		\\[YaTeX-intelligent-newline]
-View the structure of file inclusion by
		\\[YaTeX-display-hierarchy]
-Refer the online help of popular LaTeX commands by
		\\[YaTeX-help]   (help)
		\\[YaTeX-apropos]   (apropos)
-Edit `%# notation' by
		\\[YaTeX-%-menu]

  Those are enough for fastening your editing of LaTeX source.  But further
more features are available and they are documented in the manual.
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'yatex-mode)
  (setq mode-name (if YaTeX-japan "やてふ" "YaTeX"))
  (mapcar 'make-local-variable
	  '(dvi2-command fill-column fill-prefix
	    tmp-env-table tmp-section-table tmp-fontsize-table
	    tmp-singlecmd-table paragraph-start paragraph-separate
	    YaTeX-math-mode indent-line-function comment-line-break-function
	    comment-start comment-start-skip
	    ))
  (cond ((null YaTeX-kanji-code)
	 nil)
	((boundp 'MULE)
	 (set-file-coding-system  YaTeX-coding-system))
	((and YaTeX-emacs-20 (boundp 'buffer-file-coding-system))
	 (setq buffer-file-coding-system
	       (or (and (fboundp 'set-auto-coding) buffer-file-name
			(save-excursion
			  (goto-char (point-min))
			  (set-auto-coding buffer-file-name (buffer-size))))
		   YaTeX-coding-system)))
	((featurep 'mule)
	 (set-file-coding-system YaTeX-coding-system))
	((boundp 'NEMACS)
	 (make-local-variable 'kanji-fileio-code)
	 (setq kanji-fileio-code YaTeX-kanji-code)))
  (setq fill-column YaTeX-fill-column
	fill-prefix YaTeX-fill-prefix
	paragraph-start    YaTeX-paragraph-start
	paragraph-separate YaTeX-paragraph-separate
	indent-line-function 'YaTeX-indent-line
	comment-start YaTeX-comment-prefix
	comment-end ""
	comment-start-skip "[^\\\\]%+[ \t]*"
	local-abbrev-table yatex-mode-abbrev-table)
  (if (fboundp 'comment-indent-new-line) ;for Emacs21
      (setq comment-line-break-function 'YaTeX-comment-line-break))

  (if (and YaTeX-use-font-lock (featurep 'font-lock))
      (progn
	(require 'yatex19)
	(YaTeX-font-lock-set-default-keywords)
	(or  (featurep 'xemacs)
	     (set (make-local-variable 'font-lock-defaults)
		  (get 'yatex-mode 'font-lock-defaults)))
	;;(font-lock-mode 1)
	))
  (use-local-map YaTeX-mode-map)
  (set-syntax-table YaTeX-mode-syntax-table)
  (if YaTeX-dos (setq YaTeX-saved-screen-height (screen-height)))
  (YaTeX-read-user-completion-table)
  (and (fboundp 'YaTeX-hilit-setup-alist) (YaTeX-hilit-setup-alist))
  (makunbound 'inenv)
  (turn-on-auto-fill)			;1.63
  (and (= 0 (buffer-size)) (file-exists-p YaTeX-template-file)
       (y-or-n-p (format "Insert %s?" YaTeX-template-file))
       (insert-file-contents (expand-file-name YaTeX-template-file)))
  (run-hooks 'text-mode-hook 'yatex-mode-hook))

;---------- Define YaTeX-mode functions ----------
(defvar YaTeX-ec "\\" "Escape character of current mark-up language.")
(defvar YaTeX-ec-regexp (regexp-quote YaTeX-ec))
(defvar YaTeX-struct-begin
  (concat YaTeX-ec "begin{%1}%2")
  "Keyword format of begin-environment.")
(defvar YaTeX-struct-end
  (concat YaTeX-ec "end{%1}")
  "Keyword format of end-environment.")
(defvar YaTeX-struct-name-regexp "[^}]+"
  "Environment name regexp.")
(defvar YaTeX-TeX-token-regexp
  (cond (YaTeX-japan "[A-Za-z*ぁ-ん亜-龠]+")
	(t "[A-Za-z*]+"))
  "Regexp of characters which can be a member of TeX command's name.")
(defvar YaTeX-kanji-regexp "[ぁ-ん亜-龠]"
  "Generic regexp of Japanese Kanji (and symbol) characters.")
(defvar YaTeX-command-token-regexp YaTeX-TeX-token-regexp
  "Regexp of characters which can be a member of current mark up language's command name.")

;;(defvar YaTeX-struct-section
;;  (concat YaTeX-ec "%1{%2}")
;;  "Keyword to make section.")

;;;
;; autoload section
;;;

;;autoload from yatexprc.el
(autoload 'YaTeX-visit-main "yatexprc" "Visit main LaTeX file." t)
(autoload 'YaTeX-visit-main-other-window "yatexprc"
  "Visit main other window." t)
(autoload 'YaTeX-main-file-p "yatexprc" "Check if the file is main." t)
(autoload 'YaTeX-get-builtin "yatexprc" "Get %# built-in." t)
(autoload 'YaTeX-system "yatexprc" "Call system command" t)
(autoload 'YaTeX-save-buffers "yatexprc" "Save buffers of same major mode" t)

;;autoload from yatexmth.el
(autoload 'YaTeX-math-insert-sequence "yatexmth" "Image input." t)
(autoload 'YaTeX-in-math-mode-p "yatexmth" "Check if in math-env." t)
(autoload 'YaTeX-toggle-math-mode "yatexmth" "YaTeX math-mode interfaces." t)
(autoload 'YaTeX-math-member-p "yatexmth" "Check if a word is math command." t)
(autoload 'YaTeX-insert-amsparens-region "yatexmth" "AMS parens region" t)
(autoload 'YaTeX-insert-amsbraces-region "yatexmth" "AMS braces region" t)
(autoload 'YaTeX-insert-amsbrackets-region "yatexmth" "AMS brackets region" t)
(autoload 'YaTeX-on-parenthesis-p "yatexmth" "Check if on math-parens" t)
(autoload 'YaTeX-goto-open-paren "yatexmth" "Goto opening paren" t)
(autoload 'YaTeX-change-parentheses "yatexmth" "Change corresponding parens" t)
(autoload 'YaTeX-goto-corresponding-paren "yatexmth" "\bigl\bigr jumps" t)

;;autoload from yatexhlp.el
(autoload 'YaTeX-help "yatexhlp" "YaTeX helper with LaTeX commands." t)
(autoload 'YaTeX-apropos "yatexhlp" "Apropos for (La)TeX commands." t)

;;autoload from yatexgen.el
(autoload 'YaTeX-generate "yatexgen" "YaTeX add-in function generator." t)
(autoload 'YaTeX-generate-simple "yatexgen" "YaTeX add-in support." t)

;;autoload from yatexsec.el
(autoload 'YaTeX-section-overview "yatexsec" "YaTeX sectioning(view)" t)
(autoload 'YaTeX-read-section-in-minibuffer "yatexsec" "YaTeX sectioning" t)
(autoload 'YaTeX-make-section-with-overview "yatexsec" "YaTeX sectioning" t)

;;autoload from yatexenv.el
(autoload 'YaTeX-what-column "yatexenv" "YaTeX env. specific funcs" t)
(autoload 'YaTeX-intelligent-newline "yatexenv" "YaTeX env. specific funcs" t)
(autoload 'YaTeX-indent-line-equation "yatexenv" "Indent equation lines." t)
(autoload 'YaTeX-goto-corresponding-leftright "yatexenv" "\left\right jumps" t)

;;autoload from yatexhie.el
(autoload 'YaTeX-display-hierarchy "yatexhie"
  "YaTeX document hierarchy browser" t)
(autoload 'YaTeX-display-hierarchy-directly "yatexhie"
  "Same as YaTeX-display-hierarchy.  Call from mouse." t)

;;autoload from yatexpkg.el
(autoload 'YaTeX-package-auto-usepackage "yatexpkg" "Auto \\usepackage" t)

;;;
;; YaTeX-mode functions
;;;
(defun YaTeX-insert-begin-end (env region-mode)
  "Insert \\begin{mode-name} and \\end{mode-name}.
This works also for other defined begin/end tokens to define the structure."
  (setq YaTeX-current-completion-type 'begin)
  (let*((ccol (current-column)) beg beg2 exchange
	(arg region-mode)		;for old compatibility
	(indent-column (+ ccol YaTeX-environment-indent))(i 1) func)
    (if (and region-mode (> (point) (mark)))
	(progn (exchange-point-and-mark)
	       (setq exchange t
		     ccol (current-column)
		     indent-column (+ ccol YaTeX-environment-indent))))
    ;;VER2 (insert "\\begin{" env "}" (YaTeX-addin env))
    (setq beg (point))
    (YaTeX-insert-struc 'begin env)
    (setq beg2 (point))
    (insert "\n")
    (indent-to indent-column)
    (save-excursion
      ;;indent optional argument of \begin{env}, if any
      (while (> (point-beginning-of-line) beg)
	(skip-chars-forward "\\s " (point-end-of-line))
	(indent-to indent-column)
	(forward-line -1)))
    (require 'yatexenv)
    (if region-mode
	  ;;if region-mode, indent all text in the region
	(save-excursion
	  (if (fboundp (intern-soft (concat "YaTeX-enclose-" env)))
	      (funcall (intern-soft (concat "YaTeX-enclose-" env))
		       (point) (mark))
	    (while (< (progn (forward-line 1) (point)) (mark))
	      (if (eolp) nil
		(skip-chars-forward " \t\n")
		(indent-to indent-column))))))
    (if region-mode (exchange-point-and-mark))
    (indent-to ccol)
    ;;VER2 (insert "\\end{" env "}\n")
    (YaTeX-insert-struc 'end env)
    (YaTeX-reindent ccol)
    (if region-mode
	(progn
	  (insert "\n")
	  (or exchange (exchange-point-and-mark)))
      (goto-char beg2)
      (YaTeX-intelligent-newline nil)
      (YaTeX-indent-line))
    (YaTeX-package-auto-usepackage env 'env)
    (if YaTeX-current-position-register
	(point-to-register YaTeX-current-position-register))))

(defun YaTeX-make-begin-end (arg)
  "Make LaTeX environment command of \\begin{env.} ... \\end{env.}
by completing read.
 If you invoke this command with universal argument,
\(key binding for universal-argument is \\[universal-argument]\)
you can put REGION into that environment between \\begin and \\end."
  (interactive "P")
  (let*
      ((mode (if arg " region" ""))
       (env
	(YaTeX-read-environment
	 (format "Begin environment%s(default %s): " mode YaTeX-env-name))))
    (if (string= env "")
	(setq env YaTeX-env-name))
    (setq YaTeX-env-name env)
    (YaTeX-update-table
     (list YaTeX-env-name) 'env-table 'user-env-table 'tmp-env-table)
    (YaTeX-insert-begin-end YaTeX-env-name arg)))

(defun YaTeX-make-begin-end-region ()
  "Call YaTeX-make-begin-end with ARG to specify region mode."
  (interactive)
  (YaTeX-make-begin-end t))

(defun YaTeX-guess-section-type ()
  (if (eq major-mode 'yatex-mode)
      (save-excursion
	(cond
	 ((save-excursion (not (search-backward YaTeX-ec nil t)))
	  (if YaTeX-use-LaTeX2e "documentclass" "documentstyle"))
	 ((progn
	    (if (= (char-after (1- (point))) ?~) (forward-char -1))
	    (forward-char -1) (looking-at "表\\|図\\|式\\|第"))
	  "ref")
	 ((and (looking-at "[a-z \t]")
	       (progn (skip-chars-backward "a-z \t")
		      (looking-at "table\\|figure\\|formula")))
	  "ref")
	 ((save-excursion
	    (skip-chars-backward "[^ア-ン]")
	    (looking-at "プログラム\\|リスト"))
	  "ref")
	 ((YaTeX-re-search-active-backward
	   (concat YaTeX-ec-regexp "begin{\\([^}]+\\)}")
	   (regexp-quote YaTeX-comment-prefix)
	   (save-excursion (forward-line -1) (point))
	   t)
	  (let ((env (YaTeX-match-string 1)))
	    (cdr (assoc env
			'(("table" . "caption"))))))
	 ))))

(defun YaTeX-make-section (arg &optional beg end cmd)
  "Make LaTeX \\section{} type command with completing read.
With numeric ARG, you can specify the number of arguments of
LaTeX command.
  For example, if you want to produce LaTeX command

	\\addtolength{\\topmargin}{8mm}

which has two arguments.  You can produce that sequence by typing...
	ESC 2 C-c s add SPC RET \\topm SPC RET 8mm RET
\(by default\)
Then yatex will automatically complete `addtolength' with two arguments
next time.
  You can complete symbol at LaTeX command and the 1st argument.

If the optional 2nd and 3rd argument BEG END are specified, enclose
the region from BEG to END into the first argument of the LaTeX sequence.
Optional 4th arg CMD is LaTeX command name, for non-interactive use."
  (interactive "P")
  (setq YaTeX-current-completion-type 'section)
  (if (equal arg '(4)) (setq beg (region-beginning) end (region-end)))
  (unwind-protect
      (let*
	  ((source-window (selected-window))
	   guess
	   (section
	    (or cmd
		(progn
		  (setq guess
			(or (YaTeX-guess-section-type) YaTeX-section-name))
		  (YaTeX-read-section
		   (if YaTeX-simple-messages
		       (format "Section-type (default %s): " guess)
		     (if (> (minibuffer-depth) 0)
			 (format "%s???{} (default %s)%s: "
				 YaTeX-ec guess
				 (format "[level:%d]" (minibuffer-depth)))
		       (format "(C-v for view-section) %s???{%s} (default %s): "
			       YaTeX-ec (if beg "region" "") guess)))
		   nil))))
	   (section (if (string= section "") guess section))
	   (numarg	;; The number of section-type command's argument
	    (or (and (numberp arg) arg)
		(nth 1 (YaTeX-lookup-table section 'section))
		1))
	   (arg-reader (intern-soft (concat "YaTeX::" section)))
	   (addin-args (and arg-reader (fboundp arg-reader)))
	   (title "")
	   (j 1)
	   (after-change-functions nil)	;inhibit font-locking temporarily
	   (enable-recursive-minibuffers t)
	   (mkarg-func
	    (function
	     (lambda (n)
	       (while (<= j n)
		 (insert
		  (concat		;to allow nil return value
		   "{"
		   (setq title
			 (cond
			  (addin-args (funcall arg-reader j))
			  (YaTeX-skip-default-reader "")
			  (t
			   (read-string
			    (format "Argument %d of %s: " j section)))))
		   "}"))
		 (setq j (1+ j))))))
	   );;let
	(setq YaTeX-section-name section)
	(if beg
	    (let*((e (make-marker))
		  (ar2 (intern-soft (concat "YaTeX::" section "-region")))
		  (arp (and ar2 (fboundp ar2))))
	      (goto-char end)
	      (insert "}")
	      (set-marker e (point))
	      (goto-char beg)
	      (insert YaTeX-ec YaTeX-section-name
		      (YaTeX-addin YaTeX-section-name))
	      (if (> numarg 1) (funcall mkarg-func (1- numarg)))
	      (insert "{")
	      (if arp (funcall ar2 (point) e))
	      (goto-char e)
	      (set-marker e nil))
	  (use-global-map YaTeX-recursive-map)
	  (if (= numarg 0) (YaTeX-make-singlecmd YaTeX-section-name)
	    (progn (insert YaTeX-ec YaTeX-section-name)
		   (insert (YaTeX-addin YaTeX-section-name))))
	  ;;read arguments with add-in
	  (funcall mkarg-func numarg))
	(YaTeX-update-table
	 (if (/= numarg 1) (list section numarg)
	   (list section))
	 'section-table 'user-section-table 'tmp-section-table)
	(if YaTeX-current-position-register
	    (point-to-register YaTeX-current-position-register))
	(if (string= (buffer-substring (- (point) 2) (point)) "{}")
	  (forward-char -1))
	(while (string= (buffer-substring (- (point) 3) (1- (point))) "{}")
	  (forward-char -2))
	(YaTeX-package-auto-usepackage section 'section))
    (if (<= (minibuffer-depth) 0) (use-global-map global-map))
    (insert "")))		;insert dummy string to fontify(Emacs20)

(defun YaTeX-make-section-region (args beg end)
  "Call YaTeX-make-section with arguments to specify region mode."
 (interactive "P\nr")
 (YaTeX-make-section args beg end))

(defun YaTeX-make-fontsize (arg &optional fontsize)
  "Make completion like {\\large ...} or {\\slant ...} in minibuffer.
If you invoke this command with universal argument, you can put region
into {\\xxx } braces.
\(key binding for universal-argument is \\[universal-argument]\)"
  (interactive "P")
  (YaTeX-sync-local-table 'tmp-fontsize-table)
  (let* ((mode (if arg "region" ""))
	 (fontsize
	  (or fontsize
	      (YaTeX-read-fontsize
	       (if YaTeX-simple-messages
		   (format "Font or size (default %s): " YaTeX-fontsize-name)
		 (format "{\\??? %s} (default %s)%s: " mode YaTeX-fontsize-name
			 (if (> (minibuffer-depth) 0)
			     (format "[level:%d]" (minibuffer-depth)) "")))
	       nil nil))))
    (if (string= fontsize "")
	(setq fontsize YaTeX-fontsize-name))
    (setq YaTeX-current-completion-type 'large)
    (setq YaTeX-fontsize-name fontsize)
    (YaTeX-update-table
     (list YaTeX-fontsize-name)
     'fontsize-table 'user-fontsize-table 'tmp-fontsize-table)
    (and YaTeX-use-LaTeX2e
	 (YaTeX-latex2e-p)
	 (setq fontsize
	       (cdr (assoc YaTeX-fontsize-name LaTeX2e-fontstyle-alist)))
	 (setq YaTeX-fontsize-name fontsize))
    (if arg
	(save-excursion
	  (if (> (point) (mark)) (exchange-point-and-mark))
	  (insert "{\\" YaTeX-fontsize-name " ")
	  (exchange-point-and-mark)
	  (insert "}"))
      (insert (concat "{\\" YaTeX-fontsize-name " }"))
      (forward-char -1)
      (if YaTeX-current-position-register
	  (point-to-register YaTeX-current-position-register))
      (save-excursion
	(insert (YaTeX-addin YaTeX-fontsize-name)))
      (YaTeX-package-auto-usepackage YaTeX-fontsize-name 'large))))

(defun YaTeX-make-fontsize-region ()
  "Call function:YaTeX-make-fontsize with ARG to specify region mode."
  (interactive)
  (YaTeX-make-fontsize t))

(defvar YaTeX-singlecmd-suffix "" "*Suffix for maketitle-type commands.")
(defvar YaTeX-read-singlecmd-history nil "Holds maketitle-type history.")
(put 'YaTeX-read-singlecmd-history 'no-default t)
(defun YaTeX-make-singlecmd (single)
  (interactive
   (list (YaTeX-cplread-with-learning
	  (if YaTeX-simple-messages
	      (format "maketitle-type (default %s): " YaTeX-single-command)
	    (format "%s??? (default %s)%s: " YaTeX-ec YaTeX-single-command
		    (if (> (minibuffer-depth) 0)
			(format "[level:%d]" (minibuffer-depth)) "")))
	  'singlecmd-table 'user-singlecmd-table 'tmp-singlecmd-table
	  nil nil nil 'YaTeX-read-singlecmd-history)))
  (if (string= single "")
      (setq single YaTeX-single-command))
  (setq YaTeX-single-command single)
  (setq YaTeX-current-completion-type 'maketitle)
  (let ((dollar (and (not (YaTeX-in-math-mode-p))
		     (YaTeX-math-member-p YaTeX-single-command)))
	p q)
    (if dollar (insert "$"))
    (insert YaTeX-ec YaTeX-single-command)
    (setq p (point))
    (insert (YaTeX-addin single) YaTeX-singlecmd-suffix)
    (if dollar (insert "$"))
    (setq q (point))
    (goto-char p)
    (forward-char -2)
    (if (looking-at "\\[\\]") (forward-char 1) (goto-char q)))
  (YaTeX-package-auto-usepackage YaTeX-single-command 'maketitle)
  (if YaTeX-current-position-register
      (point-to-register YaTeX-current-position-register)))

(defvar YaTeX-completion-begin-regexp "[{\\]"
  "Regular expression of limit where LaTeX command's completion begins.")

(defun YaTeX-do-completion ()
  "Try completion on LaTeX command preceding point."
  (interactive)
  (if
      (or (eq (preceding-char) ? )
	  (eq (preceding-char) ?\t)
	  (eq (preceding-char) ?\n)
	  (bobp))
      (message "Nothing to complete.")   ;Do not complete
    (let* ((end (point))
	   (limit (point-beginning-of-line))
	   (completion-begin 
	    (progn (re-search-backward "[ \t\n]" limit 1) (point)))
	   (begin (progn
		    (goto-char end)
		    (if (re-search-backward YaTeX-completion-begin-regexp
					    completion-begin t)
			(1+ (point))
		      nil))))
      (goto-char end)
      (cond
       ((null begin)
	(message "I think it is not a LaTeX sequence."))
       (t
	(mapcar 'YaTeX-sync-local-table
		'(tmp-section-table tmp-env-table tmp-singlecmd-table))
	(let*((pattern (buffer-substring begin end))
	      (all-table
	       (append
		section-table user-section-table tmp-section-table
		env-table     user-env-table     tmp-env-table
		singlecmd-table user-singlecmd-table tmp-singlecmd-table))
	      ;; First,
	      ;; search completion without backslash.
	      (completion (try-completion pattern all-table)))
	  (if
	      (eq completion nil)
	      ;; Next,
	      ;; search completion with backslash
	      (setq completion
		    (try-completion (buffer-substring (1- begin) end)
				    all-table nil)
		    begin (1- begin)))
	  (cond
	   ((null completion)
	    (message (concat "Can't find completion for '" pattern "'"))
	    (ding))
	   ((eq completion t) (message "Sole completion."))
	   ((not (string= completion pattern))
	    (delete-region begin end)
	    (insert completion)
	    )
	   (t
	    (message "Making completion list...")
	    (with-output-to-temp-buffer "*Help*"
	      (display-completion-list
	       (all-completions pattern all-table)))))))))))

(defun YaTeX-toggle-modify-mode (&optional arg)
  (interactive "P")
  (or (memq 'YaTeX-modify-mode mode-line-format)
      (setq mode-line-format
	    (append (list "" 'YaTeX-modify-mode) mode-line-format)))
  (if (or arg (null YaTeX-modify-mode))
      (progn
	(setq YaTeX-modify-mode "*m*")
	(message "Modify mode"))
    (setq YaTeX-modify-mode nil)
    (message "Cancel modify mode."))
  (set-buffer-modified-p (buffer-modified-p)))	;redraw mode-line

(defun YaTeX-switch-mode-menu (arg &optional char)
  (interactive "P")
  (message "Toggle: (M)odify-mode ma(T)h-mode")
  (let ((c (or char (read-char))))
    (cond
     ((= c ?m) (YaTeX-toggle-modify-mode arg))
     ((or (= c ?$) (= c ?t))
      (if YaTeX-auto-math-mode
	  (message "Makes no sense in YaTeX-auto-math-mode.")
	(YaTeX-toggle-math-mode arg))))))

(defun YaTeX-insert-quote ()
  (interactive)
  (insert
   (cond
    ((YaTeX-literal-p) ?\")
    ((= (preceding-char) ?\\ ) ?\")
    ;((= (preceding-char) ?\( ) ?\")
    ((or (= (preceding-char) 32)
	 (= (preceding-char) 9)
	 (= (preceding-char) ?\n)
	 (bobp)
	 (string-match
	  (regexp-quote (char-to-string (preceding-char)))
	  "、。，．？！「」『』【】()"))
     "``")
    (t  "''"))))

(defun YaTeX-closable-p ()
  (and (not YaTeX-modify-mode)
       (not (eq YaTeX-close-paren-always 'never))
       (or YaTeX-close-paren-always (eolp))
       (not (input-pending-p))
       (not (YaTeX-literal-p)))
  ;;(or YaTeX-modify-mode
  ;;    (and (not YaTeX-close-paren-always) (not (eolp)))
  ;;    (input-pending-p)
  ;;    (YaTeX-quick-in-environment-p "verbatim"))
  )

(defun YaTeX-insert-braces-region (beg end &optional open close)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert (or close "}"))
    (goto-char beg)
    (insert (or open "{"))))

(defun YaTeX-insert-braces (arg &optional open close)
  (interactive "p")
  (let (env)
    (cond
     ((YaTeX-jmode) (YaTeX-self-insert arg))
     ((not (YaTeX-closable-p)) (YaTeX-self-insert arg))
     ((save-excursion
	(and (> (- (point) (point-min)) 6)
	     (condition-case () (forward-char -6) (error nil)))
	(looking-at "\\\\left\\\\"))
      (insert "{\\right\\}")
      (forward-char -8))
     ((save-excursion			;from matsu@math.s.chiba-u.ac.jp
	(and (> (- (point) (point-min)) 6) (forward-char -6))
	(looking-at "\\\\[bB]igl\\\\"))
      (insert
       (concat
	"{" (buffer-substring (match-beginning 0) (- (match-end 0) 2)) "r\\}"))
      (forward-char -7))
     ((save-excursion
	(and (> (- (point) (point-min)) 7)
	     (condition-case () (forward-char -7) (error nil)))
	(looking-at "\\\\[bB]iggl\\\\"))
      (insert
       (concat
	"{" (buffer-substring (match-beginning 0) (- (match-end 0) 2)) "r\\}"))
      (forward-char -8))
     ((= (preceding-char) ?\\ )
      (insert "{\\}")
      (forward-char -2))		;matsu's hack ends here
     ((and (> (point) (+ (point-min) 4))
	   (save-excursion (backward-char 4) (looking-at "\\\\end"))
	   (not (YaTeX-literal-p))
	   (setq env (YaTeX-inner-environment)))
      (momentary-string-display
       (concat
	"{"
	(cond
	 (YaTeX-japan
	  (format "今度からはちゃんと %s b を使いましょう" YaTeX-prefix))
	 (t (format "You don't understand Zen of `%s b':p" YaTeX-prefix)))
	"}")
       (point))
      (insert (or open "{") env (or close "}")))
     (t
      (insert (or open "{") (or close "}"))
      (forward-char -1)
      (if (and (eq (char-after (point)) ?\})
	       (eq (char-after (- (point) 2)) ?\\ ))
	  (progn (insert "\\") (forward-char -1)))
      ))))

(defun YaTeX-jmode ()
  (or (and (boundp 'canna:*japanese-mode*) canna:*japanese-mode*)
      (and (boundp 'egg:*mode-on*) egg:*mode-on* egg:*input-mode*)))

(defun YaTeX-jmode-off ()
  (cond
   ((and (boundp 'canna:*japanese-mode*) canna:*japanese-mode*)
    (canna-toggle-japanese-mode))
   ((and (boundp 'egg:*mode-on*) egg:*mode-on* egg:*input-mode*)
    (egg:toggle-egg-mode-on-off))
   ((and (fboundp 'skk-mode) (boundp 'skk-mode) skk-mode)
    (cond
     ((fboundp 'skk-latin-mode)	(skk-latin-mode t))
     ((fboundp 'skk-mode-off)	(skk-mode-off))
     (t (j-mode-off))))
   ((and (fboundp 'toggle-input-method) current-input-method)
    (toggle-input-method))
   ((and (fboundp 'fep-force-off) (fep-force-off)))))

(defun YaTeX-self-insert (arg)
  (call-interactively (global-key-binding (char-to-string last-command-char))))

(defun YaTeX-insert-brackets (arg)
  "Insert Kagi-kakko or \\ [ \\] pair or simply \[."
  (interactive "p")
  (let ((col (1- (current-column))))
    (cond
     ((YaTeX-jmode) (YaTeX-self-insert arg))
     ((not (YaTeX-closable-p))
      (YaTeX-self-insert arg))
     ((save-excursion
	(and (> (- (point) (point-min)) 5) (forward-char -5))
	(looking-at "\\\\left"))
      (insert "[\\right]")
      (forward-char -7))
     ((save-excursion			;from matsu@math.s.chiba-u.ac.jp
	(and (> (- (point) (point-min)) 5) (forward-char -5))
	(looking-at "\\\\[bB]igl"))
      (insert
       (concat
	"[" (buffer-substring (match-beginning 0) (- (match-end 0) 1)) "r]"))
      (forward-char -6))
     ((save-excursion
	(and (> (- (point) (point-min)) 6) (forward-char -6))
	(looking-at "\\\\[bB]iggl"))
      (insert
       (concat
	"[" (buffer-substring (match-beginning 0) (- (match-end 0) 1)) "r]"))
      (forward-char -7))		;matsu's hack ends here
     ((and (= (preceding-char) ?\\ )
	   (/= (char-after (- (point) 2)) ?\\ )
	   (not (YaTeX-in-math-mode-p)))
      (insert last-command-char "\n")
      (indent-to (max 0 col))
      (insert "\\]")
      (beginning-of-line)
      (open-line 1)
      (delete-region (point) (progn (beginning-of-line) (point)))
      (indent-to (+ YaTeX-environment-indent (max 0 col)))
      (or YaTeX-auto-math-mode YaTeX-math-mode (YaTeX-toggle-math-mode 1)))
     ((YaTeX-closable-p)
      (insert "[]")
      (backward-char 1))
     (t (YaTeX-self-insert arg)))))

(defun YaTeX-insert-brackets-region (beg end)
  (interactive "r")
  (YaTeX-insert-braces-region beg end "[" "]"))

(defun YaTeX-insert-parens (arg)
  "Insert parenthesis pair."
  (interactive "p")
  (cond
   ((YaTeX-jmode) (YaTeX-self-insert arg))
   ((not (YaTeX-closable-p)) (YaTeX-self-insert arg))
   ((save-excursion
      (and (> (- (point) (point-min)) 5) (forward-char -5))
      (looking-at "\\\\left"))
    (insert "(\\right)")
    (forward-char -7))
   ((save-excursion			;from matsu@math.s.chiba-u.ac.jp
      (and (> (- (point) (point-min)) 5) (forward-char -5))
      (looking-at "\\\\[bB]igl"))
    (insert
     (concat
      "(" (buffer-substring (match-beginning 0) (- (match-end 0) 1)) "r)"))
     (forward-char -6))
   ((save-excursion
      (and (> (- (point) (point-min)) 6) (forward-char -6))
      (looking-at "\\\\[bB]iggl"))
    (insert
     (concat
      "(" (buffer-substring (match-beginning 0) (- (match-end 0) 1)) "r)"))
     (forward-char -7))
   ((= (preceding-char) ?\\ )		;matsu's hack ends here
    (insert "(\\)")
    (backward-char 2))
   ((YaTeX-closable-p)
    (insert "()")
    (backward-char 1))
   (t (YaTeX-self-insert arg))))

(defun YaTeX-insert-parens-region (beg end)
  (interactive "r")
  (YaTeX-insert-braces-region beg end "(" ")"))

(defun YaTeX-insert-bar (arg)
  "Insert bar pair."
  (interactive "p")
  (cond
   ((YaTeX-jmode) (YaTeX-self-insert arg))
   ((not (YaTeX-closable-p)) (YaTeX-self-insert arg))
   ((save-excursion
      (and (> (- (point) (point-min)) 5) (forward-char -5))
      (looking-at "\\\\left"))
    (insert "|\\right|")
    (forward-char -7))
   ((save-excursion			;from matsu@math.s.chiba-u.ac.jp
      (and (> (- (point) (point-min)) 5) (forward-char -5))
      (looking-at "\\\\[bB]igl"))
    (insert
     (concat
      "|" (buffer-substring (match-beginning 0) (- (match-end 0) 1)) "r|"))
     (forward-char -6))
   ((save-excursion
      (and (> (- (point) (point-min)) 6) (forward-char -6))
      (looking-at "\\\\[bB]iggl"))
    (insert
     (concat
      "|" (buffer-substring (match-beginning 0) (- (match-end 0) 1)) "r|"))
     (forward-char -7))
   ((save-excursion		; added by Jin <MAF01011@nifty.ne.jp>
      (and (> (- (point) (point-min)) 6) (forward-char -6))
      (looking-at "\\\\left\\\\"))
    (insert "|\\right\\|")
    (forward-char -8))
   ((save-excursion
      (and (> (- (point) (point-min)) 6) (forward-char -6))
      (looking-at "\\\\[bB]igl\\\\"))
    (insert
     (concat
      "|" (buffer-substring (match-beginning 0) (- (match-end 0) 2)) "r\\|"))
     (forward-char -7))
   ((save-excursion
      (and (> (- (point) (point-min)) 7) (forward-char -7))
      (looking-at "\\\\[bB]iggl\\\\"))
    (insert
     (concat
      "|" (buffer-substring (match-beginning 0) (- (match-end 0) 2)) "r\\|"))
     (forward-char -8))		; added by Jin up to here.
   ((= (preceding-char) ?\\ )
    (insert "|\\|")
    (backward-char 2))
;   ((and (YaTeX-closable-p)
;	 (/= (preceding-char) ?|)
;	 (/= (following-char) ?|))
;    (insert "||")
;    (backward-char 1))
   (t (YaTeX-self-insert arg))))

(defun YaTeX-insert-dollar ()
  (interactive)
  (if (or (not (YaTeX-closable-p))
	  (= (preceding-char) 92)
	  (and (YaTeX-in-math-mode-p)
	       (or (/= (preceding-char) ?$) (/= (following-char) ?$))))
      (insert "$")
    (insert "$$")
    (forward-char -1)
    (YaTeX-jmode-off)
    (or YaTeX-auto-math-mode YaTeX-math-mode (YaTeX-toggle-math-mode 1))))

(defun YaTeX-insert-dollars-region (beg end)
  (interactive "r")
  (YaTeX-insert-braces-region beg end "$" "$"))

(defun YaTeX-insert-amper ()
  (interactive)
  (if (or (string-match YaTeX-array-env-regexp
			(or (YaTeX-inner-environment t) "document"))
	  (= (preceding-char) 92)
	  (YaTeX-literal-p)
	  (YaTeX-in-math-mode-p))
      (insert "&")
    (insert "\\&")))

(defun YaTeX-version ()
  "Return string of the version of running YaTeX."
  (interactive)
  (message
   (concat "Yet Another tex-mode "
	   (if YaTeX-japan "「野鳥」" "`Wild Bird'")
	   " Revision "
	   YaTeX-revision-number)))

(defun YaTeX-typeset-menu (arg &optional char)
  "Typeset, preview, visit error and miscellaneous convenient menu.
Optional second argument CHAR is for non-interactive call from menu."
  (interactive "P")
  (message
   (concat "J)latex R)egion B)ibtex mk(I)ndex "
	   (if (fboundp 'start-process) "K)ill-latex ")
	   "P)review "
	   (and (boundp 'window-system) window-system "S)earch ")
	   "V)iewerr L)pr"))
  (let ((sw (selected-window)) (c (or char (read-char))))
    (require 'yatexprc)			;for Nemacs's bug
    (select-window sw)
    (cond
     ((= c ?j) (YaTeX-typeset-buffer))
     ((= c ?r) (YaTeX-typeset-region))
     ((= c ?b) (YaTeX-call-command-on-file
		bibtex-command "*YaTeX-bibtex*" YaTeX-parent-file))
     ((= c ?i) (YaTeX-call-command-on-file
		makeindex-command "*YaTeX-makeindex*" YaTeX-parent-file))
     ((= c ?k) (YaTeX-kill-typeset-process YaTeX-typeset-process))
     ((= c ?p) (call-interactively 'YaTeX-preview))
     ((= c ?q) (YaTeX-system "lpq" "*Printer queue*"))
     ((= c ?v) (YaTeX-view-error))
     ((= c ?l) (YaTeX-lpr arg))
     ((= c ?m) (YaTeX-switch-mode-menu arg))
     ((= c ?b) (YaTeX-insert-string "\\"))
     ((= c ?s) (YaTeX-xdvi-remote-search arg)))))

(if (fboundp 'wrap-function-to-control-ime)
    (wrap-function-to-control-ime 'YaTeX-typeset-menu t "P"))


(defun YaTeX-%-menu (&optional beg end char)
  "Operate %# notation."
  ;;Do not use interactive"r" for the functions which require no mark
  (interactive)
  (message "!)Edit-%%#! B)EGIN-END-region L)Edit-%%#LPR")
  (let ((c (or char (read-char))) (string "") key
	(b (make-marker)) (e (make-marker)))
    (save-excursion
      (cond
       ((or (= c ?!) (= c ?l))		;Edit `%#!'
	(goto-char (point-min))
	(setq key (cond ((= c ?!) "%#!")
			((= c ?l) "%#LPR")))
	(if (re-search-forward key nil t)
	    (progn
	      (setq string (buffer-substring (point) (point-end-of-line)))
	      (delete-region (point) (progn (end-of-line) (point))))
	  (open-line 1)
	  (delete-region (point) (progn (beginning-of-line)(point)));for 19 :-<
	  (insert key))
	(unwind-protect
	    (setq string (read-string (concat key ": ") string))
	  (insert string)))

       ((= c ?b)			;%#BEGIN %#END region
	(or end (setq beg (min (point) (mark)) end (max (point) (mark))))
	(set-marker b beg)
	(set-marker e end)
	(goto-char (point-min))
	(while (re-search-forward "^%#\\(BEGIN\\)\\|\\(END\\)$" nil t)
	  (beginning-of-line)
	  (delete-region (point) (progn (forward-line 1) (point))))
	(goto-char b)
	(open-line 1)
	(delete-region (point) (progn (beginning-of-line)(point)));for 19 :-<
	(insert "%#BEGIN")
	(goto-char e)
	(insert "%#END\n")
	(set-marker b nil)
	(set-marker e nil))
       ))))

(defun YaTeX-goto-corresponding-label (reverse &optional otherwin)
  "Jump to corresponding \\label{} and \\ref{} or \\cite and \\bibitem.
  The default search direction depends on the command at the cursor position.
When the cursor is on \\ref(\\cite), YaTeX will try to search the
corresponding \\label(\\bibitem) backward,
and if it fails search forward again.  And when the cursor is
on \\label(\\bibitem), YaTeX will search the corresponding \\ref(\\cite)
forward at first and secondary backward.
  Argument REVERSE non-nil makes the default
direction rule reverse.  Since Search string is automatically set in
search-last-string, you can repeat search the same label/ref by typing
\\[isearch-forward] or \\[isearch-backward].
  If optional second argument OTHERWIN is non-nil, move to other window."

  (let ((scmd "") label direc string blist (p (point)) (cb (current-buffer))
	(refcommands "label\\|\\(page\\|eq\\)?ref\\|cite\\|bibitem")
	(foundmsg (format "Type %s %c to return to original position."
			  (key-description
			   (car
			    (or (where-is-internal 'register-to-point)
				(where-is-internal 'jump-to-register))))
			  YaTeX-current-position-register))
	(func (function (lambda (string sfunc)
			  (or
			   (funcall sfunc string nil t)
			   (funcall (if (eq sfunc 're-search-forward)
					're-search-backward 're-search-forward)
				    string nil t))))))
    (cond
     ((YaTeX-on-section-command-p refcommands)
      (setq scmd
	    (cdr
	     (assoc
	      (YaTeX-match-string 1)
	      '(("label" . "\\\\\\(page\\|eq\\)?ref{%k}")
		("ref" . "\\\\label{%k}")
		("eqref" . "\\\\label{%k}")
		("pageref" . "\\\\label{%k}")
		("cite" .
		 "\\\\bibitem\\(\\[[^]]+\\]\\)?{%k}\\|^\\s *@[a-z]+{%k,")
		("bibitem" . "\\\\cite\\(\\[[^]]+\\]\\)?")))))
      (goto-char (match-end 0))
      (let ((label (buffer-substring 
		    (1- (point)) (progn (backward-list 1) (1+ (point)))))
	    (fp (make-marker))fl fn
	    (goother (function (lambda (buffer point)
				 (goto-char point)
				 (if (one-window-p)
				     (split-window-calculate-height
				      YaTeX-default-pop-window-height))
				 (select-window (get-lru-window))
				 (switch-to-buffer buffer)))))
	;(setq string (concat "\\" scmd "{" label "}"))
	;(setq string (concat "\\\\" scmd "{" (regexp-quote label) "}"))
	(setq string (YaTeX-replace-format scmd "k" (regexp-quote label)))
	(setq direc (if (string-match "ref\\|cite" scmd)
			're-search-forward 're-search-backward))
	(if YaTeX-current-position-register
	    (point-to-register YaTeX-current-position-register))
	(if reverse (setq direc (if (eq direc 're-search-forward)
				    're-search-backward 're-search-forward)))
	(if (funcall func string direc)	;label/ref found!
	    (progn
	      (if otherwin (funcall goother cb p))
	      (goto-char (match-beginning 0))
	      (push-mark p))
	  ;;if label/ref not found, search through all yatex buffers.
	  (goto-char p)			;resume position of current buffer
	  (catch 'found
	    (setq blist (YaTeX-yatex-buffer-list))
	    (while blist
	      ;; search for corresponding keyword
	      (set-buffer (car blist))
	      (if (YaTeX-on-section-command-p refcommands)
		  (goto-char (match-beginning 0)))
	      (cond
	       ; cond1
	       ((funcall func string direc)
		(cond
		 (otherwin
		  (set-buffer cb)
		  (funcall goother (car blist) p))
		 ((or (get-buffer-window (car blist))
		      (and YaTeX-emacs-19
			   (get-buffer-window (car blist) t)))
		  (goto-buffer-window (car blist)))
		 (t
		  (switch-to-buffer (car blist))
		  (message foundmsg)))
		(goto-char (match-beginning 0))
		(throw 'found t))
	       ; cond2
	       ((and
		 (string-match "bibitem" scmd)
		 (catch 'found2
		   (save-excursion
		     (goto-char (point-min))
		     (while (YaTeX-re-search-active-forward
			     "\\\\bibliography{\\([^}]*\\)}" "%" nil t)
		       (setq fl (YaTeX-split-string (YaTeX-match-string 1) ","))
		       (while fl
			 (if (or (file-exists-p (setq fn (car fl)))
				 (file-exists-p (setq fn (concat fn ".bib"))))
			     (progn
			       (set-buffer (find-file-noselect fn))
			       (save-excursion
				 (goto-char (point-min))
				 (if (YaTeX-re-search-active-forward
				      string "%" nil t)
				     (throw 'found2
					    (set-marker fp (point)))))))
			 (setq fl (cdr fl)))))))
		(if otherwin
		    (funcall goother (marker-buffer fp) fp)
		  (switch-to-buffer (marker-buffer fp))
		  (goto-char fp))
		(set-marker fp nil)
		(message foundmsg)
		(throw 'found t)))
	      (setq blist (cdr blist)))
	    ;; search for bibliography
	    )))
      (if YaTeX-emacs-19
	  (setq regexp-search-ring
		(cons string (delete string regexp-search-ring)))
	(setq search-last-regexp string)))
     (t nil))))

;;YaTeX-goto-corresponding-environment was moved to yatexlib

(defun YaTeX-goto-corresponding-file (&optional other)
  "Visit or switch buffer of corresponding file,
looking at \\input or \\include or \includeonly on current line."
  (if (not (YaTeX-on-includes-p)) nil
    (let ((parent buffer-file-name) input-file b)
      (save-excursion
	(if (and (re-search-forward "[{%]" (point-end-of-line) t)
		 (= ?{ (char-after (match-beginning 0))))
	    nil
	  (skip-chars-backward "^,{"))
	(setq input-file
	      (buffer-substring
	       (point) (progn (skip-chars-forward "^ ,}") (point))))
	(if (not (string-match "\\.\\(tex\\|sty\\)$" input-file))
	    (setq input-file (concat input-file ".tex"))))
      (cond
       (other (YaTeX-switch-to-buffer-other-window input-file))
       ((setq b (YaTeX-get-file-buffer input-file))
	(goto-buffer-window b))
       (t (YaTeX-switch-to-buffer input-file)))
      (or (YaTeX-get-builtin "!")
	  YaTeX-parent-file
	  (setq YaTeX-parent-file parent)))))

(defun YaTeX-goto-corresponding-BEGIN-END ()
  (if (not (YaTeX-on-BEGIN-END-p)) nil
    (if (cond
	 ((equal (match-beginning 0) (match-beginning 1)) ;if on %#BEGIN
	  (not (search-forward "%#END" nil t)))
	 (t ; if on %#END
	  (not (search-backward "%#BEGIN" nil t))))
	(error "Corresponding %%#BEGIN/END not found."))
    (beginning-of-line)
    t))

(defvar YaTeX-processed-file-regexp-alist nil
  "Alist of regexp of processed file regexp vs. its file name part;
For example, if you include image file with `\\epsfile{file=FILE}' where
`FILE' is processed file.  You might want to view FILE with other previewer
such as ghostview, or want to preview its source which was drawn with
other drawing tool, tgif for example.  Then you should set entire regexp
of including expression and enclose its file name part with \\\\( and \\\\).

 Ex. (\"\\\\\\\\epsfile{[^}]*file=\\\\([^,} ]+\\\\)\\\\(\\\\.e?ps\\\\)?[^}]*}\" 1)

Where the first group surrounded by \\\\( and \\\\) is the file name part
of expression.  So you should set 1 to second element.  And the first
matching group is sent to (image) processor defined by the variable
YaTeX-file-processor-alist. See also the documentation of
YaTeX-file-processor-alist.

↑じゃ良くわかんないすね。例えば tgif hoge.obj して hoge.eps を
\\epsfile{file=hoge.eps} でインクルードしているとしよう。その行で
\[prefix\] g を押した時に tgif を起動して欲しかったら、まず上のような
正規表現を設定する。\\\\(と\\\\)で囲んだところがファイル名になるように
注意する。でファイル名部分が何番目の\\\\(\\\\)になるかをリストの2番目に書く。
すると、その部分が変数 YaTeX-file-processor-alist で定義された
処理プログラムに渡される。というわけ。
ん～やっぱりむずかしいね。分からない時は隣の Lisper に聞くか、
fj野鳥の会で聞こう!
")

(defvar YaTeX-processed-file-regexp-alist-default
  '(("\\\\epsfile\\(\\[[^]]+\\]\\)?{[^},]*file=\\(\\([^,} ]*/\\)?[^,}. ]+\\)\\(\\.e?ps\\)?[^}]*}" 2)
    ("\\\\epsfig{[^},]*fi\\(le\\|gure\\)=\\(\\([^,} ]*/\\)?[^,}. ]+\\)\\(\\.e?ps\\)?[^}]*}" 2)
    ("\\\\postscriptbox{[^}]*}{[^}]*}{\\(\\([^,} ]*/\\)?[^}. ]+\\)\\(\\.e?ps\\)?}" 1)
    ("\\\\\\(epsfbox\\|includegraphics\\|epsfig\\)\\*?{\\(\\([^,} ]*/\\)?[^}. ]+\\)\\(\\.e?ps\\)?}" 2) ;\epsfbox{hoge.ps} or \includegraphics{hoge.eps}
    ("\\\\\\(psbox\\)\\(\\[[^]]+\\]\\)?{\\(\\([^,} ]*/\\)?[^} ]+\\)\\(\\.e?ps\\)}" 3) ;\psbox[options...]{hoge.eps} (97/1/11)
    ("\\\\input{\\([^} ]+\\)\\(\\.tps\\)}" 1) ;tgif2tex (1998/9/16)
    )
  "See the documentation of YaTeX-processed-file-regexp-alist.")

(defvar YaTeX-file-processor-alist nil
  "*Alist of files' processor vs. its extension;
See also the documentation of YaTeX-processed-file-regexp-alist.")
  
(defvar YaTeX-file-processor-alist-default
  '(("tgif" . ".obj")
    ("ghostview" . ".ps")
    ("ghostview" . ".eps")
    (t . ".tex")
    (t . ".sty")
    (t . ""))
  "See the documentation of YaTeX-file-processor-alist.")

(defun YaTeX-goto-corresponding-file-processor (&optional other)
  "Execute corresponding file processor."
  (save-excursion
    (or (looking-at YaTeX-ec-regexp)
	(skip-chars-backward (concat "^" YaTeX-ec) (point-beginning-of-line)))
    (let ((list (append YaTeX-processed-file-regexp-alist
			YaTeX-processed-file-regexp-alist-default))
	  (p (point)) flist file
	  (peol (point-end-of-line))
	  (basedir
	   (if YaTeX-search-file-from-top-directory
	       (save-excursion (YaTeX-visit-main t) default-directory)
	     ".")))
      (setq flist (catch 'found
		    (while list
		      (goto-char p)
		      (if (re-search-forward (car (car list)) peol t)
			  (progn
			    (setq file (YaTeX-match-string
					(car (cdr (car list)))))
			    (throw 'found (cdr (car list)))))
		      (setq list (cdr list)))))
      (if flist				;if pattern and file name found
	  (let*((plist (append YaTeX-file-processor-alist
			       YaTeX-file-processor-alist-default))
		(plist0 plist)
		ext cmd src buf (alt (car (cdr flist))))
	    (if (and (re-search-forward
		      (concat YaTeX-comment-prefix "\\s *\\(.*\\)$") peol t)
		     (assoc (setq cmd (YaTeX-match-string 1))
			    YaTeX-file-processor-alist))
		(setq src		;if processor is specified
		      (concat file
			      (cdr (assoc cmd YaTeX-file-processor-alist))))
	      (while plist		;if processor is not specified
		(setq ext (cdr (car plist)))
		(if (and (string< "" (concat file ext))
			 (file-exists-p
			  (expand-file-name (concat file ext) basedir)))
		    (setq cmd (car (car plist))
			  src (concat file ext) plist nil))
		(setq plist (cdr plist)))
	      (if (and (null src) alt YaTeX-create-file-prefix-g)
		  (setq cmd alt
			src (concat file (cdr (assoc alt plist0))))))
	    (if src			;if processor and src file found
		(let ((default-directory basedir))
		  (cond
		   ((stringp cmd)
		    (let ((buf (concat "* " cmd " " src " *")))
		      (YaTeX-system (concat cmd " " src) buf)
		      t))
		   ((eq t cmd)
		    (let ((parent buffer-file-name))
		      (funcall
		       (cond
			(other 'YaTeX-switch-to-buffer-other-window)
			((get-file-buffer src) 'goto-buffer-window)
			(t 'YaTeX-switch-to-buffer))
		       src)
		      (or (YaTeX-get-builtin "!")
			  YaTeX-parent-file
			  (setq YaTeX-parent-file parent))
		      t))
		   ((symbolp cmd)
		    (cond
		     ((symbol-function cmd)
		      (funcall cmd src other)))
		    t)))))))))

(defun YaTeX-on-section-command-p (command)
  "Check if point is on the LaTeX command: COMMAND(regexp).
Return nil if point is not on it.  Otherwise return the
number of argument position.
Section command name is stored in match-data #1.
Parsing information is stored to plist.
Macros name stored to propname 'command.
Macro's argument number stored to propname 'argc."
  (let ((p (point)) md (parg 0) (argc 1) word (grouping 0) (i 0)
	(ec+command (concat YaTeX-ec-regexp "\\(" command "\\)")))
    (setplist 'YaTeX-on-section-command-p nil)
    (while (setq i (string-match "\\\\(" command i))
      (setq grouping (1+ grouping) i (+ i 2)))
    (save-excursion
      (if (looking-at ec+command) nil
	(catch 'found			;caught value has no meaning
	  ;;(1) looking at current position
	  (and (looking-at command)
	       (save-excursion
		 (while (and (not (bobp)) (looking-at command))
		   (forward-char -1))
		 (looking-at ec+command))
	       (goto-char (match-beginning 0))
	       (throw 'found t))
	  ;;(2) search command directly
	  (skip-chars-forward "^{}[]")
	  (and (YaTeX-re-search-active-backward
		ec+command
		YaTeX-comment-prefix nil t)
	       (>= p (match-beginning 0))
	       (throw 'found (goto-char (match-beginning 0))))
	  ;;(3) search token
	  (goto-char p)
	  (while t
	    (if (bobp) (throw 'found nil))
	    (cond
	     ((looking-at YaTeX-ec-regexp) (throw 'found t))
	     ((looking-at "[[{]") nil)
	     ((looking-at "[]}]")(condition-case nil (up-list -1) (error nil)))
	     (t (skip-chars-backward " \t\r\n")))
	    (skip-chars-backward (concat "^ \t\r\n{}[]" YaTeX-ec-regexp))
	    (or (bobp) (forward-char -1)))))
      (if (and
	   (looking-at (concat ec+command
			       "\\(\\(\\[[^]]+\\]\\|([0-9,]+)\\)*\\)"	;optional arg
			       ;"[ \t\n\r]*{[^}]+}")) ;arg braces
			       "[ \t\n\r]*{[^}]*}")) ;arg braces
	   (not (YaTeX-lookup-table
		 (setq word (YaTeX-match-string 1)) 'singlecmd)))
	  (progn
	    (setq md (match-data))
	    (skip-chars-forward "^{")
	    (if (<= (point) p) (setq parg (1+ parg)))
	    (setq argc
		  (or (car (cdr (YaTeX-lookup-table word 'section)))
		      argc))
	    (put 'YaTeX-on-section-command-p 'argc argc)
	    (put 'YaTeX-on-section-command-p 'command argc)
	    (while (and (>= (setq argc (1- argc)) 0)
			(progn (skip-chars-forward " \t\n\r")
			       (looking-at "{")))
	      (forward-list 1)
	      (if (<= (point) p) (setq parg (1+ parg))))
	    (store-match-data md)
	    (setq i (+ 2 grouping))
	    (if (and (match-beginning i)
		     (>= p (match-beginning i)) (< p (match-end i)))
		-1			;return -1 if point is on optional arg
	      (if (< p (point)) parg))
	    )))))

(defun YaTeX-on-maketitle-p ()
  "Check if point is on maketitle type commands.
Call this function after YaTeX-on-section-command-p."
  (let ((p (point)))
    (save-excursion
      (or (= (char-after (point)) ?\\ )
	  (progn
	    (skip-chars-backward
	     (concat "^" YaTeX-ec-regexp) (point-beginning-of-line))
	    (or (bobp) (bolp) (backward-char 1))))
      (and (looking-at (concat YaTeX-ec-regexp YaTeX-TeX-token-regexp))
	   (<= (match-beginning 0) p)
	   (> (match-end 0) p)))))

(defun YaTeX-on-begin-end-p ()
  (save-excursion
    (if (and (boundp 'in-leftright-p) in-leftright-p)
	;; Dirty workaround for YaTeX-goto-corresponding-leftright 2003/3/28
	(let ((md (match-data)))	; for safety
	  (if (looking-at YaTeX-ec-regexp)
	      nil			; stay here
	    (cond
	     ((looking-at "\\w")		(skip-chars-backward "A-Za-z"))
	     ((looking-at "\\.()\\[\\]|")	(forward-char -1)))
	    (if (equal (char-after (1- (point)))
		       (string-to-char YaTeX-ec))
		(forward-char -1))))
      ;(beginning-of-line)
      (if (equal (char-after (point)) ?\\) nil	;stay here
	(skip-chars-backward "^\n\\\\")
	(or (bolp) (forward-char -1))))
    (re-search-forward
     ;;"\\\\begin{\\([^}]+\\)}\\|\\\\end{\\([^}]+\\)}"
     (concat
      (YaTeX-replace-format-args
       (regexp-quote YaTeX-struct-begin)
       (concat "\\(" YaTeX-struct-name-regexp "\\)") "" "" "")
      "\\|"
      (YaTeX-replace-format-args
       (regexp-quote YaTeX-struct-end)
       (concat "\\(" YaTeX-struct-name-regexp "\\)") "" "" "")
      "\\|\\("
      YaTeX-ec-regexp  ;;"[][()]\\)"
      "[][]\\)"
      )
     (point-end-of-line) t)))

(defun YaTeX-on-includes-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\(\\(include[^}]*\\)\\|\\(input\\)\\){[^}]*}"
		       (point-end-of-line) t)))

(defun YaTeX-on-comment-p (&optional sw)
  "Return t if current line is commented out.
Optional argument SW t to treat all `%' lines as comment,
even if on `%#' notation."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\\s ")
    (looking-at (if sw "%" "%[^#]"))))

(defun YaTeX-on-BEGIN-END-p ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (re-search-forward
       "\\(%#BEGIN\\)\\|\\(%#END\\)" (point-end-of-line) t))))

(defun YaTeX-goto-corresponding-* (arg)
  "Parse current line and call suitable function."
  (interactive "P")
  (let (mm)
    (cond
     ((YaTeX-goto-corresponding-label arg))
     ((YaTeX-goto-corresponding-environment))
     ((YaTeX-goto-corresponding-file-processor arg))
     ((YaTeX-goto-corresponding-file arg))
     ((YaTeX-goto-corresponding-BEGIN-END))
     ((and (setq mm (YaTeX-in-math-mode-p))
	   (YaTeX-goto-corresponding-leftright)))
     ((and ;;mm YaTeX-use-AMS-LaTeX
	   (YaTeX-goto-corresponding-paren)))
     ;;((and (string-match
     ;;	  YaTeX-equation-env-regexp	;to delay loading
     ;;	  (or (YaTeX-inner-environment t) "document"))
     ;;	 (YaTeX-goto-corresponding-leftright)))
     (t (message "I don't know where to go.")))))

(defun YaTeX-goto-corresponding-*-other-window (arg)
  "Parse current line and call suitable function."
  (interactive "P")
  (cond
   ((YaTeX-goto-corresponding-label arg t))
   ;;((YaTeX-goto-corresponding-environment))
   ((YaTeX-goto-corresponding-file t))
   ;;((YaTeX-goto-corresponding-BEGIN-END))
   (t (message "I don't know where to go."))))

(defun YaTeX-comment-region (alt-prefix)
  "Comment out region by '%'.
If you call this function on the 'begin{}' or 'end{}' line,
it comments out whole environment"
  (interactive "P")
  (if (not (YaTeX-on-begin-end-p))
      (comment-out-region
       (if alt-prefix
	   (read-string "Insert prefix: ")
	 YaTeX-comment-prefix))
    (YaTeX-comment-uncomment-env 'comment-out-region)))

(defun YaTeX-uncomment-region (alt-prefix)
  "Uncomment out region by '%'."
  (interactive "P")
  (if (not (YaTeX-on-begin-end-p))
      (uncomment-out-region
       (if alt-prefix (read-string "Remove prefix: ")
	 YaTeX-comment-prefix)
       (region-beginning) (region-end) YaTeX-uncomment-once)
    (YaTeX-comment-uncomment-env 'uncomment-out-region)))

(defun YaTeX-comment-uncomment-env (func)
  "Comment or uncomment out one LaTeX environment switching function by FUNC."
  (let (beg (p (point)))
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (YaTeX-goto-corresponding-environment)
      (beginning-of-line)
      (if (> p (point)) (setq beg (1+ beg)) (forward-char 1))
      (funcall func YaTeX-comment-prefix beg (point) YaTeX-uncomment-once)))
  (message "%sommented out current environment."
	   (if (eq func 'comment-out-region) "C" "Un-c")))

(defun YaTeX-comment-paragraph ()
  "Comment out current paragraph."
  (interactive)
  (save-excursion
    (cond
     ((YaTeX-on-begin-end-p)
      (beginning-of-line)
      (insert YaTeX-comment-prefix)
      (YaTeX-goto-corresponding-environment)
      (beginning-of-line)
      (insert YaTeX-comment-prefix))
     ((YaTeX-on-comment-p)
      (message "Already commented out."))
     (t
      (mark-paragraph)
      (if (looking-at paragraph-separate) (forward-line 1))
      (comment-out-region "%")))))

(defun YaTeX-uncomment-paragraph ()
  "Uncomment current paragraph."
  (interactive)
  (save-excursion
    (if (YaTeX-on-begin-end-p)
	(let ((p (point-marker)))
	  (YaTeX-goto-corresponding-environment)
	  (YaTeX-remove-prefix YaTeX-comment-prefix YaTeX-uncomment-once)
	  (goto-char p)
	  (YaTeX-remove-prefix YaTeX-comment-prefix YaTeX-uncomment-once)
	  (set-marker p nil))
      (if (YaTeX-on-comment-p)
	  (let*((fill-prefix "")
		;;append `^%' to head of paragraph delimiter.
		(paragraph-start
		 (concat
		  "^$\\|^%\\(" YaTeX-paragraph-separate "\\)"))
		(paragraph-separate paragraph-start))
	    (mark-paragraph)
	    (if (not (bobp)) (forward-line 1))
	    (uncomment-out-region "%" nil nil YaTeX-uncomment-once))
	(message "This line is not a comment line.")))))

(defun YaTeX-remove-prefix (prefix &optional once)
  "Remove prefix on current line as far as prefix detected. But
optional argument ONCE makes deletion once."
  (interactive "sPrefix:")
  (beginning-of-line)
  (while (re-search-forward (concat "^" prefix) (point-end-of-line) t)
    (replace-match "")
    (if once (end-of-line))))

(defun YaTeX-kill-some-pairs (predicate gofunc kill-contents)
  "Kill some matching pair.
This function assumes that pairs occupy whole of each line where they resid."
  (if (not (funcall predicate)) nil
    (let ((b1 (match-beginning 0)) (e1 (match-end 0))
	  b2 e2)
      (save-excursion
	(funcall gofunc)
	(funcall predicate)		;get match data
	(if (< (point) e1)		;if currently on begin-line
	    (progn
	      (setq b2 b1 e2 e1
		    b1 (match-beginning 0) e1 (match-end 0))
	      (goto-char e2))		;goto end-line's end
	  (setq b2 (match-beginning 0)
		e2 (match-end 0))
	  (goto-char e2))	;now e2 has surely end-line's end
	(skip-chars-forward " \t")
	(and (eolp)
	     (not (eobp))
	     (setq e2 (1+ (point))))
	(if (not kill-contents)
	    (kill-region
	     (progn
	       (goto-char b2)
	       (skip-chars-backward " \t%")
	       (if (bolp) (point) b2))
	     e2))
	(goto-char b1)
	(skip-chars-backward " \t%")
	(if (not kill-contents)
	    (progn
	      (kill-append
	       (buffer-substring
		(setq b1 (if (bolp) (point) b1))
		(setq e1
		      (progn
			(goto-char e1)
			(while (looking-at "{\\| \t") 
			  (forward-list 1))
			(skip-chars-forward " \t")
			(if (and (eolp) (not (eobp)))
			    (1+ (point))
			  (point)))))
	       t)
	      (delete-region b1 e1))
	  (kill-region
	   (if (bolp) (point) b1)
	   e2)))
      t)))

(defun YaTeX-kill-section-command (point kill-all)
  "Kill section-type command at POINT leaving its last argument.
Non-nil for the second argument kill its last argument too."
  (let ((cmd (get 'YaTeX-on-section-command-p 'command))
	(argc (get 'YaTeX-on-section-command-p 'argc))
	beg (end (make-marker)))
    (save-excursion
      (goto-char point)
      (or (looking-at YaTeX-ec-regexp)
	  (progn
	    (skip-chars-backward (concat "^" YaTeX-ec-regexp))
	    (forward-char -1)))
      (setq beg (point))
      (skip-chars-forward "^{")
      (while (> (setq argc (1- argc)) 0)
	(skip-chars-forward "^{")
	(forward-list 1))
      (kill-region beg (point))
      (forward-list 1)
      (set-marker end (point))
      (if kill-all
	  (progn
	    (kill-append (buffer-substring beg end) nil)
	    (delete-region beg end))
	(goto-char beg)
	(kill-append
	 (buffer-substring
	  (point) (progn (skip-chars-forward "^{" end) (1+ (point))))
	 nil)
	(delete-region beg (1+ (point)))
	(goto-char end)
	(set-marker end nil)
	(kill-append (buffer-substring (point) (1- (point))) nil)
	(delete-backward-char 1)))))

(defun YaTeX-kill-paren (kill-contents)
  "Kill parentheses leaving its contents.
But kill its contents if the argument KILL-CONTENTS is non-nil."
  (interactive "P")
  (let (p bsl (backslash-syntax (char-to-string (char-syntax ?\\)))
	  (md (match-data)))
    (unwind-protect
	(save-excursion
	  (modify-syntax-entry ?\\ " ")
	  (if (looking-at "\\s(\\|\\(\\s)\\)")
	      (progn
		(if (match-beginning 1)
		    (up-list -1))
		(if (and (> (point) (point-min))
			 (= (char-after (1- (point))) ?\\ ))
		    (setq p (1- (point)) bsl t)
		  (setq p (point)))
		(forward-list 1)
					;(YaTeX-goto-open-paren t)
		(if kill-contents (delete-region p (point))
		  (backward-delete-char 1)
		  (cond
		   ((save-excursion
		      (forward-char -2)
		      (looking-at (concat YaTeX-ec-regexp "/")))
		    (backward-delete-char 2))
		   ((= (char-after (1- (point))) ?\\)
		    (backward-delete-char 1)))
		  (goto-char p)
		  (if (looking-at
		       (concat "{" YaTeX-ec-regexp
			       YaTeX-command-token-regexp "+"
			       "\\s +"))
		      (delete-region (point) (match-end 0))
		    (delete-char 1)
		    (if bsl (delete-char 1))))
		t)))
      (modify-syntax-entry ?\\ backslash-syntax)
      (store-match-data md))))

(defvar YaTeX-read-environment-history nil "Holds history of environments.")
(put 'YaTeX-read-environment-history 'no-default t)
(defun YaTeX-read-environment (prompt &optional predicate must-match initial)
  "Read a LaTeX environment name with completion."
  (YaTeX-sync-local-table 'tmp-env-table)
  (completing-read-with-history
   prompt
   (append tmp-env-table user-env-table env-table)
   predicate must-match initial
   'YaTeX-read-environment-history))

(defvar YaTeX-read-section-history nil "Holds history of section-types.")
(put 'YaTeX-read-section-history 'no-default t)
(defun YaTeX-read-section (prompt &optional predicate initial)
  "Read a LaTeX section-type command with completion."
  (YaTeX-sync-local-table 'tmp-section-table)
  (let ((minibuffer-completion-table
	 (append tmp-section-table user-section-table section-table)))
    (read-from-minibuffer-with-history
     prompt initial YaTeX-section-completion-map nil
     'YaTeX-read-section-history)))

(defun YaTeX-read-section-with-overview ()
  "Read sectioning command with overview.
This function refers a local variable `source-window' in YaTeX-make-section"
  (interactive)
  (require 'yatexsec)			;some case needs this
  (if (> (minibuffer-depth) 1)
      (error "Too many minibuffer levels for overview."))
  (let ((sw (selected-window))
	(minibuffer-max-depth nil) ; for XEmacs20
	(enable-recursive-minibuffers t) sect)
    (unwind-protect
	(progn
	  (select-window source-window)
	  (setq sect (YaTeX-read-section-in-minibuffer
		      "Sectioning(Up=C-p, Down=C-n, Help=?): "
		      YaTeX-sectioning-level (YaTeX-section-overview))))
      (select-window sw))
    (YaTeX-minibuffer-erase)
    (insert sect)
    (exit-minibuffer)))

(defvar YaTeX-read-fontsize-history nil "Holds history of font designator.")
(put 'YaTeX-read-fontsize-history 'no-default t)
(defun YaTeX-read-fontsize (prompt &optional predicate must-match initial)
  "Read a LaTeX font changing command with completion."
  (YaTeX-sync-local-table 'tmp-fontsize-table)
  (completing-read-with-history
   prompt (append tmp-fontsize-table user-fontsize-table fontsize-table)
   predicate must-match initial 'YaTeX-read-fontsize-history))

(defun YaTeX-change-environment ()
  "Change the name of environment."
  (interactive)
  (if (not (YaTeX-on-begin-end-p)) nil
    (save-excursion
      (let (p env (m1 (match-beginning 1)) (m2 (match-beginning 2)))
	(setq env (if m1 (buffer-substring m1 (match-end 1))
		    (buffer-substring m2 (match-end 2))))
	(goto-char (match-beginning 0))
	(set-mark-command nil)
	(YaTeX-goto-corresponding-environment)
	(setq newenv (YaTeX-read-environment
		      (format "Change environment `%s' to: " env)))
	(cond
	 ((string= newenv "")	(message "Change environment cancelled."))
	 ((string= newenv env)	(message "No need to change."))
	 (t
	  (search-forward (concat "{" env) (point-end-of-line) t)
	  (replace-match (concat "{" newenv) t)
	  (exchange-point-and-mark)
	  (search-forward (concat "{" env) (point-end-of-line) t)
	  (replace-match (concat "{" newenv) t)))
	t))))

(defun YaTeX-change-section ()
  "Change section-type command."
  (interactive)
  (let*((where (YaTeX-on-section-command-p YaTeX-command-token-regexp))
	(p (point)) (cmd (YaTeX-match-string 1))
	(beg (make-marker)) (end (make-marker)) old new)
    (if (null where) nil
      (unwind-protect
	  (progn
	    (cond
	     ((equal where 0);;if point is on section command
	      (set-marker beg (match-beginning 1))
	      (set-marker end (match-end 1))
	      (goto-char beg)		;beginning of the command
	      (setq new (YaTeX-read-section
			 (format "Change `%s' to: " cmd) nil)))

	     ((= where -1);;if point is on a optional parameter
	      (set-marker beg (match-beginning 2))
	      (skip-chars-forward "^{")
	      (set-marker end (point))
	      (goto-char p)
	      (setq new
		    (if (fboundp (intern-soft (concat YaTeX-addin-prefix cmd)))
			(YaTeX-addin cmd)
		      (concat "["
			      (read-string (format "Change `%s' to: "
						   (buffer-substring
						    (1+ beg) (1- end))))
			      "]"))))

	     ((> where 0);;if point is in arguments' braces
	      (or (looking-at "{")
		  (progn (skip-chars-backward "^{") (forward-char -1)))
	      (set-marker beg (1+ (point)))
	      (forward-list 1)
	      (forward-char -1)
	      (set-marker end (point))
	      (setq old (buffer-substring beg end))
	      (goto-char p)
	      (if (> (length old) 40)
		  (setq old (concat (substring old 0 12) "..."
				    (substring old -12))))
	      (setq new
		    (if (intern-soft (concat "YaTeX::" cmd))
			(funcall (intern-soft (concat "YaTeX::" cmd)) where)
		      (read-string (format "Change `%s' to: " old)))))
	     )				;cond
	    (delete-region beg end)
	    (goto-char beg)
	    (insert-before-markers new))
	(set-marker beg nil)
	(set-marker end nil))
      ;;(goto-char (marker-position p))
      new)))

(defun YaTeX-change-fontsize ()
  "Change large-type command."
  (let ((lt (append tmp-fontsize-table user-fontsize-table fontsize-table))
	(p (point)) large old new beg end)
    ;;(and (looking-at "}") (up-list -1))
    ;;(and (looking-at "{") (forward-char 1))
    ;;Is above convenient?
    (save-excursion
      (or (looking-at YaTeX-ec-regexp)
	  (progn
	    (skip-chars-backward (concat "^" YaTeX-ec-regexp))
	    (forward-char -1)))
      (cond
       ((and
	 (looking-at
	  (concat YaTeX-ec-regexp "\\(" YaTeX-TeX-token-regexp "\\)"))
	 (< p (match-end 0))
	 (assoc (setq old (YaTeX-match-string 1)) lt))
	(goto-char p)
	(setq beg (match-beginning 1) end (match-end 1) ;save match position
	      new (completing-read
		   (format "Change font/size `%s' to : " old) lt))
	(delete-region beg end)
	(goto-char beg)
	(insert-before-markers new)
	new)
       (t nil)
       ))))

(defun YaTeX-change-math-image ()
  "Change with image completion."
  (let (maketitle memberp beg end)
    (if (and (YaTeX-on-maketitle-p)
	     (progn
	       (setq maketitle (substring (YaTeX-match-string 0) 1))
	       (setq memberp (YaTeX-math-member-p maketitle))))
	(let ((last-command-char (string-to-char (car memberp))))
	  (setq beg (match-beginning 0) end (match-end 0))
	  (delete-region beg end)
	  (YaTeX-math-insert-sequence t (cdr memberp))))))

(defun YaTeX-kill-* (&optional arg)
  "Parse current line and call suitable function.
Non-nil for ARG kills its contents too."
  (interactive "P")
  (cond
   ((YaTeX-kill-some-pairs 'YaTeX-on-begin-end-p
			   'YaTeX-goto-corresponding-environment arg))
   ((YaTeX-kill-some-pairs 'YaTeX-on-BEGIN-END-p
			   'YaTeX-goto-corresponding-BEGIN-END arg))
   ((YaTeX-on-section-command-p YaTeX-command-token-regexp);on any command
    (YaTeX-kill-section-command (match-beginning 0) arg))
   ((YaTeX-kill-paren arg))
   (t (message "I don't know what to kill."))))

(defun YaTeX-change-* ()
  "Parse current line and call suitable function."
  (interactive)
  (cond
   ((YaTeX-change-parentheses))
   ((YaTeX-change-environment))
   ((YaTeX-change-section))
   ((YaTeX-change-fontsize))
   ((YaTeX-change-math-image))
   (t (message "I don't know what to change."))))

;;;
;Check availability of add-in functions
;;;
(cond
 ((featurep 'yatexadd) nil)		;Already provided.
 ((progn (load "yatexadd" t) (featurep 'yatexadd)) nil)
 (t (message "YaTeX add-in functions not supplied.")))

(defun YaTeX-addin (name)
  "Check availability of addin function and call it if exists."
  (if (and (not (get 'YaTeX-generate 'disabled))
	   (intern-soft (concat YaTeX-addin-prefix name))
	   (fboundp (intern-soft (concat YaTeX-addin-prefix name))))
      (let ((s (funcall (intern (concat YaTeX-addin-prefix name)))))
	(if (stringp s) s ""))
    "")) ;Add in function is not bound.


(defun YaTeX-on-item-p (&optional point)
  "Return t if POINT (default is (point)) is on \\item."
  (let ((p (or point (point))))
    (save-excursion
      (goto-char p)
      (end-of-line)
      (setq p (point))
      (re-search-backward YaTeX-paragraph-delimiter nil t)
      (re-search-forward YaTeX-item-regexp p t))))

(defun YaTeX-in-verb-p (&optional point)
  "Check if POINT is in verb or verb*.  Default of POINT is (point)."
  (setq point (or point (point)))
  (save-excursion
    (let ((md (match-data)))
      (goto-char point)
      (unwind-protect
	  (if (not (re-search-backward
		    (concat YaTeX-ec-regexp
			    "\\(" YaTeX-verb-regexp "\\)"
			    "\\([^-A-Za-z_*]\\)")
		    (point-beginning-of-line) t))
	      nil
	    (goto-char (match-end 2))
	    (skip-chars-forward
	     (concat "^" (buffer-substring (match-beginning 2) (match-end 2))))
	    (and (< (match-beginning 2) point) (< (1- point) (point))))
	(store-match-data md)))))

(defun YaTeX-literal-p (&optional point)
  "Check if POINT is in verb or verb* or verbatime environment family.
Default of POINT is (point)."
  (let ((md (match-data)))
    (unwind-protect
	(cond
	 ((equal YaTeX-ec "\\")		;maybe LaTeX
	  (save-excursion
	    (and point (goto-char point))
	    (or (YaTeX-in-verb-p (point))
		(and (not (looking-at "\\\\end{verb"))
		     (YaTeX-quick-in-environment-p
		      YaTeX-verbatim-environments))))))
      (store-match-data md))))

(defun YaTeX-in-environment-p (env)
  "Return if current LaTeX environment is ENV.
ENV is given in the form of environment's name or its list."
  (let ((md (match-data)) (nest 0) p envrx)
    (cond
     ((atom env)
      (setq envrx
	    (concat "\\("
		    (regexp-quote
		     (YaTeX-replace-format-args
		      YaTeX-struct-begin env "" ""))
		    "\\)\\|\\("
		    (regexp-quote
		     (YaTeX-replace-format-args
		      YaTeX-struct-end env "" ""))
		    "\\)"))
      (save-excursion
	(setq p (catch 'open
		  (while (YaTeX-re-search-active-backward
			  envrx YaTeX-comment-prefix nil t)
		    (if (match-beginning 2)
			(setq nest (1+ nest))
		      (setq nest (1- nest)))
		    (if (< nest 0) (throw 'open t)))))))
     ((listp env)
      (setq p
	    (or (YaTeX-in-environment-p (car env))
		(and (cdr env) (YaTeX-in-environment-p (cdr env)))))))
    (store-match-data md)
    p;(or p (YaTeX-in-verb-p (match-beginning 0)))
    ))

(defun YaTeX-quick-in-environment-p (env)
  "Check quickly but unsure if current environment is ENV.
ENV is given in the form of environment's name or its list.
This function returns correct result only if ENV is NOT nested."
  (save-excursion
    (let ((md (match-data)) (p (point)) rc clfound)
      (cond
       ((listp env)
	(or (YaTeX-quick-in-environment-p (car env))
	    (and (cdr env) (YaTeX-quick-in-environment-p (cdr env)))))
       (t
	(if (YaTeX-search-active-backward
	     (YaTeX-replace-format-args YaTeX-struct-begin env "" "")
	     YaTeX-comment-prefix nil t)
	    (setq rc (not (YaTeX-search-active-forward
			   (YaTeX-replace-format-args
			    YaTeX-struct-end env)
			   YaTeX-comment-prefix p t nil))))
	(store-match-data md)
	rc)))))

;; Filling \item
(defun YaTeX-remove-trailing-comment (start end)
  "Remove trailing comment from START to end."
  (save-excursion
    (let ((trcom (concat YaTeX-comment-prefix "$")))
      (goto-char start)
      (while (re-search-forward trcom end t)
	(if (/= (char-after (1- (match-beginning 0))) ?\\ )
	    (replace-match "\\1"))))))

(defvar YaTeX-itemize-withlabel-max-indent-depth 8)
(defun YaTeX-get-item-info (&optional recent thisenv)
  "Return the list of the beginning of \\item and column of its item.
If it seems to be outside of itemizing environment, just return nil.
Non-nil for optional argument RECENT refers recent \\item.
Optional second argument THISENV omits calling YaTeX-inner-environment."
  (save-excursion
    (let* ((p (point)) env e0 c cc md
	   (bndry (and (setq env (or thisenv (YaTeX-inner-environment t)))
		       (get 'YaTeX-inner-environment 'point))))
      (end-of-line)
      (if (if recent
	      (catch 'found
		(while (YaTeX-re-search-active-backward
			YaTeX-item-regexp YaTeX-comment-prefix bndry t)
		  (setq md (match-data))
		  (YaTeX-inner-environment t)
		  (store-match-data md)
		  (if (= bndry (get 'YaTeX-inner-environment 'point))
		      (throw 'found t))))
	    (goto-char bndry)
	    (YaTeX-re-search-active-forward
	     YaTeX-item-regexp YaTeX-comment-prefix p t))
	  (progn
	    (goto-char (match-end 0))
	    ;(setq c (current-column))
	    (if (string-match "desc" env)
		(setq c 6)
	      (setq cc (current-column))
	      (if (equal (following-char) ?\[) (forward-list 1))
	      (if (< (- (current-column) cc)
		     YaTeX-itemize-withlabel-max-indent-depth)
		  (setq c 0)
		(move-to-column cc)
		(setq c YaTeX-itemize-withlabel-max-indent-depth)))
	    (skip-chars-forward " \t" (point-end-of-line))
	    (list (point-beginning-of-line) (+ c (current-column))))))))

(defun YaTeX-fill-item ()
  "Fill item in itemize environment."
  (interactive)
  (save-excursion
    (let* ((p (point))
	   (item-term (concat
		       "\\(^[ \t]*$\\)\\|" YaTeX-item-regexp "\\|\\("
		       YaTeX-ec-regexp "\\(begin\\|end\\)\\)"))
	   ;;This value depends on LaTeX.
	   fill-prefix start col
	   (info (YaTeX-get-item-info t)))
      (if (null info) nil		;not on \item, do nothing
	(setq start (car info)
	      col (car (cdr info)))
	(save-excursion
	  (if (re-search-backward "^\\s *$" start t)
	      ;;if separated from \item line, isolate this block
	      (progn
		(setq start (1+ (match-end 0)))
		(goto-char start)
		(skip-chars-forward " \t")
		(delete-region (point) start) ;is this your favor???
		(indent-to col))))
	(beginning-of-line)
	(if (<= (save-excursion
		 (re-search-forward
		  (concat "\\\\end{\\|\\\\begin{\\|^[ \t]*$") nil t)
		 (match-beginning 0))
	       p)
	    (progn  (message "Not on itemize.") nil)
	  (end-of-line)
	  (newline)
	  (indent-to col)
	  (setq fill-prefix
		(buffer-substring (point-beginning-of-line)(point)))
	  (beginning-of-line)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (re-search-forward item-term nil 1)
	  (YaTeX-remove-trailing-comment start (point))
	  (beginning-of-line)
	  (push-mark (point) t)
	  (goto-char start)
	  (forward-line 1)
	  (while (< (point) (mark))
	    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	    (forward-line 1))
	  (fill-region-as-paragraph start (mark))
	  (if NTT-jTeX
	      (while (progn(forward-line -1)(end-of-line) (> (point) start))
		(insert ?%)))
	  (pop-mark))))))

(defun YaTeX-fill-paragraph (arg)
  "YaTeX adjustment function for fill-paragraph.
*Protect \\verb from unexpected broken up."
  (interactive "P")
  (cond
   ((not (eq major-mode 'yatex-mode)) (fill-paragraph arg))
   ((YaTeX-quick-in-environment-p YaTeX-fill-inhibit-environments) nil)
   ((YaTeX-in-math-mode-p) nil)
   (t
    (save-excursion
      (let*((verbrex (concat YaTeX-ec-regexp
			     "\\(" YaTeX-verb-regexp "\\)" ;match#1
			     "\\(.\\).*\\(\\2\\)")) ;match #2 and #3
	    (tilderex (concat "\\("
			      YaTeX-kanji-regexp "~"
			      "\\)" YaTeX-ec-regexp
			      "\\|\\("
			      "~" YaTeX-kanji-regexp
			      "\\)"))
	    (p (point)) ii end poslist spacelist lenlist b e n
	    (fill-prefix fill-prefix)
	    (inenv (or (YaTeX-inner-environment t) "document"))
	    (border (get 'YaTeX-inner-environment 'point)))
	(cond
	 ((save-excursion (beginning-of-line) ;if point is on the first
			  (setq end (point))  ;non-whitespace char
			  (skip-chars-forward " \t")
			  (equal (point) p))
	  (setq fill-prefix (buffer-substring p end)))
	 ((and ;;(not YaTeX-emacs-19)
	       (string-match YaTeX-itemizing-env-regexp inenv)
	       (setq ii (YaTeX-get-item-info)))
	  (save-excursion
	    (beginning-of-line)
	    (indent-to-column (car (cdr ii)))
	    (setq fill-prefix
		  (buffer-substring (point) (point-beginning-of-line)))
	    (delete-region (point) (progn (beginning-of-line) (point))))))
	(cond
	 ((string-match "tabular" inenv)
	  (let ((b (point-beginning-of-line))
		(e (point-end-of-line)))
	    (if (re-search-backward
		 "&\\|\\\\\\\\\\|\\\\\\(begin\\|end\\){" border t)
		(setq b (if (match-beginning 1)
			    (progn (forward-line 1) (point))
			  (point-beginning-of-line))))
	    (goto-char p)
	    (if (re-search-forward
		 "&\\|\\\\\\\\\\|\\\\\\(end\\|begin\\){" nil t)
		(setq e (if (match-beginning 1)
			    (progn (forward-line -1)
				   (point-end-of-line))
			  (match-beginning 0))))
	    (set-mark e)
	    (goto-char b)))
	 (t
	  (mark-paragraph)))
	(save-restriction
	  (narrow-to-region (region-beginning) (region-end))
	  (YaTeX-remove-trailing-comment (point-min) (point-max))
	  ;; First, replace spaces in verb to _ temporarily.
	  (goto-char (point-min))
	  (while (YaTeX-re-search-active-forward
		  verbrex YaTeX-comment-prefix (point-max) t)
	    (setq end (match-beginning 3))
	    (goto-char (match-beginning 2))
	    (while (re-search-forward "\\s " end t)
	      (setq poslist (cons (make-marker) poslist)
		    spacelist (cons (preceding-char) spacelist)
		    lenlist (cons 1 lenlist))
	      (replace-match "_")
	      (set-marker (car poslist) (match-beginning 0))))
	  ;; Second, replace "表~\ref{...}" to "\\\ref{...}"
	  (goto-char (point-min))
	  (while (YaTeX-re-search-active-forward
		  tilderex YaTeX-comment-prefix (point-max) t)
	    (if (match-beginning 1)
		(setq b (match-beginning 1) e (match-end 1) n 1)
	      (setq b (match-beginning 2) e (match-end 2) n 2))
	    (setq poslist (cons (make-marker) poslist)
		  spacelist (cons (YaTeX-match-string n) spacelist)
		  lenlist (cons 2 lenlist))
	    (goto-char (match-beginning 0))
	    (delete-region (point) e)
	    (insert YaTeX-ec YaTeX-ec)	;set-marker should be here
	    (set-marker (car poslist) b))
	  ;;(fill-paragraph arg)
	  (fill-region-as-paragraph (point-min) (point-max) arg)
	  (while spacelist
	    (goto-char (car poslist))
	    (set-marker (car poslist) nil)
	    (and (eolp) (skip-chars-forward "\n\t "))
	    (delete-char (car lenlist))
	    (insert (car spacelist))
	    (setq spacelist (cdr spacelist)
		  poslist (cdr poslist)
		  lenlist (cdr lenlist)))
	  (goto-char (point-min))
	  (forward-word 1)
	  (beginning-of-line)
	  (while (re-search-forward "\\\\\\([a-z]*ref\\|cite\\){" nil t)
	    (if (< (point-end-of-line)
		   (save-excursion (forward-char -1) (forward-list 1) (point)))
		(progn (end-of-line)
		       (if (save-excursion
			     (backward-word 1)
			     (looking-at "[^0-9A-z!-)]"))
			   (insert YaTeX-comment-prefix)))))
	  ;; Nonbreak space `~'
	  (goto-char (point-min))
	  (while (YaTeX-re-search-active-forward
		  "~\\(\\s *\\)$" YaTeX-comment-prefix (point-max) t)
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert YaTeX-comment-prefix))
	  (goto-char (point-min))
	  (if (and NTT-jTeX (looking-at "[ \t]\\|^$"))
	      (progn
		(goto-char (point-min))
		(while (not (eobp))
		  (end-of-line)
		  (or (bolp)
		      (save-excursion
			(backward-word 1)
			(looking-at "[0-9A-z!-)]")) ;is not japanese string
		      (progn (setq p (point)) (insert YaTeX-comment-prefix)))
		  (forward-line 1))
		(goto-char p)
		(if (looking-at "%") (delete-char 1)) ;remove last inserted `%'
		))))))))

(if (fboundp 'YaTeX-saved-indent-new-comment-line) nil
  (fset 'YaTeX-saved-indent-new-comment-line
	(symbol-function 'indent-new-comment-line))
  (fset 'indent-new-comment-line 'YaTeX-indent-new-comment-line))

(defun YaTeX-indent-new-comment-line (&optional soft)
  "Tuned `indent-new-comment-line' function for yatex.
See the documentation of `YaTeX-saved-indent-new-comment-line'."
  (interactive)
  (cond
   ((or (not (memq major-mode '(yatex-mode yahtml-mode)))
	(string-match
	 "document"
	 (or (and (boundp 'inenv) inenv)
	     (or (YaTeX-inner-environment t) "document"))))
    (apply 'YaTeX-saved-indent-new-comment-line (if soft (list soft))))
;   ((and (eq major-mode 'yahtml-mode)
;	 (string-match
;	  "^[Pp][Rr][Ee]" (yahtml-inner-environment-but "^[Aa]\\b" t)))
;    (yahtml-indent-new-commnet-line))
   ((and (eq major-mode 'yatex-mode)	;1997/2/4
	 (YaTeX-in-math-mode-p)) nil)		;1996/12/30
   (t (let (fill-prefix)
	(apply 'YaTeX-saved-indent-new-comment-line (if soft (list soft)))))))

(defun YaTeX-fill-* ()
  "Fill paragraph according to its condition."
  (interactive)
  (cond
   ((YaTeX-fill-item))
   ))

;; Accent completion
(defun YaTeX-read-accent-char (x)
  "Read char in accent braces."
  (let ((c (read-char)))
    (concat
     (if (and (or (= c ?i) (= c ?j))
	      (not (string-match (regexp-quote x) "cdb")))
	 "\\" "")
     (char-to-string c))))

(defun YaTeX-make-accent ()
  "Make accent usage."
  (interactive)
  (message "1:` 2:' 3:^ 4:\" 5:~ 6:= 7:. u v H t c d b")
  (let ((c (read-char))(case-fold-search nil))
    (setq c (cond ((and (> c ?0) (< c ?8))
		   (substring "`'^\"~=." (1- (- c ?0)) (- c ?0)))
		  ((= c ?h) "H")
		  (t (char-to-string c))))
    (if (not (string-match c "`'^\"~=.uvHtcdb")) nil
      (insert "\\" c "{}")
      (backward-char 1)
      (insert (YaTeX-read-accent-char c))
      (if (string= c "t") (insert (YaTeX-read-accent-char c)))
      (forward-char 1))))

;; Indentation
(defun YaTeX-current-indentation ()
  "Return the indentation of current environment."
  (save-excursion
    ;;(beginning-of-line)
    (if (YaTeX-beginning-of-environment t)
	(goto-char (get 'YaTeX-inner-environment 'point))
      (forward-line -1)
      (beginning-of-line)
      (skip-chars-forward " \t"))
    (current-column)))

(defun YaTeX-previous-line-indentation ()
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (current-column)))

(defvar YaTeX-noindent-env-regexp "verbatim\\*?\\|alltt"
  "*Regexp of environment names that should begin with no indentation.
All verbatime-like environment name should match with.")
(defun YaTeX-indent-line ()
  "Indent corrent line referrin current environment."
  (interactive)
  (let ((indent-relative
	 (function
	  (lambda (&optional additional)
	    (YaTeX-reindent
	     (+ (YaTeX-current-indentation)
		(or additional 0)
		YaTeX-environment-indent)))))
	sect depth iteminfo (p (point)) pp (peol (point-end-of-line)) begend
	;;inenv below is sometimes defined in YaTeX-indent-new-comment-line
	(inenv (or (and (boundp 'inenv) inenv) (YaTeX-inner-environment t))))
    ;;(if NTT-jTeX		;;Do you need this section?
    ;;	(save-excursion
    ;;  (end-of-line)
    ;;  (let ((p (point)))
    ;;    (forward-line -1)
    ;;    (end-of-line)
    ;;    (or (= p (point))
    ;;	(progn (backward-char (length YaTeX-comment-prefix))
    ;;	       (not (looking-at (regexp-quote YaTeX-comment-prefix))))
    ;;	(progn
    ;;	  (skip-chars-backward YaTeX-comment-prefix)
    ;;	  (kill-line))))))
    (or inenv (setq inenv "document"))	;is the default environment
    (cond
     ((and
       (prog1 (YaTeX-on-begin-end-p)
	 (setq begend (match-beginning 0)))
       (or (match-beginning 2)		;if \end
	   (and (match-beginning 3)	;if \) \]
		(= (char-syntax (char-after (1+ (match-beginning 3)))) ?\)))))
      (YaTeX-reindent
       (save-excursion
	 (YaTeX-goto-corresponding-environment)
	 (current-column))))
     ;; on the begining of verbatime line, remove all indentation
     ((and begend ;; match-beginning 0 of \begin
	   YaTeX-noindent-env-regexp
	   (stringp YaTeX-noindent-env-regexp)
	   (save-excursion
	     (and ;; if the \begin is the first declaration of this line
	      (progn (beginning-of-line) (skip-chars-forward " \t")
		     (= begend (point)))
	      (progn
		(goto-char begend)
		(looking-at
		 (concat YaTeX-ec-regexp
			 "begin{\\(" YaTeX-noindent-env-regexp "\\)}"))))))
      (save-excursion
	(goto-char begend)
	(delete-region (point) (point-beginning-of-line))))
     ((string-match YaTeX-equation-env-regexp inenv)
      (YaTeX-indent-line-equation))	;autoload-ed from yatexenv
     (;(YaTeX-in-environment-p '("itemize" "enumerate" "description" "list"))
      (string-match YaTeX-itemizing-env-regexp inenv)
      ;;(YaTeX-on-item-p) ??
      ;;(setq iteminfo (YaTeX-get-item-info t))
      (if (save-excursion
	    (beginning-of-line)
	    (re-search-forward YaTeX-item-regexp peol t))
	  (progn
	    (save-excursion
	      (goto-char (1+ (match-beginning 0)))
	      (setq depth
		    (* YaTeX-environment-indent
		       (cond
			((looking-at "subsubsub")	3)
			((looking-at "subsub")	2)
			((looking-at "sub")	1)
			(t			0)))))
	    (funcall indent-relative depth))
	(YaTeX-reindent (or (car (cdr (YaTeX-get-item-info t inenv)))
			    (+ (save-excursion
				 (beginning-of-line)
				 (YaTeX-current-indentation))
			       YaTeX-environment-indent))))
      )
     ((YaTeX-literal-p)			;verbatims
      (tab-to-tab-stop))
     ((string-match "\\(tabular\\|array\\)" inenv) ;1.73
      (let ((n 1))
	(condition-case err
	    (save-excursion
	      (beginning-of-line)
	      (skip-chars-forward "[ \t]")
	      ;;(if (looking-at "&") (forward-char 1))
	      (require 'yatexenv)
	      (setq n (car (YaTeX-array-what-column-internal))))
	  (error nil))
	(YaTeX-reindent
	 (+ (YaTeX-current-indentation)
	    YaTeX-environment-indent
	    (* (1- n) YaTeX-tabular-indentation)))))
     ((and inenv (not (equal "document" inenv)))
      (funcall indent-relative))
     ((YaTeX-on-section-command-p YaTeX-sectioning-regexp)
      (require 'yatexsec)		;to know YaTeX-sectioning-level
      (setq sect (YaTeX-match-string 1))
      (if (string-match "\\*$" sect)
	  (setq sect (substring sect 0 -1)))
      (YaTeX-reindent
       (* (max
	   (1-				;I want chapter to have indentation 0
	    (or (cdr (assoc sect YaTeX-sectioning-level))
		0))
	   0)
	  YaTeX-environment-indent)))
     ;;Default movement
     ((and (bolp) fill-prefix) (insert fill-prefix))
     (t (save-excursion
	  (beginning-of-line)
	  (if fill-prefix
	      (progn
		(delete-region (point)
			       (progn (skip-chars-forward " \t")
				      (point)))
		(insert fill-prefix))
	    (skip-chars-forward " \t")
	    (if (bobp)
		nil
	      (indent-relative-maybe))))
	(skip-chars-forward " \t")))
    ;;if current line is \begin, re-indent \end too
    (if (and (YaTeX-on-begin-end-p) (match-beginning 1))
	(save-excursion
	  ;;(beginning-of-line)
	  ;;(search-forward "\\begin")
	  (goto-char (match-beginning 0))
	  (setq depth (current-column))
	  (YaTeX-goto-corresponding-environment)
	  (YaTeX-reindent depth)))
    (if (or
	 (and NTT-jTeX
	      (save-excursion (beginning-of-line) (looking-at "[ \t]")))
	 (save-excursion
	   (beginning-of-line)
	   (and
	    (not (bobp))
	    (progn
	      (backward-char 1)
	      (re-search-backward
	       "\\\\\\(\\(page\\)?ref\\|cite\\){" (point-beginning-of-line) t))
	    (goto-char (1- (match-end 0)))
	    (> (save-excursion
		 (condition-case ()
		     (progn (forward-list 1) (point))
		   (error (point-max))))
	       (point-end-of-line)))))
	(save-excursion
	  (end-of-line)
	  (let ((p (point)))
	    (forward-line -1)
	    (end-of-line)
	    (or (= p (point))
		(looking-at (regexp-quote YaTeX-comment-prefix))
		(bobp) (bolp)
		(save-excursion
		  (backward-word 1)
		  (looking-at "\\sw+")) ;is not japanese string
		(insert YaTeX-comment-prefix)))))))

(defun YaTeX-comment-line-break (&optional soft)
  "Call comment-indent-new-line and YaTeX-indent-line"
  (comment-indent-new-line soft)
  (YaTeX-indent-line))

(defun YaTeX-latex2e-p ()
  (let ((b (current-buffer))
	(ptn (concat YaTeX-ec "documentclass")))
    (unwind-protect
	(or (save-excursion (search-backward ptn nil t))
	    (progn
	      (YaTeX-visit-main t)
	      (save-excursion (search-backward ptn nil t))))
      (set-buffer b))))

(provide 'yatex)
(defvar yatex-mode-load-hook nil
  "*List of functions to be called when yatex.el is loaded.")
(if (and YaTeX-emacs-19 YaTeX-display-color-p (not (featurep 'yatex19)))
    (load "yatex19"))
(load "yatexhks" t)

;;-------------------- Final hook jobs --------------------
(substitute-all-key-definition
 'fill-paragraph 'YaTeX-fill-paragraph YaTeX-mode-map)
(substitute-all-key-definition
 'kill-buffer 'YaTeX-kill-buffer YaTeX-mode-map)
(run-hooks 'yatex-mode-load-hook)

;; `History' was moved to ChangeLog
;----------------------------- End of yatex.el -----------------------------

; Local variables:
; coding: sjis
; End:
