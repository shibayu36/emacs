;;; -*- Emacs-Lisp -*-
;;; YaTeX facilities for Emacs 19 or later
;;; (c)1994-2009 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Mon Sep 28 10:45:30 2009 on firestorm
;;; $Id: yatex19.el,v 1.74 2009/09/28 01:54:43 yuuji Rel $

;(require 'yatex)

(and (boundp 'YaTeX-use-hilit19)
     YaTeX-use-hilit19
     (require 'hilit19))

(defvar YaTeX-use-highlighting (or YaTeX-use-font-lock YaTeX-use-hilit19)
  "*Use highlighting buffer or not.")
(defvar YaTeX-background-mode
  (or (if (fboundp 'get-frame-background-mode)
          (get-frame-background-mode (selected-frame)))
      (if (fboundp 'frame-parameters)
          (cdr (assq 'background-mode (frame-parameters))))
      (if (boundp 'frame-background-mode)
          frame-background-mode)
      (if (boundp 'hilit-background-mode)
          hilit-background-mode)
      (if (face-background 'default)
          (if (> (+ 32768 32768 32768)
                 (apply '+
                        (funcall (if (fboundp 'color-rgb-components)
                                     'color-rgb-components
                                   'x-color-values)
                                 (face-background 'default))))
              'dark
            'light))))

(defvar YaTeX-mode-menu-map (make-sparse-keymap "YaTeX"))
(defvar YaTeX-mode-menu-map-process (make-sparse-keymap "Process"))
(define-key YaTeX-mode-map [menu-bar yatex]
  (cons "YaTeX" YaTeX-mode-menu-map))
(YaTeX-define-menu
 'YaTeX-mode-menu-map-process
 (nreverse
 '((buffer "LaTeX" . (lambda () (interactive) (YaTeX-typeset-menu nil ?j)))
   (kill "Kill LaTeX" . (lambda () (interactive) (YaTeX-typeset-menu nil ?k)))
   (bibtex "BibTeX" . (lambda () (interactive) (YaTeX-typeset-menu nil ?b)))
   (mindex "makeindex" . (lambda () (interactive) (YaTeX-typeset-menu nil ?i)))
   (preview "Preview" . (lambda () (interactive) (YaTeX-typeset-menu nil ?p)))
   (lpr "lpr" . (lambda () (interactive) (YaTeX-typeset-menu nil ?l)))
   (lpq "lpq" . (lambda () (interactive) (YaTeX-typeset-menu nil ?q))))))
(defvar YaTeX-mode-menu-map-modes (make-sparse-keymap "Modes"))
(YaTeX-define-menu
 'YaTeX-mode-menu-map-modes
 (delq nil
       (nreverse
	(list
	 (if YaTeX-auto-math-mode nil
	   (cons 'math (cons "Toggle math-mode"
			     '(lambda () (interactive)
				(YaTeX-switch-mode-menu nil ?t)))))
	 (cons 'mod (cons "Toggle Modify Mode"
			  '(lambda () (interactive)
			     (YaTeX-switch-mode-menu nil ?m))))))))
(defvar YaTeX-mode-menu-map-percent (make-sparse-keymap "percent"))
(YaTeX-define-menu
 'YaTeX-mode-menu-map-percent
 (nreverse
  '((!		"Change LaTeX typesetter(%#!)"
		. (lambda () (interactive) (YaTeX-%-menu nil nil ?!)))
    (begend	"Set %#BEGIN-%#END on region"
		. (lambda () (interactive) (YaTeX-%-menu nil nil ?b)))
    (lpr 	"Change LPR format"
		. (lambda () (interactive) (YaTeX-%-menu nil nil ?l))))))

(defvar YaTeX-mode-menu-map-jump (make-sparse-keymap "jump"))
(YaTeX-define-menu
 'YaTeX-mode-menu-map-jump
 (nreverse
 '((corres     "Goto corersponding position" . YaTeX-goto-corresponding-*)
   (main      "Visit main source"
	      . (lambda () (interactive) (YaTeX-visit-main)))
   (main-other "Visit main source other window"
	       . YaTeX-visit-main-other-window))))

(defvar YaTeX-mode-menu-map-comment (make-sparse-keymap "comment"))
(YaTeX-define-menu
 'YaTeX-mode-menu-map-comment
 (nreverse
  '((comment	"Comment region or environment" . YaTeX-comment-region)
    (uncomment	"Unomment region or environment" . YaTeX-uncomment-region)
    (commentp	"Comment paragraph" . YaTeX-comment-paragraph)
    (uncommentp	"Uncomment paragraph" . YaTeX-uncomment-paragraph))))

(YaTeX-define-menu
 'YaTeX-mode-menu-map
 (nreverse
  (list
   ;; Change/Kill/Fill -------------------------------------------------------
   (cons (list 'chg "Change") (cons "Change macros"  'YaTeX-change-*))
   (cons (list 'kill "Kill") (cons "Kill macros"  'YaTeX-kill-*))
   (cons (list 'fill "Fill") (cons "Fill \\item"  'YaTeX-fill-item))
   (cons (list 'nl "Newline") (cons "Newline"  'YaTeX-intelligent-newline))
   ;; ========================================================================
   (cons (list 'sep1 "---") (cons "---" nil))
   ;; Comment/Uncomment ------------------------------------------------------
   (cons (list 'comment "comment") (cons "Comment region or environment"
					 'YaTeX-comment-region))
   (cons (list 'uncomment "uncomment") (cons "Uncomment region or environment"
					     'YaTeX-uncomment-region))
   (cons (list 'commentp "commentp") (cons "Comment paragraph"
					   'YaTeX-comment-paragraph))
   (cons (list 'uncommentp "uncommentp") (cons "Uncomment paragraph"
					       'YaTeX-uncomment-paragraph))
   ;; ========================================================================
   (cons (list 'sep2 "---") (cons "---" nil))
   ;; Jump cursor ------------------------------------------------------------
   (cons (list 'jump "jump") (cons "Jump Cursor" YaTeX-mode-menu-map-jump))
   ;; Document hierarchy  ---------------------------------------------------
   (cons (list 'hier "hier") (cons "Display Document hierarchy"
				    'YaTeX-display-hierarchy))
   ;; What position ----------------------------------------------------------
   (cons (list 'col "column") (cons "What column in tabular"
				    'YaTeX-what-column))
   ;; % menu -----------------------------------------------------------------
   (cons (list 'percent "percent") (cons "Edit %# notation"
				   YaTeX-mode-menu-map-percent))
   ;; Switch modes -----------------------------------------------------------
   (cons (list 'mode "mode") (cons "Switching YaTeX's modes"
				   YaTeX-mode-menu-map-modes))
   ;; ========================================================================
   (cons (list 'sep "---") (cons "---" nil))
   ;; Help for LaTeX ---------------------------------------------------------
   (cons (list 'ap "apr") (cons "Apropos on LaTeX commands" 'YaTeX-apropos))
   (cons (list 'help "help") (cons "Help on LaTeX commands" 'YaTeX-help))
   ;; Menu for Typeset relating processes ------------------------------------
   (cons (list 'process "Process menu")
	 (cons "Process" YaTeX-mode-menu-map-process)))
))

;; Make section-type commands menu -------------------------------------------
(defvar YaTeX-mode-menu-map-sectionr
      (make-sparse-keymap "Enclose region with section-type macro"))
(defvar YaTeX-mode-menu-map-section (make-sparse-keymap "Section-type macro"))
(let ((sorted-section
       (sort
	(delq nil
	      (mapcar (function (lambda (s)
				  (if (> (length (car s)) 5)
				      (car s))))
		      (append section-table user-section-table)))
	'string<)))
  (YaTeX-define-menu
   'YaTeX-mode-menu-map-section
   (mapcar
    (function (lambda (secname)
		(cons (intern secname)
		      (cons secname
			    (list 'lambda ()
				  (list 'interactive)
				  (list 'YaTeX-make-section
					nil nil nil
					secname))))))
    sorted-section))
  (YaTeX-define-menu
   'YaTeX-mode-menu-map-sectionr
   (mapcar 
    (function (lambda (secname)
		(cons (intern secname)
		      (cons secname
			    (list 'lambda ()
				  (list 'interactive)
				  (list 'YaTeX-make-section
					nil
					(list 'region-beginning)
					(list 'region-end)
					secname))))))
    sorted-section)))

(YaTeX-define-menu
 'YaTeX-mode-menu-map
 (nreverse
  (list
   (cons '(sectionr "Section-type (long name)")
	 (cons "Section type" YaTeX-mode-menu-map-section))
   (cons '(section "Section-type region (long name)")
	 (cons "Section type region (long name)"
	       YaTeX-mode-menu-map-sectionr)))))

;; Make large-type commands menu ---------------------------------------------
(defvar YaTeX-mode-menu-map-envr (make-sparse-keymap "Environment region"))
(defvar YaTeX-mode-menu-map-env (make-sparse-keymap "Environment"))

(let ((sorted-env
       (sort
	(mapcar (function (lambda (s) (car s)))
		(append env-table user-env-table))
	'string<)))
  (YaTeX-define-menu
   'YaTeX-mode-menu-map-env
   (mapcar
    (function (lambda (envname)
		(cons (intern envname)
		      (cons envname
			    (list 'lambda ()
				  (list 'interactive)
				  (list 'YaTeX-insert-begin-end
					envname nil))))))
    sorted-env))
  (YaTeX-define-menu
   'YaTeX-mode-menu-map-envr
   (mapcar 
    (function (lambda (envname)
		(cons (intern envname)
		      (cons envname
			    (list 'lambda ()
				  (list 'interactive)
				  (list 'YaTeX-insert-begin-end
					envname t))))))
    sorted-env)))
(YaTeX-define-menu
 'YaTeX-mode-menu-map
 (nreverse
  (list
   (cons '(envr "Environment")
	 (cons "Environment" YaTeX-mode-menu-map-env))
   (cons '(env "Environment region")
	 (cons "Environment region"
	       YaTeX-mode-menu-map-envr)))))

(and (featurep 'xemacs)
     (add-hook 'yatex-mode-hook
	       '(lambda ()
		  (or (assoc "YaTeX" current-menubar)
		      (progn
			(set-buffer-menubar (copy-sequence current-menubar))
			(add-submenu nil YaTeX-mode-menu-map))))))

;; Other key bindings for window-system
;(YaTeX-define-key [?\C- ] 'YaTeX-do-completion)
(define-key YaTeX-mode-map [?\M-\C- ] 'YaTeX-mark-environment)

;; Highlightening
;; メニューに比べてこっちは結構本気でやってます。
;; だって文書構造がとっても分かり易いんだもん。
;; みんなも hilit19.el を使おう!
;; とかいってるうちに hilit19 って obsolete になってしまった…
;; …ということで、hilit19 用のパターンを font-lock に変換する関数を
;; 作成してなんとか font-lock にも対応(2000年12月)。
;; しかし、font-lock は仕様が変わりやすい雰囲気でずっと動き続けるか
;; どうかは不明。むしろ進化の止まったhilit19を使い続ける方が安心と
;; 言えないこともないが世の流れは読めず……。
;;
;; さて、まずは対応する {} をピカピカ範囲とするような関数を作る。
;; これは hilit-LaTeX.el を参考にした。でも、ちゃんと section 型コマンドの
;; 引数を数えて正しい位置までピカピカさせるよ～ん!

(defun YaTeX-19-region-section-type (pattern)
  "Return cons of starting and end point of section-type commands of PATTERN."
  (if (re-search-forward pattern nil t)
      (let ((m0 (match-beginning 0)) (e0 (match-end 0)) cmd (argc 1))
	(setq cmd (substring (YaTeX-match-string 0) 1)
	      argc (or (car (cdr (YaTeX-lookup-table cmd 'section))) argc))
	(if (= argc 0) (cons m0 (point)) ;引数個数0ならマッチした領域
	  (skip-chars-forward " \n\t*")
	  (while (looking-at "\\[") (forward-list 1)) ;optionならスキップ
	  (skip-chars-forward " \n\t")
	  (prog1
	      (if (looking-at "{")	;{}が始まるならちゃんとしたsection型
		  (cons m0
			(condition-case err
			    (progn
			      ;;(skip-chars-backward "^{") (forward-char -2)
			      (while (> argc 0)
				(skip-chars-forward "^{")
				(forward-list 1)
				(setq argc (1- argc)))
			      (point))
			  (error m0)))
			;{}でないならたぶん \verb 環境などにあるダミー
		(cons m0 e0))
	    ;;move to re-search end not to make font-lock confused
	    (goto-char e0))))))

(defun YaTeX-19-region-large-type (pattern)
  "Return cons of large-type contents.
Assumes PATTERN begins with `{'."
  (if (re-search-forward pattern nil t)
      (let ((m0 (match-beginning 0)) (e0 (match-end 0))p)
	(goto-char m0)
	(skip-chars-forward "^ \t\n")
	(skip-chars-forward " \t\n")
	(prog1
	    (cons (setq p (point))
		  (condition-case err
		      (progn (goto-char m0) (forward-list 1) (1- (point)))
		    (error (1+ p))))
	  ;;move to re-search end not to make font-lock confused
	  (goto-char e0)))))

(defun YaTeX-19-region-env-type (envptn)
  "Return cons of environment contents specified by ENVPTN as regexp."
  (if (and (looking-at envptn) ;;re-search-forward envptn nil t)
	   (save-excursion
	     (not(search-backward YaTeX-comment-prefix
				  (point-beginning-of-line) t))))
      (let ((m0 (match-beginning 0)) (e0 (match-end 0))
	    (env (YaTeX-match-string 1))
	    (nextline (progn (forward-line 1) (point))))
	(goto-char m0)
	;(message "max=%d" (point-max))(sit-for 2)
	(condition-case err
	    (if (YaTeX-goto-corresponding-environment)
		(prog1
		    (cons nextline (match-beginning 0))
		  (goto-char e0)))
	  (error nil)))))

(defun YaTeX-19-region-paren-math (ptn)
  "Return cons of \(...\) or \[...\] type math environment."
  (if (looking-at "\\\\\\([\[(]\\)")
      (let*((ptype (cdr (assoc (YaTeX-match-string 1)
			       '(("(" . ")") ("[" "]")))))
	    (b (match-beginning 0))
	    (e (match-end 0)))
	(condition-case err
	    (if (re-search-forward
		 (concat "[^\\]\\\\" (regexp-quote ptype))
		 nil t)
		(prog1 (cons b (match-beginning 0))
		  (goto-char e)))
	  (error nil)))))

(defun YaTeX-19-region-math-sub (ptn)
  "Return cons of _{...}"
  (if (and (looking-at ptn) 
	   (eq YaTeX-font-lock-formula-face
	       (get-text-property (point) 'face)))
      (let ((e (match-end 0)) (p (point)))
	(goto-char e)
	(prog1
	    (condition-case ()
		(if (looking-at "{")
		    (cons (1+ (point))
			  (progn (forward-list 1) (1- (point))))
		  (cons e
			(cond
			 ((looking-at (concat YaTeX-ec-regexp
					      YaTeX-TeX-token-regexp))
			  (match-end 0))
			 ;; other case??
			 (t (1+ (point)))))))
	  (goto-char e)))))

;; 些細なことだが % の前の文字もピカリとさせてしまうようで… >hilit19
;; ↓この関数は下の hilit-set-mode-patterns の "[^\\]\\(%\\).*$" に
;; 依存している
(defun YaTeX-19-region-comment (pattern)
  "Return list of comment start and end point."
  (if (re-search-forward pattern nil t)
      (cons (match-beginning 2) (match-end 0))))

;; 2006/6/23 match only if it's in specified envrironment.
(defun YaTeX-19-re-search-in-env (ptn_env)
  (catch 'done
    ;; For font-lock, this function should find it.
    (let (md r)
      (while (YaTeX-re-search-active-forward
	      (car ptn_env) YaTeX-comment-prefix nil t)
	(setq md (match-data)
	      r (string-match (cdr ptn_env)
			      (or (YaTeX-inner-environment 'quick) "")))
	(store-match-data md)
	(if r (setq r (cons (match-beginning 0) (match-end 0))))
	(if (or YaTeX-use-hilit19 r) (throw 'done r))
	(goto-char (match-end 0)))
      (throw 'done r))))

;;(make-face 'tt)
;;(set-face-font 'tt "-schumacher-clean-medium-r-normal--*-*-*-*-*-*-*-*")
;;(hilit-translate 'tt "white")

;; font-lockの関数呼びパターンの場合は正規表現が行末までマッチすると
;; hilit候補対象外にされてしまうので1字手前で正規表現を止める
(defvar YaTeX-hilit-patterns-alist
  '(
    ;; formulas
    (YaTeX-19-region-math-sub "[^\\]^" YaTeX-font-lock-math-sup-face overwrite)
    (YaTeX-19-region-math-sub "[^\\]_" YaTeX-font-lock-math-sub-face overwrite)
    (YaTeX-19-region-env-type
     "\\\\begin{\\(equation\\|eqnarray\\|displaymath\\|\\(x?x?\\|fl\\)align\\|multline\\|gather\\)" formula)
    ;(YaTeX-19-region-paren-math "\\\\" formula)
    ;;("[^\\]\\\\("  "\\\\)" formula)                   ; \( \)
    ;;("[^\\]\\\\\\[" "\\\\\\]" formula)                ; \[ \]
    ;; comments
    (YaTeX-19-region-comment "\\([^\\]\\|^\\)\\(%\\).*$" comment)

    (YaTeX-19-region-section-type "\\\\footnote\\(mark\\|text\\)?\\>" keyword)
    ("\\\\[a-z]+box" 0 keyword)
    (YaTeX-19-region-section-type "\\\\\\(v\\|h\\)space\\>" keyword)

    ;; (re-)define new commands/environments/counters
    (YaTeX-19-region-section-type
     "\\\\\\(re\\)?new\\(environment\\|command\\|theorem\\|length\\|counter\\)\\>"
     defun)
    (YaTeX-19-region-section-type
     "\\\\textbf\\>" bold)

    ;; various declarations/definitions
    (YaTeX-19-region-section-type
     "\\\\\\(set\\|setto\\|addto\\)\\(length\\|width\\|counter\\)\\>"
     define)
    (YaTeX-19-region-section-type
     "\\\\\\(title\\|author\\|date\\|thanks\\)\\>" define)

    ("\\\\document\\(style\\|class\\)\\(\\[.*\\]\\)?{" "}" decl)

    ("\\\\\\(begin\\|end\\|nofiles\\|includeonly\\|usepackage\\(\\[.*\\]\\)?\\){" "}" decl)
    ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" 0 decl)
    ("\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\){" "}" decl)
    ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" 0 decl)
    ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
     0 decl)
    ("\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|ss\\|tt\\)\\b" 0 decl)

    ;; label-like things
    ;;this should be customized by YaTeX-item-regexp
    ("\\\\\\(sub\\)*item\\b\\(\\[[^]]*\\]\\)?" 0 label)
    (YaTeX-19-region-section-type
     "\\\\\\(caption\\|bibitem\\)\\(\\[[^]]*\\]\\)?\\>" label)

    ;; things that do some sort of cross-reference
    (YaTeX-19-region-section-type
     "\\\\\\(\\(no\\|possessive\\)?cite[a-z]*\\|[a-z]*ref\\|label\\|index\\|glossary\\)\\>"
     crossref)

    ;; things that bring in external files
    ("\\\\\\(include\\|input\\|bibliography\\(style\\)?\\){" "}" include)

   ;; ("\\\\begin{\\(eqn\\|equation\\|x?x?align\\|split\\|multline\\|gather\\)"
   ;;  "\\\\end{\\(eqn\\|equation\\|x?x?align\\|split\\|multline\\|gather\\).*}"
   ;;  formula)
    ("\\([^\\$]\\|^\\)\\($\\($[^$]*\\$\\|[^$]*\\)\\$\\)" 2 formula); '$...$' or '$$...$$'

    ;; "wysiwyg" emphasis -- these don't work on nested expressions
    (YaTeX-19-region-large-type "{\\\\\\(em\\|it\\|sl\\)"  italic)
    (YaTeX-19-region-large-type "{\\\\bf" bold)
    ;;;(YaTeX-19-region-large-type "{\\\\tt" tt)
    ;;;("\\\\begin{verbatim" "\\\\end{verbatim" tt)

    ("``" "''" string)
    ("\\\\\\(new\\|clear\\(double\\)?\\)page\\>\\|\\\\\\(\\\\\\|cr\\)\\>"
     0 delimiter)
    ;; re-search-in-env seems to make it slow down. 2007/2/11
    ;;(YaTeX-19-re-search-in-env
    ;; ("&\\|\\\\hline" . "tabular\\|equation\\|eqn\\|array\\|align") delimiter)
    ;;(YaTeX-19-re-search-in-env ("\\\\[+-=><'`]" . "tabbing") delimiter)
    ("&\\|\\\\hline\\|\\\\[+-=><'`]" 0 delimiter)
    )
  "*Hiliting pattern alist for LaTeX text.")

;;(defvar YaTeX-hilit-pattern-adjustment-default nil)
;; ↑いらなくなった。
(defvar YaTeX-hilit-pattern-adjustment-private nil
  "*Adjustment hilit-pattern-alist for default yatex-mode's pattern.")
(defvar YaTeX-hilit-sectioning-face
  '(yellow/dodgerblue yellow/slateblue)
  "*Hilightening face for sectioning unit.  '(FaceForLight FaceForDark)")
(defvar YaTeX-hilit-sectioning-attenuation-rate
  '(15 40)
  "*Maximum attenuation rate of sectioning face. '(ForeRate BackRate)
Each rate specifies how much portion of RGB value should be attenuated
towards to lowest sectioning unit.  Numbers should be written in percentage.")
(defvar YaTeX-sectioning-patterns-alist nil
  "Hilightening patterns for sectioning units.")
(defvar YaTeX-hilit-singlecmd-face
  '("slateblue2" . "aquamarine")
  "*Hilightening face for maketitle type.  '(FaceForLight FaceForDark)")

;;; セクションコマンドを、構造レベルの高さに応じて色の濃度を変える
;;; 背景が黒でないと何が嬉しいのか分からないに違いない.
;;; もしかして白地の時は構造レベルに応じて色を明るくしたほうが良いのか?
;;; ...どうやらそうでもないらしい。これでいいみたい(2000/12)。
;(if (fboundp 'win32-color-values)
;    (fset 'x-color-values 'win32-color-values))

(defun YaTeX-19-create-face (sym fgcolor &optional bgcolor)
  "Create face named SYM with face of FGCOLOR/BGCOLOR."
  (cond
   ((and YaTeX-use-font-lock (fboundp 'defface))
    (custom-declare-face
     sym
     (list
      (list (list
	     '(class color)
	     ;(list 'background YaTeX-background-mode)
	     )
	    (delq nil
		  (append
		   (list ':foreground fgcolor)
		   (if bgcolor
		       (list ':background bgcolor))
		   ))
	    )
      (list t (list ':bold t ':underline t))
      )
     (format "Font lock face for %s" sym)
      ':group 'font-lock-faces)
    (set sym sym)
    sym)
   ((and YaTeX-use-hilit19 (and (fboundp 'hilit-translate)))
    (let ((face (intern (concat fgcolor "/" bgcolor))))
      (if (facep sym)
	  (hilit-translate sym face)
	(make-face sym)
	(or (memq sym hilit-predefined-face-list)
	    (progn
	      (set-face-foreground sym fgcolor)
	      (set-face-background sym bgcolor)
	      (setq hilit-predefined-face-list
		    (cons sym hilit-predefined-face-list)))))
      face))))

(cond
 (YaTeX-use-highlighting
  (cond
   (window-system
    (let*((sectface
	   (car (if (eq YaTeX-background-mode 'dark)
		    (cdr YaTeX-hilit-sectioning-face)
		  YaTeX-hilit-sectioning-face)))
	  (sectcol (symbol-name sectface))
	  (fl YaTeX-use-font-lock)
	  (form (if fl "#%02x%02x%02x" "hex-%02x%02x%02x"))
	  sect-pat-alist)
      (if (string-match "/" sectcol)
	  (let ((fmin (nth 0 YaTeX-hilit-sectioning-attenuation-rate))
		(bmin (nth 1 YaTeX-hilit-sectioning-attenuation-rate))
		colorvalue fR fG fB bR bG bB pat fg bg level from face list lm)
	    (require 'yatexsec)
	    (setq fg (substring sectcol 0 (string-match "/" sectcol))
		  bg (substring sectcol (1+ (string-match "/" sectcol)))
		  colorvalue (x-color-values fg)
		  fR (/ (nth 0 colorvalue) 256)
		  fG (/ (nth 1 colorvalue) 256)
		  fB (/ (nth 2 colorvalue) 256)
		  colorvalue (x-color-values bg)
		  bR (/ (nth 0 colorvalue) 256)
		  bG (/ (nth 1 colorvalue) 256)
		  bB (/ (nth 2 colorvalue) 256)
		  lm YaTeX-sectioning-max-level
		  list YaTeX-sectioning-level)
	    (while list
	      (setq pat (concat YaTeX-ec-regexp (car (car list))
				;;"\\*?\\(\\[[^]]*\\]\\)?\\>" ;改行はさむと駄目
				"\\>"
				)
		    level (cdr (car list))
		    fg (format form
			       (- fR (/ (* level fR fmin) lm 100))
			       (- fG (/ (* level fG fmin) lm 100))
			       (- fB (/ (* level fB fmin) lm 100)))
		    bg (format form
			       (- bR (/ (* level bR bmin) lm 100))
			       (- bG (/ (* level bG bmin) lm 100))
			       (- bB (/ (* level bB bmin) lm 100)))
		    from (intern (format "YaTeX-sectioning-%d" level))
		    ;;face (intern (concat fg "/" bg))
		    )
	      (setq face (YaTeX-19-create-face from fg bg))
	      (setq sect-pat-alist
		    (cons;;(list pat "}" face)
		     (list 'YaTeX-19-region-section-type pat face)
		     sect-pat-alist))
	      (setq list (cdr list)))
	    (setq YaTeX-sectioning-patterns-alist sect-pat-alist)))))
   (t					;not window-system
    (setq YaTeX-sectioning-patterns-alist
	  (list
	   (list
	    (concat YaTeX-ec-regexp
		    "\\(\\(sub\\)*\\(section\\|paragraph\\)\\|chapter"
		    "\\|part\\){[^}]*}")
	    0
	    'define)))))))

;; ローカルなマクロを読み込んだ後 redraw すると
;; ローカルマクロを keyword として光らせる(keywordじゃまずいかな…)。
(defvar hilit-patterns-alist nil)	;for absence of hilit19

(defun YaTeX-19-collect-macros ()
  (cond
   (YaTeX-use-highlighting
    (let ((get-face
	   (function
	    (lambda (table)
	      (cond
	       ((eq YaTeX-background-mode 'light) (car table))
	       ((eq YaTeX-background-mode 'dark) (cdr table))
	       ;; Default case equals to 'light mode...is it OK?
	       (t (car table))))))
	  sect single pattern-alist)
      (YaTeX-19-create-face ;;hilit-translate
       ;;sectioning (funcall get-face YaTeX-hilit-sectioning-face)
       'macro (funcall get-face YaTeX-hilit-singlecmd-face))
      (if (setq sect (append user-section-table tmp-section-table))
	  (setq sect (concat "\\\\\\("
			     (mapconcat
			      (function
			       (lambda (s) (regexp-quote (car s))))
			      sect
			      "\\|")
			     "\\)\\>")))
      (if (setq single (append user-singlecmd-table tmp-singlecmd-table))
	  (setq single (concat "\\\\\\("
			       (mapconcat
				(function (lambda (s) (regexp-quote (car s))))
				single
				"\\|")
			       "\\)\\b")))
      (cons 'yatex-mode
	    (append
	     (list nil)
	     YaTeX-sectioning-patterns-alist
	     YaTeX-hilit-pattern-adjustment-private
	     ;;YaTeX-hilit-pattern-adjustment-default
	     YaTeX-hilit-patterns-alist
	     (delq nil
		   (list
		    (if sect (list
			      'YaTeX-19-region-section-type
			      sect
			      'keyword))
		    (if single (list single 0 'macro))))))))))

;;2006/6/23 new face, `delimiter' introduced
(YaTeX-19-create-face 'delimiter "saddlebrown" "ivory")

;;(YaTeX-19-collect-macros)	;causes an error
(defun YaTeX-hilit-setup-alist ()
  (cond
   ((boundp 'hilit-patterns-alist)
    (setq hilit-patterns-alist
	  (delq (assq 'yatex-mode hilit-patterns-alist) hilit-patterns-alist))
    (if YaTeX-use-hilit19
	(setq hilit-patterns-alist
	      (cons (YaTeX-19-collect-macros) hilit-patterns-alist))))))

(defun YaTeX-hilit-recenter (arg)
  "Collect current local macro and hilit-recenter."
  (interactive "P")
  (YaTeX-hilit-setup-alist)
  (if (fboundp 'font-lock-mode) (font-lock-mode -1))
  (hilit-recenter arg))

(let ((k (append (where-is-internal 'hilit-recenter)
		 (where-is-internal 'recenter))))
  (while k
    (define-key YaTeX-mode-map (car k) 'YaTeX-19-recenter)
    (setq k (cdr k))))

(defun YaTeX-19-recenter (&optional arg)
  (interactive "P")
  (if YaTeX-use-hilit19
      (YaTeX-hilit-recenter arg)
    (YaTeX-font-lock-recenter arg)))

(defun YaTeX-font-lock-recenter (&optional arg)
  (interactive "P")
  (cond
   ((and (boundp 'hilit-patterns-alist)
	 (assq 'yatex-mode hilit-patterns-alist))
    (if (fboundp 'hilit-unhighlight-region)
	(hilit-unhighlight-region (point-min) (point-max)))
    (setq hilit-patterns-alist	;ensure to remove
	  (delq (assq 'yatex-mode hilit-patterns-alist)
		hilit-patterns-alist))))
  (setq YaTeX-font-lock-keywords
	(YaTeX-convert-pattern-hilit2fontlock
	 (cdr (YaTeX-19-collect-macros)))
;;; Keep this section for debugging.
;;    YaTeX-font-lock-keywords
;;    (append (YaTeX-convert-pattern-hilit2fontlock
;; 	    (cdr (YaTeX-19-collect-macros)))
;; 	   '(((lambda (lim)
;; 		(YaTeX-19-re-search-in-env '("foo" . "tabular"))
;; 		;(search-forward "foo" nil t)
;; 	       )
;; 	      (0 YaTeX-font-lock-delimiter-face))))
	;;font-lock-keywords nil
	font-lock-set-defaults nil)
  ;;(save-excursion
  ;; (font-lock-fontify-region (window-start) (window-end))
  (font-lock-mode -1)			;is stupid, but sure.
  (font-lock-mode 1)
  (recenter arg))

(defun YaTeX-font-lock-fontify-region (beg end)
  (interactive "r")
  (save-excursion (font-lock-fontify-region beg end)))

(defun YaTeX-font-lock-fontify-environment ()
  (interactive)
  (save-excursion
    (save-match-data			;is safe after emacs-19
      (YaTeX-mark-environment)
      (message "")
      (YaTeX-font-lock-fontify-region (region-beginning) (region-end)))))

(defun YaTeX-font-lock-highlight-menu ()
  (interactive)
  (message "Force Highlight: R)egion E)nvironment")
  (let ((c (read-char)))
    (cond
     ((memq c '(?R ?r))
      (YaTeX-font-lock-fontify-region (region-beginning) (region-end)))
     ((memq c '(?e ?e))
      (YaTeX-font-lock-fontify-environment)))))

(if YaTeX-use-font-lock
    (YaTeX-define-key "u" 'YaTeX-font-lock-highlight-menu))

(defvar YaTeX-font-lock-keywords nil
  "Pattern-face alist of yahtml-mode for font-lock")

(defun YaTeX-font-lock-set-default-keywords ()
  (put 'yatex-mode 'font-lock-defaults
       (list 'YaTeX-font-lock-keywords nil nil))
  (setq YaTeX-font-lock-keywords
	(YaTeX-convert-pattern-hilit2fontlock
	 (cons nil
	       (append YaTeX-sectioning-patterns-alist
		       YaTeX-hilit-pattern-adjustment-private
		       YaTeX-hilit-patterns-alist)))))

(if YaTeX-use-font-lock
    (progn
      (if (and (boundp 'hilit-mode-enable-list) hilit-mode-enable-list)
	  ;;for those who use both hilit19 and font-lock
	  (if (eq (car hilit-mode-enable-list) 'not)
	      (or (member 'yatex-mode hilit-mode-enable-list)
		  (nconc hilit-mode-enable-list (list 'yatex-mode)))
	    (setq hilit-mode-enable-list
		  (delq 'yatex-mode hilit-mode-enable-list))))
      (YaTeX-font-lock-set-default-keywords)))

(defun YaTeX-switch-to-new-window ()
  (let ((c 0) (i 1) (free (make-string win:max-configs ? )))
    (while (< i win:max-configs)
      (or (aref win:configs i) (aset free i (+ i win:base-key)))
      (setq i (1+ i)))
    (while (not (string-match (char-to-string c) free))
      (message "Which window to create? [%s]: " free)
      (setq c (read-char)))
    (message "Creating window [%c]" c)
    (set-buffer (get-buffer-create "*scratch*"))
    (win:switch-window (- c win:base-key))))

(defun YaTeX-visit-main-other-frame ()
  "Visit main file in other frame.
WARNING, This code is not perfect."
  (interactive)
  (if (YaTeX-main-file-p) (message "I think this is main LaTeX source.")
    (let (parent)
      (save-excursion (YaTeX-visit-main t) (setq parent (current-buffer)))
      (cond
       ((get-buffer-window parent t)
	(goto-buffer-window parent))
       ((and (featurep 'windows) win:use-frame)
	(YaTeX-switch-to-new-window)
	(switch-to-buffer parent))
       (t (switch-to-buffer-other-frame (buffer-name parent)))))))

(defun YaTeX-goto-corresponding-*-other-frame (arg)
  "Go to corresponding object in other frame."
  (interactive "P")
  (let (b p)
    (save-window-excursion
      (save-excursion
	(YaTeX-goto-corresponding-* arg)
	(setq b (current-buffer) p (point))))
    (cond
     ((get-buffer-window b t)
      (goto-buffer-window b)
      (goto-char p))
     ((and (featurep 'windows) win:use-frame)
      (YaTeX-switch-to-new-window)
      (switch-to-buffer b)
      (goto-char p))
     (t (switch-to-buffer-other-frame (buffer-name b))
	(goto-char p)))))

;;; reverseVideo にして hilit-background-mode を 'dark
;;; にしている人は数式などが暗くなりすぎて見づらいかもしれない。
;;; 次のコードを hilit19 をロードしている場所の直後に置くとちょっ
;;; とはまし。
;;;  (if (eq hilit-background-mode 'dark)
;;;      (hilit-translate
;;;       string 'mediumspringgreen
;;;       formula 'khaki
;;;       label 'yellow-underlined))
(and YaTeX-emacs-19
     (not (featurep 'xemacs))
     (boundp 'byte-compile-current-file)
     byte-compile-current-file
     (progn
       (if YaTeX-emacs-20 (require 'font-lock))
       (if (and (boundp 'window-system) window-system)
	   (require 'hilit19)
	 (error "Byte compile this file on window system! Not `-nw'!"))))

(provide 'yatex19)


; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; coding: sjis
; End:
