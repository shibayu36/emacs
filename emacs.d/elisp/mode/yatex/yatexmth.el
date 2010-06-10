;;; -*- Emacs-Lisp -*-
;;; YaTeX math-mode-specific functions.
;;; yatexmth.el
;;; (c)1993-2006 by HIROSE Yuuji [yuuji@yatex.org]
;;; Last modified Sun Dec 24 15:13:15 2006 on firestorm
;;; $Id: yatexmth.el,v 1.73 2006/12/24 06:17:15 yuuji Rel $

;;; [Customization guide]
;;;
;;;	  By default,  you can use two  completion  groups in YaTeX math
;;;	mode, `;' for mathematical signs and `:' for greek letters.  But
;;;	you  can add other completion groups   by defining the  alist of
;;;	`prefix  key'    vs   `completion   list'    into  the  variable
;;;	YaTeX-math-key-list-private.  If  you wish  to    accomplish the
;;;	completion as follows(prefix key is `,'):
;;;
;;;		KEY		COMPLETION
;;;		s		\sin
;;;		S		\arcsin
;;;		c		\cos
;;;		C		\arccos
;;;			:
;;;		T		\arctan
;;;		l		\log
;;;		hs		\sinh
;;;
;;;	Typing `s' after `,' makes `\sin', `hs' after `,' makes `\sinh'
;;;	and so on.  First, you have to define list of key-completion
;;;	pairs.  Variable name(YaTeX-math-funcs-list) is arbitrary.
;;;
;;;		(setq YaTeX-math-funcs-list
;;;		      '(("s"	"sin")
;;;			("S"	"arcsin")
;;;				:
;;;			("hs"	"sinh")))
;;;
;;;	Next, define the list of prefix-key vs completion list(defined
;;;	above) into the variable YaTeX-math-key-list-private.
;;;
;;;		(setq YaTeX-math-key-list-private
;;;		      '(("," . YaTeX-math-funcs-list)
;;;			("'" . Other-List-if-any)))
;;;
;;;	  Put these expressions into your ~/.emacs, and you can use this
;;;	completion in the math-mode.
;;;
;;;	  And you can add your favorite completion sequences to the
;;;	default completion list invoked with `;', by defining those lists
;;;	into variable YaTeX-math-sign-alist-private.

;;; 【イメージ補完の追加方法】
;;;
;;;	  標準のイメージ補完では、「;」で始まる数式記号補完と、「:」で始
;;;	まるギリシャ文字補完が使用可能ですが、これ以外の文字で始まる補完
;;;	シリーズも定義することができます。例えば、「,」で始まる次のよう
;;;	な補完シリーズを定義する場合を考えてみます。
;;;
;;;		補完キー	補完結果
;;;		s		\sin
;;;		S		\arcsin
;;;		c		\cos
;;;		C		\arccos
;;;			:
;;;		T		\arctan
;;;		l		\log
;;;		hs		\sinh
;;;
;;;	「,」のあとに s を押すと \sin が、hs を押すと \sinh が入力されま
;;;	す。このような補完リストの登録は以下のようにします(変数名は任意)。
;;;
;;;		(setq YaTeX-math-funcs-list
;;;		      '(("s"	"sin")
;;;			("S"	"arcsin")
;;;				:
;;;			("hs"	"sinh")))
;;;
;;;	さらに、「,」を押した時にイメージ補完が始まるよう次の変数に、起動キー
;;;	と上で定義した補完用変数の登録を行ないます。
;;;
;;;		(setq YaTeX-math-key-list-private
;;;		      '(("," . YaTeX-math-funcs-list)
;;;			("'" . ほかに定義したいシリーズがあれば…)))
;;;
;;;	これらを ~/.emacs に書いておけば、math-mode で自分専用のイメージ
;;;	補完が利用できます。

(defvar YaTeX-jisold
  (and (boundp 'dos-machine-type)
       (eq dos-machine-type 'pc98)))

(defmacro YaTeX-setq-math-sym (sym old new)
  (list 'setq sym (list 'if 'YaTeX-jisold old new)))

(YaTeX-setq-math-sym YaTeX-image-in		"E"    		"∈")
(YaTeX-setq-math-sym YaTeX-image-ni		"ヨ"		"∋")
(YaTeX-setq-math-sym YaTeX-image-subset		" _\n(\n ~"	"⊂")
(YaTeX-setq-math-sym YaTeX-image-subseteq	" _\n(_\n ~"	"⊆")
(YaTeX-setq-math-sym YaTeX-image-supset		"_\n )\n~"	"⊃")
(YaTeX-setq-math-sym YaTeX-image-supseteq	"_\n_)\n~" 	"⊇")
(YaTeX-setq-math-sym YaTeX-image-nabla		"___\n\\\\/"	"∇")
(YaTeX-setq-math-sym YaTeX-image-partial	" -+\n+-+\n+-+" "∂")
(YaTeX-setq-math-sym YaTeX-image-dagger		"+\n|"		"†")
(YaTeX-setq-math-sym YaTeX-image-ddagger	"+\n+\n|"	"‡")
(YaTeX-setq-math-sym YaTeX-image-equiv		"＝\n￣"	"≡")
(YaTeX-setq-math-sym YaTeX-image-int		" /\\\n \\\n\\/" "∫")
(YaTeX-setq-math-sym YaTeX-image-bot		"｜\n￣"	"⊥")
(YaTeX-setq-math-sym YaTeX-image-neg		"�ｲ"		"￢")
(YaTeX-setq-math-sym YaTeX-image-flat		"ｂ"		"♭")
(YaTeX-setq-math-sym YaTeX-image-sqrt		"√"		"√")
(defvar YaTeX-image-nearrow '("__\n /|\n/" "  ＿\n  ／|\n／" ))
(defvar YaTeX-image-nwarrow '(" __\n|\\\n  \\" " ＿\n|＼\n   ＼"))
(defvar YaTeX-image-searrow '("\\\n \\|\n--`" "＼\n  ＼|\n  ￣"))
(defvar YaTeX-image-swarrow '("  /\n|/\n'~~" "   ／\n|／\n ￣"))


(defvar
 YaTeX-math-sign-alist-default
 '(
   ;frequently used
   ("||"	"|"		("||"		"∥"))
   ("sum"	"sum"		("\\-+\n >\n/-+" "Σ"))
   ("sigma"	"sum"		("\\-+\n >\n/-+" "Σ"))
   ("integral"	"int"		(" /\\\n \\\n\\/" YaTeX-image-int))
   ("ointegral"	"oint"		" /\\\n(\\)\n\\/")
   ("sqrt"	"sqrt"		("  __\n,/" YaTeX-image-sqrt))
   ("root"	"sqrt"		("  __\n,/" YaTeX-image-sqrt))
   ("A"		"forall"	"|_|\nV")
   ("E"		"exists"	"-+\n-+\n-+")
   ("!"		"neg"		("--+\n  |" YaTeX-image-neg))
   ("oo"	"infty"		("oo"		"∞"))
   ("\\"	"backslash"	("\\"		"＼"))
   ("..."	"cdots"		("..."		"…"))

   ;;binary operators
   ("+-"	"pm"		("+\n-" "±"))
   ("-+"	"mp"		"-\n+")
   ("x"		"times"		("x" "×"))
   ("/"		"div"		(",\n-\n'" "÷"))
   ("f"		"frac"		"xxx\n---\nyyy" "÷")
   ("*"		"ast"		"*")
   ("#"		"star"		("_/\\_\n\\  /\n//\\\\" "★"))
   ("o"		"circ"		"o")
   ("o*"	"bullet"	" _\n(*)\n ~")
   ("."		"cdot"		".")
   ("cap"	"cap"		"/-\\\n| |")
   ("cup"	"cup"		"| |\n\\-/")
   ("u+"	"uplus"		"|+|\n\\-/")
   ("|~|"	"sqcap"		"|~|")
   ("|_|"	"sqcup"		"|_|")
   ("v"		"vee"		"v")
   ("^"		"wedge"		"^")
   ("\\\\"	"setminus"	"\\")
   (")("	"wr"		" )\n(")
   ("<>"	"diamond"	"<>")
   ("/\-"	"bigtriangleup"	("/\\\n~~" "△"))
   ("-\\/"	"bigtriangledown" ("__\n\\/" "▽"))
   ("<|"	"triangleleft"	"<|")
   ("|>"	"triangleright"	"|>")
   ("<||"	"lhd"		"/|\n\\|")
   ("||>"	"rhd"		"|\\\n|/")
   ("<|-"	"unlhd"		"<|\n~~")
   ("|>-"	"unrhd"		"|>\n~~")
   ("o+"	"oplus"		" _\n(+)\n ~")
   ("o-"	"ominus"	" _\n(-)\n ~")
   ("ox"	"otimes"	" _\n(x)\n ~")
   ("o/"	"oslash"	" _\n(/)\n ~")
   ("o."	"odot"		"(.)")
   ("O"		"bigcirc"	"O")
   ("t"		"dagger"	("+\n|" YaTeX-image-dagger))
   ("tt"	"ddagger"	("+\n+\n|" YaTeX-image-ddagger))
   ("II"	"amalg"		"II")
   ;	:
   ;;relational operators
   ("<"		"leq"		("<\n-"		"≦"))
   (">"		"geq"		(">\n-"		"≧"))
   ("-="	"equiv"		("=\n-"		YaTeX-image-equiv))
   ("=-"	"equiv"		("=\n-"		YaTeX-image-equiv))
   ("---"	"equiv"		("=\n-"		YaTeX-image-equiv))
   ("("		"subset"	(" _\n(\n ~"	YaTeX-image-subset))
   ("(-"	"subseteq"	(" _\n(_\n ~"	YaTeX-image-subseteq))
   (")"		"supset"	("_\n )\n~"	YaTeX-image-supset))
   (")-"	"supseteq"	("_\n_)\n~"	YaTeX-image-supseteq))
   ("["		"sqsubset"	"[")
   ("[-"	"sqsubseteq"	"[\n~")
   ("]"		"sqsupset"	"]")
   ("]-"	"sqsupseteq"	"]\n~")
   ("{"		"in"		("(-" YaTeX-image-in))
   ("}"		"ni"		("-)" YaTeX-image-ni))
   ("|-"	"vdash"		"|-")
   ("-|"	"dashv"		"-|")
   ("~"		"sim"		"~(tild)")
   ("~-"	"simeq"		"~\n-")
   ("asymp"	"asymp"		"v\n^")
   ("~~"	"approx"	"~\n~")
   ("~="	"cong"		"~\n=")
   ("=/"	"neq"		("=/="		"≠"))
   (".="	"doteq"		".\n=")
   ("o<"	"propto"	"o<")
   ("|="	"models"	"|=")
   ("_|_"	"perp"		"_|_")
   ("|"		"mid"		"|")
   ("||"	"parallel"	"||")
   ("bowtie"	"bowtie"	"|><|(wide)")
   ("|><|"	"join"		"|><|")
   ("\\_/"	"smile"		"\\_/")
   ("/~\\"	"frown"		"/~~\\")
   ("-<"	"prec"		("-<"		"く"))
   ("-<="	"preceq"	("-<\n-"	"く\n="))
   ("<<"	"ll"		("<<"		"《"))
   (">>"	"gg"		(">>"		"》"))
   ;	:
   ;;arrows
   ("<-"	"leftarrow"	("<-"		"←"))
   ("\C-b"	"leftarrow"	("<-"		"←"))
   ("<--"	"longleftarrow"	("<--"		"←--"))
   ("<="	"Leftarrow"	"<=")
   ("<=="	"Longleftarrow"	"<==")
   ("->"	"rightarrow"	("->"		"→"))
   ("\C-f"	"rightarrow"	("->"		"→"))
   ("-->"	"longrightarrow" ("-->"		"--→"))
   ("=>"	"Rightarrow"	"=>")
   ("==>"	"Longrightarrow" "==>")
   ("<->"	"leftrightarrow" ("<->"		"←→"))
   ("<-->"	"longleftrightarrow" ("<---->"	"←--→"))
   ("<=>"	"Leftrightarrow" "<=>")
   ("<==>"	"Longleftrightarrow" "<==>")
   ("^|"	"uparrow"	("^\n|" "↑"))
   ("\C-p"	"uparrow"	("^\n|" "↑"))
   ("^||"	"Uparrow"	"/\\\n||")
   ("\C-n"	"downarrow"	("|\nv" "↓"))
   ("v|"	"downarrow"	("|\nv" "↓"))
   ("v||"	"Downarrow"	"||\n\\/")
   ("\C-p\C-f"	"nearrow"	YaTeX-image-nearrow)
   ("\C-f\C-p"	"nearrow"	YaTeX-image-nearrow)
   ("ne"	"nearrow"	YaTeX-image-nearrow)
   ("\C-p\C-b"	"nwarrow"	YaTeX-image-nwarrow)
   ("\C-b\C-p"	"nwarrow"	YaTeX-image-nwarrow)
   ("nw"	"nwarrow"	YaTeX-image-nwarrow)
   ("\C-n\C-f"	"searrow"	YaTeX-image-searrow)
   ("\C-f\C-n"	"searrow"	YaTeX-image-searrow)
   ("se"	"searrow"	YaTeX-image-searrow)
   ("\C-n\C-b"	"swarrow"       YaTeX-image-swarrow)
   ("\C-b\C-n"	"swarrow"	YaTeX-image-swarrow)
   ("sw"	"swarrow"	YaTeX-image-swarrow)
   ("|->"	"mapsto"	("|->"		"|→"))
   ("<-)"	"hookleftarrow"	("   ,\n<--+"	"   ヽ\n<--/"))
   ("(->"	"hookrightarrow" ("`\n+-->"	"／\n＼-->"))
   ("/-"	"leftharpoonup"	"/\n~~~")
   ("\\-"	"leftharpoondown" "__\n\\")
   ("-/"	"rightharpoondown"  "__\n/")
   ("-\\"	"rightharpoonup" "~~\n\\")
   ;;left and right
   ("left"	"left"		"(leftmark)")
   ("right"	"right"		"(rightmark)")
   ;;accent marks
   ("tilde"	"tilde"		"~\n?")
   ("T"		"tilde"		"~\n?")
   ("wtilde"	"widetilde"	"~\n?")
   ("hat"	"hat"		"^\n?")
   ("what"	"widehat"	"/\\\n??")
   ("w^"	"widehat"	"/\\\n?")
   ("check"	"check"		"v\n?")
   ("bar"	"bar"		"_\n?")
   ("overline"	"overline"	"_\n?")
   ("wbar"	"overline"	"--\n??")
   ("dot"	"dot"		".\n?")
   ("ddot"	"ddot"		"..\n??")
   ("vec"	"vec"		("->\n??" "→\n??"))
   ("~>"	"overrightarrow" ("-->\nAB" "→\nAB"))
   ("VEC"	"overrightarrow" ("-->\nAB" "→\nAB"))
   ;;rage-aware stuffs
   ("prod"	"prod"		("-+--+-\n |  |" "Π"))
   ("CUP"	"bigcup"	"|~~|\n|  |\n|  |")
   ("union"	"bigcup"	"|~~|\n|  |\n|  |")
   ("CAP"	"bigcap"	"|  |\n|  |\n|__|")
   ("isc"	"bigcap"	"|  |\n|  |\n|__|")
   ("O+"	"bigoplus"	"/~~~\\\n| + |\n\\___/")
   ("Ox"	"bigotimes"	"/~~~\\\n| X |\n\\___/")
   ;;other marks
   ("angle"	"angle"		("/\n~" "∠"))
   ("/_"	"angle"		("/\n~" "∠"))
   ("Z"		"aleph"		"|\\|")
   ("|\\|"	"aleph"		"|\\|")
   ("h-"	"hbar"		"_\nh")
 ;  ("i"		"imath"		"i") ;These chars are appeared only
 ;  ("j"		"jmath"		"j") ;as section-type arguments
   ("l"		"ell"		"l")
   ("wp"	"wp"		"???")
   ("R"		"Re"		")R")
   ("Im"	"Im"		"???")
   ("mho"	"mho"		"~|_|~")
   ("'"		"prime"		"'")
   ("0"		"emptyset"	"0")
   ("nabla"	"nabla"		("___\n\\\\/" YaTeX-image-nabla))
   ("\\/"	"surd"		"-\\/")
   ("surd"	"surd"		"-\\/")
   ("top"	"top"		"T")
   ("bot"	"bot"		("_|_"		YaTeX-image-bot))
   ("b"		"flat"		("b"		YaTeX-image-flat))
   ("LT"	"natural"	"|\nLT\n |")
   ("6"		"partial"	(" -+\n+-+\n+-+" YaTeX-image-partial))
   ("partial"	"partial"	(" -+\n+-+\n+-+" YaTeX-image-partial))
   ("round"	"partial"	(" -+\n+-+\n+-+" YaTeX-image-partial))
   ("[]"	"Box"		"[]")
   ("no"	"notag"		"\\notag")
   (":"		"colon"		":")
   ("Diamond"	"Diamond"	"/\\\n\\/")
   ("3"		"triangle"	"/\\\n~~")
   ("C"		"clubsuit"	" o\no+o\n |")
   ("D"		"diamondsuit"	"/\\\n\\/")
   ("H"		"heartsuit"	"<^^>\n \\/")
   ("S"		"spadesuit"	" /\\\n<++>\n /\\")
   )
 "Default LaTeX-math-command alist.")

(defvar YaTeX-math-sign-alist-private nil
  "*User definable key vs LaTeX-math-command alist.")

(defvar YaTeX-math-quit-with-strict-match nil
  "*T for quitting completion as soon as strict-match is found.")
(defvar YaTeX-math-sign-alist
      (append YaTeX-math-sign-alist-private YaTeX-math-sign-alist-default))

;;(defun YaTeX-math-alist2array (alist array)
;;  (set array
;;       (let ((array (make-vector (length alist) "")) (list alist) (i 0))
;;	 (while list
;;	   (aset array i (car (car list)))
;;	   (setq i (1+ i) list (cdr list)))
;;	 array))
;;)

(defvar YaTeX-greek-key-alist-default
  '(
    ("a"	"alpha"		("a" "α"))
    ("b"	"beta"		("|>\n|>\n|" "β"))
    ("g"	"gamma"		("~r" "γ"))
    ("G"	"Gamma"		("|~" "Γ"))
    ("d"	"delta"		("<~\n<>" "δ"))
    ("D"	"Delta"		("/\\\n~~" "Δ"))
    ("e"	"epsilon"	"<\n<~")
    ("e-"	"varepsilon"	("(\n(~" "ε"))
    ("z"	"zeta"		("(~\n >" "ζ"))
    ("et"	"eta"		("n\n/" "η"))
    ("th"	"theta"		("8" "θ"))
    ("Th"	"Theta"		("(8)" "Θ"))
    ("th-"	"vartheta"	("-8" "-θ"))
    ("i"	"iota"		("i\n\\_/" "ι"))
    ("k"	"kappa"		("k" "κ"))
    ("l"	"lambda"	("\\n/\\" "λ"))
    ("L"	"Lambda"	("/\\" "Λ"))
    ("m"	"mu"		(" u_\n/" "μ"))
    ("n"	"nu"		("|/" "ν"))
    ("x"	"xi"		("E\n >" "ξ"))
    ("X"	"Xi"		("---\n -\n---" "Ξ"))
    ("p"	"pi"		("__\n)(" "π"))
    ("P"	"Pi"		("__\n||" "Π"))
    ("p-"	"varpi"		("_\nw" "__\nω"))
    ("r"	"rho"		("/O" "ρ"))
    ("r-"	"varrho"	("/O\n~~" "ρ\n~~"))
    ("s"	"sigma"		("o~" "σ"))
    ("S"	"Sigma"		("\\-+\n >\n/-+" "Σ"))
    ("s-"	"varsigma"	"(~~ \n>")
    ("t"	"tau"		("__\n(" "τ"))
    ("u"	"upsilon"	("~v" "υ"))
    ("y"	"upsilon"	("~v" "υ"))
    ("U"	"Upsilon"	("~Y~" "Υ"))
    ("Y"	"Upsilon"	("~Y~" "Υ"))
    ("ph"	"phi"		("  /\n(/)\n/" "φ"))
    ("Ph"	"Phi"		(" _\n(|)\n ~" "Φ"))
    ("ph-"	"varphi"	"\\O\n|")
    ("c"	"chi"		("x" "χ"))
    ("ps"	"psi"		("\\|/\\n |" "ψ"))
    ("Ps"	"Psi"		(" ~\n\\|/\\n |" "Ψ"))
    ("o"	"omega"		("w" "ω"))
    ("w"	"omega"		("w" "ω"))
    ("O"	"Omega"		("(~)\n~ ~" "Ω"))
    ("W"	"Omega"		("(~)\n~ ~" "Ω"))
    ("f" "foo")
    )
  "Default LaTeX-math-command alist.")

(defvar YaTeX-greek-key-alist-private nil
  "*User definable key vs LaTeX-math-command alist.")

(defvar YaTeX-greek-key-alist
      (append YaTeX-greek-key-alist-private YaTeX-greek-key-alist-default))

;;(mapcar (function (lambda (x) (YaTeX-math-alist2array x)))
;;	YaTeX-math-key-list)

(defvar YaTeX-math-indicator "KEY\tLaTeX sequence\t\tsign")

(defvar YaTeX-math-need-image t
  "*T for displaying pseudo image momentarily.")
(defvar YaTeX-math-max-key 8)
(defvar YaTeX-math-max-seq
  (* 8 (1+ (/ (length "\\longleftrightarrow") 8))))
(defvar YaTeX-math-max-sign 5)
(defvar YaTeX-math-sign-width
  (+ YaTeX-math-max-key YaTeX-math-max-seq YaTeX-math-max-sign))
(defvar YaTeX-math-display-width
  (* 8 (1+ (/ YaTeX-math-sign-width 8))))
(defvar YaTeX-math-menu-map nil
  "Keymap used in YaTeX mathematical sign menu mode.")

(if YaTeX-math-menu-map nil
  (setq YaTeX-math-menu-map (make-sparse-keymap))
  (define-key YaTeX-math-menu-map "n"	'next-line)
  (define-key YaTeX-math-menu-map "p"	'previous-line)
  (define-key YaTeX-math-menu-map "f"	'YaTeX-math-forward)
  (define-key YaTeX-math-menu-map "b"	'YaTeX-math-backward)
  (define-key YaTeX-math-menu-map "v"	'scroll-up)
  (define-key YaTeX-math-menu-map " "	'scroll-up)
  (define-key YaTeX-math-menu-map "c"	'scroll-up)
  (define-key YaTeX-math-menu-map "V"	'scroll-down)
  (define-key YaTeX-math-menu-map "r"	'scroll-down)
  (define-key YaTeX-math-menu-map "\^h"	'scroll-down)
  (define-key YaTeX-math-menu-map "<"	'beginning-of-buffer)
  (define-key YaTeX-math-menu-map ">"	'end-of-buffer)
  (define-key YaTeX-math-menu-map "\^m"	'exit-recursive-edit)
  (define-key YaTeX-math-menu-map "q"	'abort-recursive-edit))

(defvar YaTeX-math-exit-key "\e"
  "*Key sequence after prefix key of YaTeX-math-mode to exit from math-mode.")

(defmacro YaTeX-math-japanese-sign (list)
  (list 'nth 1 list))

(defvar YaTeX-math-cmd-regexp (concat (regexp-quote YaTeX-ec) "[A-z|]"))
(defvar YaTeX-math-verbatim-environments
  '("alltt")
  "*List of environments in which LaTeX math mode is disabled.
This value is appended with YaTeX-verbatim-environments.")

;;;
;;YaTeX math-mode functions
;;;
;;;###autoload from yatex.el
(defun YaTeX-toggle-math-mode (&optional arg)
  (interactive "P")
  (or (memq 'YaTeX-math-mode mode-line-format) nil
      (setq mode-line-format
	    (append (list "" 'YaTeX-math-mode) mode-line-format)))
  (if YaTeX-auto-math-mode nil		;Only effective on manual mode.
    (if (or arg (null YaTeX-math-mode))
	(let (keys)
	  (setq YaTeX-math-mode "math:")
	  (message "Turn on math mode. Prefix keys are %s"
		   (mapconcat 'car YaTeX-math-key-list " "))
	  (sit-for 3)
	  (message
	   (concat "To exit from math-mode, type `ESC' after prefix, "
		   "or type `"
		   (key-description
		    (car
		     (where-is-internal
		      'YaTeX-switch-mode-menu YaTeX-mode-map)))
		   " $'")))
      (setq YaTeX-math-mode nil)
      (message "Exit from math mode."))
    (set-buffer-modified-p (buffer-modified-p))))

(defun YaTeX-math-forward (arg)
  (interactive "p")
  (re-search-forward YaTeX-math-cmd-regexp nil t arg))

(defun YaTeX-math-backward (arg)
  (interactive "p")
  (re-search-backward YaTeX-math-cmd-regexp nil t arg))

(defun YaTeX-math-gets (sign)
  (cond
   ((null sign) nil)
   ((listp sign)
    (setq sign
	  (cond
	   (YaTeX-japan (YaTeX-math-japanese-sign sign))
	   (t (car sign))))
    (YaTeX-math-gets sign))
   ((symbolp sign)
    (YaTeX-math-gets (symbol-value sign)))
   (t sign)))

(defun YaTeX-math-get-sign (list)
  (YaTeX-math-gets (car (cdr-safe (cdr-safe list)))))

(defvar YaTeX-math-section-type-regexp
  "eqn\\\\\\sw+\\b"
  "*Regexp of section-type math-mode macro")

(defun YaTeX-in-math-mode-p ()
  "If current position is supposed to be in LaTeX-math-mode, return t.
This function refers a local variable `source-window' in YaTeX-make-section."
  (save-excursion
    (and (boundp 'source-window) source-window
	 (set-buffer (window-buffer source-window)))
    (or (YaTeX-quick-in-environment-p
	 (append
	  '("math" "eqnarray" "equation" "eqnarray*" "displaymath") ;LaTeX
	  (if YaTeX-use-AMS-LaTeX
	      ;; And math modes of AMS-LaTeX
	      ;;'("align" "align*" "split" "multline" "multline*" "gather"
	      ;;  "gather*" "aligned*" "gathered" "gathered*" "alignat"
	      ;;  "equation*" "cases" "flalign" "flalign*"
	      ;;  "alignat*" "xalignat" "xalignat*" "xxalignat" "xxalignat*"
	      YaTeX-math-begin-list
	    )))
	(let*((p (point)) (nest 0) me0 r
	      (delim (concat YaTeX-sectioning-regexp "\\|^$\\|^\C-l"))
	      (boundary
	       (save-excursion
		 (if (looking-at delim)
		     (goto-char (max (point-min) (1- (point)))))
		 (re-search-backward delim nil 1)
		 (point))))
	  (save-excursion
	    (cond
	     ((catch 'open
		(save-excursion
		  (while (and (>= nest 0)
			      (re-search-backward
			       (concat YaTeX-ec-regexp ;\
				       "\\([()]\\|[][]\\)") boundary t))
		    (setq me0 (match-end 0))
		    (if (or (YaTeX-on-comment-p)
			    (YaTeX-literal-p)) nil
		      (if (or (= (char-after (1- me0)) ?\))
			      (= (char-after (1- me0)) ?\]))
			  (setq nest (1+ nest))
			(if (= (preceding-char) ?\\ ) nil ;;\\[5pt]
			  (setq nest (1- nest))))))
		  (if (< nest 0) (throw 'open t))))
	      t)
	     ((and (setq r (YaTeX-on-section-command-p
			    YaTeX-math-section-type-regexp))
		   (numberp r)
		   (> r 0))
	      t)
	     (t (catch 'dollar
		  (while ;(search-backward "$" boundary t);little bit fast.
		      (YaTeX-re-search-active-backward ;;;;;; Too slow???
		       "\\$" (concat "[^\\\\]" YaTeX-comment-prefix) boundary t)
		    (cond
		     ((equal (char-after (1- (point))) ?$) ; $$ equation $$
		      (backward-char 1)
		      (setq nest (1+ nest)))
		     ((let ((YaTeX-verbatim-environments
			     (append YaTeX-math-verbatim-environments
				     YaTeX-verbatim-environments)))
			(YaTeX-literal-p))
		      nil)
		     ((and (equal (char-after (1- (point))) ?\\ )
			   (not (equal (char-after (- (point) 3)) ?\\ )))
		      nil)		;\$
		     (t (setq nest (1+ nest)))))
		  (if (= (% nest 2) 1) (throw 'dollar t))))))))))

(defun YaTeX-math-display-list (list cols)
  (goto-char (point-max))
  (if (= cols 0) (if (not (eolp)) (newline 1))
    (forward-line -1)
    (while (looking-at "[ \t\n]") (forward-line -1)))
  (end-of-line)
  (let ((indent (* YaTeX-math-display-width cols)) sign str to)
    (indent-to indent)
    (insert (car list))
    (indent-to (setq indent (+ indent YaTeX-math-max-key)))
    (insert "\\" (car (cdr list)))
    (setq indent (+ indent YaTeX-math-max-seq))
    (setq sign (YaTeX-math-get-sign list))
    (while (and sign (not (string= "" sign)))
      (setq to (string-match "\n" sign)
	    str (if to (substring sign 0 to) sign))
      (end-of-line)
      (indent-to indent)
      (insert str)
      (cond ((eobp) (newline 1))
	    ((> cols 0) (forward-line 1)))
      (setq sign (if to (substring sign (1+ to)) "")))))

(defvar YaTeX-math-menu-buffer "*math-mode-signs*")

(defun YaTeX-math-show-menu (match-str)
  (save-window-excursion
    (YaTeX-showup-buffer YaTeX-math-menu-buffer nil t)
    (let ((maxcols (max 1 (/ (YaTeX-screen-width) YaTeX-math-sign-width)))
	  (case-fold-search nil)
	  (cols 0) (list alist) command)
      (erase-buffer)
      (insert 
       "Candidates of sign. [n:next p:prev f:forw b:back q:quit RET:select]\n")
      (insert YaTeX-math-indicator "\t")
      (insert YaTeX-math-indicator)
      (newline 1)
      (insert-char ?- (1- (YaTeX-screen-width)))
      (newline 1)
      (while list
	(if (string-match match-str (car (car list)))
	    (progn (YaTeX-math-display-list (car list) cols)
		   (setq cols (% (1+ cols) maxcols))))
	(setq list (cdr list)))
      (goto-line 4)
      (use-local-map YaTeX-math-menu-map)
      (setq buffer-read-only t)
      (unwind-protect
	  (recursive-edit)
	(skip-chars-backward "^ \t\n")
	(setq command
	      (if (re-search-forward YaTeX-math-cmd-regexp nil t)
		  (buffer-substring
		   (match-beginning 0)
		   (prog2 (skip-chars-forward "^ \t\n") (point)))
		nil))
	(kill-buffer YaTeX-math-menu-buffer))
      command)))

;
(defun YaTeX-math-show-image (image &optional exit-char)
  "Momentarily display IMAGE at the beginning of the next line;
erase it on the next keystroke.  The window is recentered if necessary
to make the whole string visible.  If the window isn't large enough,
at least you get to read the beginning."
  (if (and image (not (string= image "")))
      (let ((buffer-read-only nil)
	    (modified (buffer-modified-p))
	    (name buffer-file-name)
	    insert-start
	    insert-end)
	(unwind-protect
	    (progn
	      (save-excursion
		;; defeat file locking... don't try this at home, kids!
		(setq buffer-file-name nil)
		(forward-line 1)
		(setq insert-start (point))
		(if (eobp) (newline))
		(insert image)
		(setq insert-end (point)))
					; make sure the whole string is visible
	      (if (not (pos-visible-in-window-p insert-end))
		  (recenter (max 0
				 (- (window-height)
				    (count-lines insert-start insert-end)
				    2))))
	      (let ((char (read-char)))
		(or (eq char exit-char)
		    (setq unread-command-char char))))
	  (if insert-end
	      (save-excursion
		(delete-region insert-start insert-end)))
	  (setq buffer-file-name name)
	  (set-buffer-modified-p modified)))))

(defun YaTeX-math-insert-sequence (&optional force initial)
  "Insert math-mode sequence with image completion."
  (interactive "P")
  (let*((key (or initial "")) regkey str  last-char list i
	(case-fold-search nil) match sign
	(this-key (char-to-string last-command-char))
	(alistsym (cdr (assoc this-key YaTeX-math-key-list)))
	(alistname (symbol-name alistsym))
	(alist (symbol-value alistsym))
	(n (length alist)) (beg (point)) result)
    (if initial (insert YaTeX-ec (car (cdr (assoc initial alist)))))
    (if (string-match "^YaTeX-" alistname)
	(setq alistname (substring alistname (length "YaTeX-"))))
    (setq alistname (substring alistname 0 (string-match "-" alistname)))
    (setq result
	  (catch 'complete
	    (if (and (not force)
		     (if YaTeX-auto-math-mode
			 (not (YaTeX-in-math-mode-p))
		       (not YaTeX-math-mode)))
		(throw 'complete 'escape));this tag should be exit, but...
	    (while t
	      (message "%ssequence%s: %s"
		       (if YaTeX-simple-messages "" (concat alistname " "))
		       (if YaTeX-simple-messages "" "(TAB for menu)") key)
	      (setq last-char (read-char)
		    key (concat key (char-to-string last-char))
		    i 0)
	      (cond
	       ((string= key this-key)	;;invoke key itself
		(throw 'complete 'escape))
	       ((string= key YaTeX-math-exit-key)	;;exit from math-mode
		(throw 'complete 'exit))
	       ((string-match "\r" key)			;;RET = kakutei
		(throw 'complete 'select))
	       ((string-match "[\C-g\C-c]" key)		;;C-g = abort
		(throw 'complete 'abort))
	       ((string-match "[\t\n]" key)		;;TAB, LFD = menu
		(throw 'complete 'menu))
	       ((string-match "[\C-h\C-?]" key)		;;BS, DEL = BS
		(if (< (length key) 2) (throw 'complete 'abort))
		(setq key (substring key 0 -2))))
	      
	      (setq regkey (concat "^" (regexp-quote key)))
	      (message "Sequence%s: %s"
		       (if YaTeX-simple-messages "" "(TAB for menu)") key)
	      (if
		  (catch 'found
		    ;;(1)input string strictly matches with alist
		    (setq match (assoc key alist))
		    ;;remember previous match

		    ;;(2)search partial match into alist
		    (setq list alist)
		    (while (< i n)
		      (if (string-match
			   regkey
			   ;;(aref array i)
			   ;;(car (nth i alist))
			   (car (car list)))
			  (progn
			    (or match
				;;(setq match (nth i alist))
				(setq match (car list)))
			    (throw 'found t)))
		      (setq i (1+ i) list (cdr list))))	;catch 'found
		  nil ;;if any match, continue reading
		;;else reading of sequence has been done.
		(message "complete.")
		(throw 'complete t))

	      (if match
		  (progn (delete-region beg (point))
			 (setq YaTeX-single-command (car (cdr match)))
			 (insert YaTeX-ec YaTeX-single-command)
			 (if (and YaTeX-math-need-image
				  (setq sign (YaTeX-math-get-sign match)))
			     (YaTeX-math-show-image (concat sign "\n")))
			 )
		nil)
	      )))
    (delete-region beg (point))
    (cond
     ((memq result '(t select))
      (if (eq result t)
	  (setq unread-command-char last-char)
	(message "Done."))
      (if (assoc YaTeX-single-command section-table)
	  (YaTeX-make-section nil nil nil YaTeX-single-command)
	(setq YaTeX-current-completion-type 'maketitle)
	(YaTeX-make-singlecmd YaTeX-single-command)))
     ((eq result 'abort)
      (message "Abort."))
     ((eq result 'escape)
      (call-interactively (lookup-key global-map this-key)))
     ((eq result 'exit)
      (YaTeX-toggle-math-mode))
     ((eq result 'menu)
      (setq key (concat "^" (regexp-quote (substring key 0 -1))))
      (insert (YaTeX-math-show-menu key))))))

;; ----- Change image completion types -----
(defun YaTeX-math-member-p (item)
  "Check if ITEM is a member of image completion.
If so return the cons of its invocation key and image-string."
  (let ((lists YaTeX-math-key-list) list)
    (catch 'found
      (while lists
	(setq list (symbol-value (cdr (car lists))))
	(while list
	  (if (string= item (nth 1 (car list)))
	      (throw 'found (cons (car (car lists)) (nth 0 (car list)))))
	  (setq list (cdr list)))
	(setq lists (cdr lists))))))

;;; ----- for AMS LaTeX (by matsu@math.s.chiba-u.ac.jp) -----
(defvar YaTeX-ams-paren-modifier
  '(("Biggl" . "Biggr") ("biggl" . "biggr")
    ("Bigl" . "Bigr") ("bigl" . "bigr")
    ("left" . "right") ("" . ""))
  "Alist of modifier of parentheses.")

(defvar YaTeX-left-paren "(\\|\\[\\|\\\\{")
(defvar YaTeX-right-paren ")\\|\\]\\|\\\\}")
(defvar YaTeX-paren
  (concat YaTeX-left-paren "\\|" YaTeX-right-paren))

(defun YaTeX-on-parenthesis-p ()
  "If cursor is on an (AMS-LaTeX) parenthesis, return the parenthesis."
  (interactive)
  (let* ((list YaTeX-ams-paren-modifier)
	 (longest 0) ;; the longest length of parenthesis command strings
	 (flag t) ;; flag for whether on braces not following \
	 (point (point))
	 (move 0)
	 (paren))
    (while list
      (setq longest
	    (max longest (length (car (car list))) (length (cdr (car list)))))
      (setq list (cdr list)))
    (save-excursion
      ;; search {} and, if it does not follow `\', set flag nil.
      ;; if it is right after `\', set flag t and move to the position of \.
      ;; mmmmm.
      (if (looking-at "{\\|}")
	  (if (not (equal (char-after (1- (point))) 92))
	      (setq flag nil)
	    (forward-char -1)))
      ;; if flag is nil, do nothing.
      (if (and flag (re-search-forward YaTeX-paren
				       (+ (point) 3 longest) t))
	  (progn
	    (setq move (- (point) point))
	    (setq paren (YaTeX-match-string 0))
	    (setq list YaTeX-ams-paren-modifier)
	    ;; criterion for whether on [] () \{\} or not.
	    (if (string-match YaTeX-left-paren paren)
		(while (and list flag)
		  (let* ((mod (car (car list)))
			 (mod-length 0) ;; length of modifier
			 paren-regexp ;; regexp of paren.
			 mod-regexp) ;; regexp of modifier.
		    (if (> (length mod) 0)
			(setq mod-regexp (concat "\\\\" mod)
			      mod-length (1+ (length mod))))
		    (cond ((string= paren "\\{")
			   (setq paren-regexp (concat "\\" paren)))
			  ((string= paren "[")
			   (setq paren-regexp "\\["))
			  (t (setq paren-regexp paren)))
		    (save-excursion
		      (if (and (>= (- (point) (point-min))
				   (+ mod-length (length paren)))
			       (not (forward-char
				     (- 0 mod-length (length paren))))
			       (looking-at (concat "\\(" mod-regexp "\\)\\("
						   paren-regexp "\\)")))
			  (setq flag nil)))
		    (setq list (cdr list))))
	      (while (and list flag)
		(let* ((mod (cdr (car list)))
		       (mod-length 0)
		       paren-regexp
		       mod-regexp)
		  (if (> (length mod) 0)
		      (setq mod-regexp (concat "\\\\" mod)
			    mod-length (1+ (length mod))))
		  (cond ((string= paren "\\}")
			 (setq paren-regexp (concat "\\" paren)))
			((string= paren "]")
			 (setq paren-regexp "\\]"))
			(t (setq paren-regexp paren)))
		  (save-excursion
		    (if (and (>= (- (point) (point-min))
				 (+ mod-length (length paren)))
			     (not (forward-char
				   (- 0 mod-length (length paren))))
			     (looking-at (concat "\\(" mod-regexp "\\)\\("
						 paren-regexp "\\)")))
			(setq flag nil)))
		  (setq list (cdr list)))))
	    (if (<= move (length (YaTeX-match-string 0)))
		(YaTeX-match-string 0)))))))

(defun YaTeX-goto-open-paren (&optional jumpto-co)
  "Jump to the exact position of open parenthesis.
If optional argument JUMPTO-CO is non-nil, goto corresponding parentheses."
  (interactive)
  (let ((paren)
	(backslash-syntax (char-to-string (char-syntax ?\\))))
    (if (setq paren (YaTeX-on-parenthesis-p))
	(if (string-match "(\\|{\\|\\[" paren (1- (length paren)))
	    (progn
	      (re-search-forward "(\\|{\\|\\[" (+ (point) (length paren)) t)
	      (backward-char)
	      (if jumpto-co
		  (unwind-protect
		      (progn
			(modify-syntax-entry ?\\ " ")
			(forward-list)
			(backward-char))
		    (modify-syntax-entry ?\\ backslash-syntax)))
	      (point))
	  (re-search-forward ")\\|}\\|\\]" (+ (point) (length paren)) t)
	  (unwind-protect
	      (progn
		(modify-syntax-entry ?\\ " ")
		(backward-list)
		(point))
	    (modify-syntax-entry ?\\ backslash-syntax))))))

;;;###autoload
(defun YaTeX-goto-corresponding-paren ()
  "Go to corresponding mathematical parentheses."
  (if (YaTeX-on-parenthesis-p)
      (YaTeX-goto-open-paren t)
    nil))

(defun YaTeX-change-parentheses ()
  "Change the size of parentheses, braces, and brackets of AMS-LaTeX."
  (interactive)
  (if (not (and YaTeX-use-AMS-LaTeX (YaTeX-on-parenthesis-p)))
      nil
    (let* ((mod (YaTeX-match-string 1)) ;; modifier
	   (paren (if mod (YaTeX-match-string 2) (YaTeX-match-string 0))) ;; paren
	   (mod-length (if (or (string= mod "\\left") (string= mod "\\right"))
			   5            ;; 5 in case left or right
			 (length mod))) ;; length of modifier
	   (paren-length (length paren)) ;; length of paren
	   (length (+ mod-length paren-length)) ;; length of whole string
	   (big-p t) ;; flag whether new modifier is "[Bb]ig+" or not.
	   size ;; left, big, Big etc.
	   type ;; parentheses type
	   lr   ;; "l" or "r".
	   char newsize newsize-length
	   (backslash-syntax (char-to-string (char-syntax ?\\)))
	   (case-fold-search))
      ;; decide lr and size from mod and paren.
      (cond ((string-match "\\(\\\\[Bb]ig+\\)[lr]" mod)
	     (setq size (substring mod 1 (match-end 1))
		   lr (substring mod (match-end 1) (match-end 0))))
	    ((string-match "\\\\left" mod)
	     (setq size "left-right" lr "l"))
	    ((string-match "\\\\right" mod)
	     (setq size "left-right" lr "r"))
	    ((string-match "(\\|\\[\\|\\\\{" paren)
	     (setq size "null" lr "l"))
	    ((string-match ")\\|\\]\\|\\\\}" paren)
	     (setq size "null" lr "r"))
	    (t
	     (setq size nil lr nil)))
      (while (not newsize)
	(message (format (concat "Change from %s: "
				 "l(big) L(Big) h(bigg) H(Bigg) "
				 "r(left-right) n(NONE) ( { [") size))
	(setq char (read-char)
	      newsize (cond ((char-equal char ?l) "\\big")
			    ((char-equal char ?L) "\\Big")
			    ((char-equal char ?h) "\\bigg")
			    ((char-equal char ?H) "\\Bigg")
			    ((char-equal char ?r)
			     (setq big-p nil) "\\left")
			    ((memq char '(?\( ?\)))
			     (setq big-p nil type '("(" . ")")) "")
			    ((memq char '(?\{ ?\}))
			     (setq big-p nil type '("\\{" . "\\}")) "")
			    ((memq char '(?\[ ?\]))
			     (setq big-p nil type '("[" . "]")) "")
			    ((char-equal char ?n)
			     (setq big-p nil) "")
			    (t nil))
	      newsize-length (length newsize)))
      (YaTeX-goto-open-paren)
      (forward-char)
      (cond
       (type
	(delete-region (point) (- (point) paren-length))
	(save-excursion (insert (car type))))
       (t
	(delete-region (- (point) length) (- (point) paren-length))
	(backward-char paren-length)))
      (insert-string newsize)
      (if big-p (insert ?l))
      (unwind-protect
	  (progn
	    (modify-syntax-entry ?\\ " ")
	    (forward-list)
	    (if (string= size "left-right") (setq length (1+ length)))
	    (if (eq char ?r) (setq newsize "\\right"))
	    (cond
	     (type
	      (delete-region (point) (- (point) paren-length))
	      (insert (cdr type)))
	     (t
	      (delete-region (- (point) length) (- (point) paren-length))
	      (backward-char paren-length)
	      (insert-string newsize)
	      (if big-p (insert ?r))
	      (forward-char paren-length)))
	    (if (string= lr "l") (backward-list)))
	(modify-syntax-entry ?\\ backslash-syntax))
      t)))

(defun YaTeX-insert-amsparens-region (beg end char)
  (interactive "r\ncWhich one ? l(big) L(Big) h(bigg) H(Bigg): ")
  (let* ((case-fold-search)
	 (st (cond ((char-equal char ?l) "big")
		   ((char-equal char ?L) "Big")
		   ((char-equal char ?h) "bigg")
		   ((char-equal char ?H) "Bigg"))))
    (if st
	(YaTeX-insert-braces-region
	 beg end (concat "\\" st "l(") (concat "\\" st "r)"))
      (YaTeX-insert-braces-region beg end "(" ")"))))

(defun YaTeX-insert-amsbraces-region (beg end char)
  (interactive "r\ncWhich one ? l(big) L(Big) h(bigg) H(Bigg): ")
  (let* ((case-fold-search)
	 (st (cond ((char-equal char ?l) "big")
		   ((char-equal char ?L) "Big")
		   ((char-equal char ?h) "bigg")
		   ((char-equal char ?H) "Bigg"))))
    (if st
	(YaTeX-insert-braces-region
	 beg end (concat "\\" st "l\\{") (concat "\\" st "r\\}"))
      (YaTeX-insert-braces-region beg end "\\{" "\\}"))))

(defun YaTeX-insert-amsbrackets-region (beg end char)
  (interactive "r\ncWhich one ? l(big) L(Big) h(bigg) H(Bigg): ")
  (let* ((case-fold-search)
	 (st (cond ((char-equal char ?l) "big")
		   ((char-equal char ?L) "Big")
		   ((char-equal char ?h) "bigg")
		   ((char-equal char ?H) "Bigg"))))
    (if st
	(YaTeX-insert-braces-region
	 beg end (concat "\\" st "l[") (concat "\\" st "r]"))
      (YaTeX-insert-braces-region beg end "[" "]"))))


;;
(provide 'yatexmth)

; Local variables: 
; fill-prefix: ";;;	" 
; paragraph-start: "^$\\|\\|;;;$" 
; paragraph-separate: "^$\\|\\|;;;$" 
; End: 
