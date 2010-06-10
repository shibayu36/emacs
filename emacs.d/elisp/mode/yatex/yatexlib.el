;;; -*- Emacs-Lisp -*-
;;; YaTeX and yahtml common libraries, general functions and definitions
;;; yatexlib.el
;;; (c)1994-2009 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Mon Sep 28 10:46:39 2009 on firestorm
;;; $Id: yatexlib.el,v 1.74 2009/09/28 01:54:43 yuuji Rel $

;; General variables
(defvar YaTeX-dos (memq system-type '(ms-dos windows-nt OS/2)))
(defvar YaTeX-macos (memq system-type '(darwin)))
(defvar YaTeX-emacs-19 (>= (string-to-int emacs-version) 19))
(defvar YaTeX-emacs-20 (>= (string-to-int emacs-version) 20))
(defvar YaTeX-emacs-21 (>= (string-to-int emacs-version) 21))
(defvar YaTeX-user-completion-table
  (if YaTeX-dos "~/_yatexrc" "~/.yatexrc")
  "*Default filename in which user completion table is saved.")

(defvar YaTeX-display-color-p
  (or (and (fboundp 'display-color-p) (display-color-p))
      (and (fboundp 'device-class)
	   (eq 'color (device-class (selected-device))))
      window-system)  ; falls down lazy check..
  "Current display's capability of expressing colors.")

(defvar YaTeX-japan
  (or (boundp 'NEMACS)
      (boundp 'MULE)
      (and (boundp 'current-language-environment)
	   (string-match "[Jj]apanese" current-language-environment)))
  "Whether yatex mode is running on Japanese environment or not.")

;; autoload from yahtml.el
(autoload 'yahtml-inner-environment-but "yahtml" "yahtml internal func." t)

(defvar latex-message-kanji-code 2
  "*Kanji coding system latex command types out.
1 = Shift JIS, 2 = JIS, 3 = EUC. 4 = UTF-8")

(defvar YaTeX-kanji-code-alist
  (cond
   ((boundp '*junet*)
    (list '(0 . *noconv*)
	  (cons
	   1
	   (cond
	    (YaTeX-dos (if (boundp '*sjis-dos*) *sjis-dos* *sjis*dos))
	    (YaTeX-macos (if (boundp '*sjis-mac*) *sjis-mac* *sjis*mac))
	    (t *sjis*)))
	  '(2 . *junet*) '(3 . *euc-japan*)))
   ((and YaTeX-emacs-20 (featurep 'mule))
    ;;(cdr-safe(assq 'coding-system (assoc "Japanese" language-info-alist)))
    (list '(0 . no-conversion)
	  (cons
	   1 (cond (YaTeX-dos 'shift_jis-dos)
		   (YaTeX-macos 'shift_jis-mac)
		   ((member 'shift_jis (coding-system-list)) 'shift_jis-unix)
		   (t 'sjis)))
	  '(2 . iso-2022-jp-unix)
	  '(3 . euc-jp-unix)
	  '(4 . utf-8))))
  "Kanji-code expression translation table.")
(defvar YaTeX-inhibit-prefix-letter nil
  "*T for changing key definitions from [prefix] Letter to [prefix] C-Letter.")

(defvar YaTeX-no-begend-shortcut nil
  "*T for disabling shortcut of begin-type completion, [prefix] b d, etc.")

(defvar YaTeX-default-pop-window-height 10
  "Default typesetting buffer height.
If integer, sets the window-height of typesetting buffer.
If string, sets the percentage of it.
If nil, use default pop-to-buffer.")

(defvar YaTeX-create-file-prefix-g nil
  "*Non-nil creates new file when [prefix] g on \\include{foo}.")

(defvar YaTeX-nervous t
  "*If you are nervous about maintenance of yatexrc, set this value to T.
And you will have the local dictionary.")

(defvar YaTeX-use-italic-bold (string< "20" emacs-version)
  "*Non-nil tries to find italic/bold fontset.
This variable is effective when font-lock is used.
\it, \bf 内部での日本語が□になってしまう場合はこれをnilにして下さい。")

;----------- work variables ----------------------------------------
(defvar YaTeX-minibuffer-completion-map nil
  "Minibuffer completion key map that allows comma completion.")
(if YaTeX-minibuffer-completion-map nil
  (setq YaTeX-minibuffer-completion-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key YaTeX-minibuffer-completion-map " "
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-minibuffer-completion-map "\t"
    'YaTeX-minibuffer-complete))

(defvar YaTeX-typesetting-mode-map nil
  "Keymap used in YaTeX typesetting buffer")

(if YaTeX-typesetting-mode-map nil
  (setq YaTeX-typesetting-mode-map (make-keymap))
  ;(suppress-keymap YaTeX-typesetting-mode-map t)
  (define-key YaTeX-typesetting-mode-map " " 'YaTeX-jump-error-line)
  (define-key YaTeX-typesetting-mode-map "\C-m" 'YaTeX-send-string)
  (define-key YaTeX-typesetting-mode-map "1" 'delete-other-windows)
  (define-key YaTeX-typesetting-mode-map "0" 'delete-window)
  (define-key YaTeX-typesetting-mode-map "q" 'delete-window))

(defvar YaTeX-parent-file nil
  "*Main LaTeX source file name used when %#! expression doesn't exist.")
(make-variable-buffer-local 'YaTeX-parent-file)

;---------- Define default key bindings on YaTeX mode map ----------
;;;###autoload
(defun YaTeX-define-key (key binding &optional map)
  "Define key on YaTeX-prefix-map."
  (if YaTeX-inhibit-prefix-letter
      (let ((c (aref key 0)))
	(cond
	 ((and (>= c ?a) (<= c ?z)) (aset key 0 (1+ (- c ?a))))
	 ((and (>= c ?A) (<= c ?Z) (numberp YaTeX-inhibit-prefix-letter))
	  (aset key 0 (1+ (- c ?A))))
	 (t nil))))
  (define-key (or map YaTeX-prefix-map) key binding))

;;;###autoload
(defun YaTeX-local-table-symbol (symbol)
  "Return the lisp symbol which keeps local completion table of SYMBOL."
  (intern (concat "YaTeX$"
		  default-directory
		  (symbol-name symbol))))

;;;###autoload
(defun YaTeX-sync-local-table (symbol)
  "Synchronize local variable SYMBOL.
Copy its corresponding directory dependent completion table to SYMBOL."
  (if (boundp (YaTeX-local-table-symbol symbol))
      (set symbol (symbol-value (YaTeX-local-table-symbol symbol)))))

(defvar YaTeX-user-table-is-read nil
  "Flag that means whether user completion table has been read or not.")
;;;###autoload
(defun YaTeX-read-user-completion-table (&optional forcetoread)
  "Append user completion table of LaTeX macros"
  (interactive)
  (let*((user-table (expand-file-name YaTeX-user-completion-table))
	(local-table (expand-file-name (file-name-nondirectory user-table)))
	var localvar localbuf (curbuf (current-buffer)) sexp)
    (if YaTeX-user-table-is-read nil
      (message "Loading user completion table")
      (if (file-exists-p user-table) (load-file user-table)
	(message "Welcome to the field of YaTeX.  I'm glad to see you!")))
    (setq YaTeX-user-table-is-read t)
    (cond
     ((file-exists-p local-table)
      (set-buffer (setq localbuf (find-file-noselect local-table)))
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "(setq \\([^ \t\n]+\\)" nil t)
	(setq var (intern (buffer-substring
			   (match-beginning 1) (match-end 1)))
	      localvar (YaTeX-local-table-symbol var))
	(goto-char (match-beginning 0))
	(setq sexp (buffer-substring (point)
				     (progn (forward-sexp) (point))))
	(set-buffer curbuf)
	(or (assq var (buffer-local-variables)) (make-local-variable var))
	(eval (read sexp))
	(or (and (boundp localvar)
		 (symbol-value localvar)
		 (not forcetoread))
	    (set localvar (symbol-value var)))
	(set-buffer localbuf))
      (kill-buffer localbuf)))
    (set-buffer curbuf)))

;;;###autoload
(defun YaTeX-reload-dictionary ()
  "Reload local dictionary.
Use this function after editing ./.yatexrc."
  (interactive)
  (let ((YaTeX-user-table-is-read nil))
    (YaTeX-read-user-completion-table t)))

;;;###autoload
(defun YaTeX-lookup-table (word type)
  "Lookup WORD in completion table whose type is TYPE.
This function refers the symbol tmp-TYPE-table, user-TYPE-table, TYPE-table.
Typically, TYPE is one of 'env, 'section, 'fontsize, 'singlecmd."
  (if (symbolp type) (setq type (symbol-name type)))
  (or (assoc word (symbol-value (intern (concat "tmp-" type "-table"))))
      (assoc word (symbol-value (intern (concat "user-" type "-table"))))
      (assoc word (symbol-value (intern (concat type "-table"))))))

;;;###autoload
(defun YaTeX-update-table (vallist default-table user-table local-table)
  "Update completion table if the car of VALLIST is not in current tables.
Second argument DEFAULT-TABLE is the quoted symbol of default completion
table, third argument USER-TABLE is user table which will be saved in
YaTeX-user-completion-table, fourth argument LOCAL-TABLE should have the
completion which is valid during current Emacs's session.  If you
want to make LOCAL-TABLE valid longer span (but restrict in this directory)
create the file in current directory which has the same name with
YaTeX-user-completion-table."
  (let ((car-v (car vallist)) key answer
	(file (file-name-nondirectory YaTeX-user-completion-table)))
    (cond
     ((assoc car-v (symbol-value default-table))
      nil) ;Nothing to do
     ((setq key (assoc car-v (symbol-value user-table)))
      (if (equal (cdr vallist) (cdr key)) nil
	;; if association hits, but contents differ.
	(message
	 "%s's attributes turned into %s" (car vallist) (cdr vallist))
	(set user-table (delq key (symbol-value user-table)))
	(set user-table (cons vallist (symbol-value user-table)))
	(YaTeX-update-dictionary
	 YaTeX-user-completion-table user-table "user")))
     ((setq key (assoc car-v (symbol-value local-table)))
      (if (equal (cdr vallist) (cdr key)) nil
	(message
	 "%s's attributes turned into %s" (car vallist) (cdr vallist))
	(set local-table (delq key (symbol-value local-table)))
	(set local-table (cons vallist (symbol-value local-table)))
	(set (YaTeX-local-table-symbol local-table) (symbol-value local-table))
	(YaTeX-update-dictionary file local-table)))
     ;; All of above cases, there are some completion in tables.
     ;; Then update tables.
     (t
      (if (not YaTeX-nervous)
	  (setq answer "u")
	(message
	 (cond
	  (YaTeX-japan
	   "`%s'の登録先: U)ユーザ辞書 L)ローカル辞書 N)メモリ D)しない")
	  (t
	   "Register `%s' into: U)serDic L)ocalDic N)one D)iscard"))
	 (if (> (length car-v) 23)
	     (concat (substring car-v 0 10) "..." (substring car-v -9))
	   car-v))
	(setq answer (char-to-string (read-char))))
      (cond
       ((string-match answer "uy")
	(set user-table (cons vallist (symbol-value user-table)))
	(YaTeX-update-dictionary YaTeX-user-completion-table user-table "user")
	)
       ((string-match answer "tl")
	(set local-table (cons vallist (symbol-value local-table)))
	(set (YaTeX-local-table-symbol local-table) (symbol-value local-table))
	(YaTeX-update-dictionary file local-table))
       ((string-match answer "d") nil)	;discard it
       (t (set default-table
	       (cons vallist (symbol-value default-table)))))))))

;;;###autoload
(defun YaTeX-cplread-with-learning
  (prom default-table user-table local-table
	&optional pred reqmatch init hsym)
  "Completing read with learning.
Do a completing read with prompt PROM.  Completion table is what
DEFAULT-TABLE, USER-TABLE, LOCAL table are appended in reverse order.
Note that these tables are passed by the symbol.
Optional arguments PRED, REQMATH and INIT are passed to completing-read
as its arguments PREDICATE, REQUIRE-MATCH and INITIAL-INPUT respectively.
If optional 8th argument HSYM, history symbol, is passed, use it as
history list variable."
  (YaTeX-sync-local-table local-table)
  (let*((table (append (symbol-value local-table)
		       (symbol-value user-table)
		       (symbol-value default-table)))
	(word (completing-read-with-history
	       prom table pred reqmatch init hsym)))
    (if (and (string< "" word) (not (assoc word table)))
	(YaTeX-update-table (list word) default-table user-table local-table))
    word))

;;;###autoload
(defun YaTeX-update-dictionary (file symbol &optional type)
  (let ((local-table-buf (find-file-noselect file))
	(name (symbol-name symbol))
	(value (symbol-value symbol)))
    (save-excursion
      (message "Updating %s dictionary..." (or type "local"))
      (set-buffer local-table-buf)
      (goto-char (point-max))
      (search-backward (concat "(setq " name) nil t)
      (delete-region (point) (progn (forward-sexp) (point)))
      (delete-blank-lines)
      (insert "(setq " name " '(\n")
      (mapcar '(lambda (s)
		 (insert (format "%s\n" (prin1-to-string s))))
	      value)
      (insert "))\n\n")
      (delete-blank-lines)
      (basic-save-buffer)
      (kill-buffer local-table-buf)
      (message "Updating %s dictionary...Done" (or type "local")))))

;;;###autoload
(defun YaTeX-define-begend-key-normal (key env &optional map)
  "Define short cut YaTeX-make-begin-end key."
  (YaTeX-define-key
   key
   (list 'lambda '(arg) '(interactive "P")
	 (list 'YaTeX-insert-begin-end env 'arg))
   map))

;;;###autoload
(defun YaTeX-define-begend-region-key (key env &optional map)
  "Define short cut YaTeX-make-begin-end-region key."
  (YaTeX-define-key key (list 'lambda nil '(interactive)
			      (list 'YaTeX-insert-begin-end env t)) map))

;;;###autoload
(defun YaTeX-define-begend-key (key env &optional map)
  "Define short cut key for begin type completion.
Define both strokes for normal and region mode.
To customize YaTeX, user should use this function."
  (YaTeX-define-begend-key-normal key env map)
  (if YaTeX-inhibit-prefix-letter nil
    (YaTeX-define-begend-region-key
     (concat (upcase (substring key 0 1)) (substring key 1)) env)))

;;;###autoload
(defun YaTeX-search-active-forward (string cmntrx &optional bound err cnt func)
  "Search STRING which is not commented out by CMNTRX.
Optional arguments after BOUND, ERR, CNT are passed literally to search-forward
or search-backward.
Optional sixth argument FUNC changes search-function."
  (let ((sfunc (or func 'search-forward)) found md)
    (while (and (prog1
		    (setq found (funcall sfunc string bound err cnt))
		  (setq md (match-data)))
		(or
		 (and (eq major-mode 'yatex-mode)
		      (YaTeX-in-verb-p (match-beginning 0)))
		 (save-excursion
		   (goto-char (match-beginning 0))
		   (beginning-of-line)
		   (re-search-forward cmntrx (match-beginning 0) t)))))
    (store-match-data md)
    found))

(defun YaTeX-re-search-active-forward (regexp cmntrx &optional bound err cnt)
  "Search REGEXP backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward regexp cmntrx bound err cnt 're-search-forward))

(defun YaTeX-search-active-backward (string cmntrx &optional bound err cnt)
  "Search STRING backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward string cmntrx bound err cnt 'search-backward))

(defun YaTeX-re-search-active-backward (regexp cmntrx &optional bound err cnt)
  "Search REGEXP backward which is not commented out by regexp CMNTRX.
See also YaTeX-search-active-forward."
  (YaTeX-search-active-forward
   regexp cmntrx bound err cnt 're-search-backward))

(defun YaTeX-relative-path-p (path)
  "Return non-nil if PATH is not absolute one."
  (let ((md (match-data)))
    (unwind-protect
	(not (string-match "^\\(/\\|[a-z]:\\|\\\\\\).*/" file))
      (store-match-data md))))

;;;###autoload
(defun YaTeX-switch-to-buffer (file &optional setbuf)
  "Switch to buffer if buffer exists, find file if not.
Optional second arg SETBUF t make use set-buffer instead of switch-to-buffer."
  (interactive "Fswitch to file: ")
  (if (bufferp file)
      (setq file (buffer-file-name file))
    (and (YaTeX-relative-path-p file)
	 (eq major-mode 'yatex-mode)
	 YaTeX-search-file-from-top-directory
	 (save-excursion
	   (YaTeX-visit-main t)
	   (setq file (expand-file-name file)))))
  (let (buf (hilit-auto-highlight (not setbuf)))
    (cond
     ((setq buf (get-file-buffer file))
      (funcall (if setbuf 'set-buffer 'switch-to-buffer)
	       (get-file-buffer file))
      buf)
     ((or YaTeX-create-file-prefix-g (file-exists-p file))
      (or ;find-file returns nil but set current-buffer...
       (if setbuf (set-buffer (find-file-noselect file))
	 (find-file file))
       (current-buffer)))
     (t (message "%s was not found in this directory." file)
	nil))))

;;;###autoload
(defun YaTeX-switch-to-buffer-other-window (file)
  "Switch to buffer if buffer exists, find file if not."
  (interactive "Fswitch to file: ")
  (and (eq major-mode 'yatex-mode)
       (stringp file)
       (YaTeX-relative-path-p file)
       YaTeX-search-file-from-top-directory
       (save-excursion
	 (YaTeX-visit-main t)
	 (setq file (expand-file-name file))))
  (if (bufferp file) (setq file (buffer-file-name file)))
  (cond
   ((get-file-buffer file)
    (switch-to-buffer-other-window (get-file-buffer file))
    t)
   ((or YaTeX-create-file-prefix-g (file-exists-p file))
    (find-file-other-window file) t)
   (t (message "%s was not found in this directory." file)
      nil)))

(defun YaTeX-get-file-buffer (file)
  "Return the FILE's buffer.
Base directory is that of main file or current directory."
  (let (dir main (cdir default-directory))
    (or (and (eq major-mode 'yatex-mode)
	     YaTeX-search-file-from-top-directory
	     (save-excursion
	       (YaTeX-visit-main t)
	       (get-file-buffer file)))
	(get-file-buffer file))))

(defun YaTeX-replace-format-sub (string format repl)
  (let ((beg (or (string-match (concat "^\\(%" format "\\)") string)
		 (string-match (concat "[^%]\\(%" format "\\)") string)))
	(len (length format)))
    (if (null beg) string ;no conversion
      (concat
       (substring string 0 (match-beginning 1)) (or repl "")
       (substring string (match-end 1))))))

;;;###autoload
(defun YaTeX-replace-format (string format repl)
  "In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'"
  (let ((ans string) (case-fold-search nil))
    (while (not (string=
		 ans (setq string (YaTeX-replace-format-sub ans format repl))))
      (setq ans string))
    string))

;;;###autoload
(defun YaTeX-replace-formats (string replace-list)
  (let ((list replace-list))
    (while list
      (setq string (YaTeX-replace-format
		    string (car (car list)) (cdr (car list)))
	    list (cdr list)))
    string))

;;;###autoload
(defun YaTeX-replace-format-args (string &rest args)
  "Translate the argument mark #1, #2, ... #n in the STRING into the
corresponding real arguments ARGS."
  (let ((argp 1))
    (while args
      (setq string
	    (YaTeX-replace-format string (int-to-string argp) (car args)))
      (setq args (cdr args) argp (1+ argp))))
  string)

;;;###autoload
(defun rindex (string char)
  (let ((pos (1- (length string)))(index -1))
    (while (>= pos 0)
      (cond
       ((= (aref string pos) char)
	(setq index pos) (setq pos -1))
       (t (setq pos (1- pos))))
      )
    index))

;;;###autoload
(defun point-beginning-of-line ()
  (save-excursion (beginning-of-line)(point)))

;;;###autoload
(defun point-end-of-line ()
  (save-excursion (end-of-line)(point)))


;;;###autoload
(defun YaTeX-showup-buffer (buffer &optional func select)
  "Make BUFFER show up in certain window (but current window)
that gives the maximum value by the FUNC.  FUNC should take an argument
of its window object.  Non-nil for optional third argument SELECT selects
that window.  This function never selects minibuffer window."
  (or (and (if (and YaTeX-emacs-19 select window-system)
	       (get-buffer-window buffer t)
	     (get-buffer-window buffer))
	   (progn
	     (if select
		 (goto-buffer-window buffer))
	     t))
      (let ((window (selected-window))
	    (wlist (YaTeX-window-list)) win w (x 0))
	(cond
	 ((> (length wlist) 2)
	  (if func
	      (while wlist
		(setq w (car wlist))
		(if (and (not (eq window w))
			 (> (funcall func w) x))
		    (setq win w x (funcall func w)))
		(setq wlist (cdr wlist)))
	    (setq win (get-lru-window)))
	  (select-window win)
	  (switch-to-buffer buffer)
	  (or select (select-window window)))
	 ((= (length wlist) 2)
	  ;(other-window 1);This does not work properly on Emacs-19
	  (select-window (get-lru-window))
	  (switch-to-buffer buffer)
	  (or select (select-window window)))
	 (t				;if one-window
	  (cond
	   ((and YaTeX-emacs-19 window-system (get-buffer-window buffer t))
	    nil)			;if found in other frame
	   (YaTeX-default-pop-window-height
	    (split-window-calculate-height YaTeX-default-pop-window-height)
	    ;;(pop-to-buffer buffer)	;damn! emacs-19.30
	    (select-window (next-window nil 1))
	    (switch-to-buffer (get-buffer-create buffer))
	    (or select (select-window window)))
	   (t nil)))
	 ))))

(cond
 ((fboundp 'screen-height)
  (fset 'YaTeX-screen-height 'screen-height)
  (fset 'YaTeX-screen-width 'screen-width))
 ((fboundp 'frame-height)
  (fset 'YaTeX-screen-height 'frame-height)
  (fset 'YaTeX-screen-width 'frame-width))
 (t (error "I don't know how to run windows.el on this Emacs...")))

;;;###autoload
(defun split-window-calculate-height (height)
  "Split current window wight specified HEIGHT.
If HEIGHT is number, make a new window that has HEIGHT lines.
If HEIGHT is string, make a new window that occupies HEIGT % of screen height.
Otherwise split window conventionally."
  (if (one-window-p t)
      (split-window
       (selected-window)
       (max
	(min
	 (- (YaTeX-screen-height)
	    (if (numberp height)
		(+ height 2)
	      (/ (* (YaTeX-screen-height)
		    (string-to-int height))
		 100)))
	 (- (YaTeX-screen-height) window-min-height 1))
	window-min-height))))

;;;###autoload
(defun YaTeX-window-list ()
  (let*((curw (selected-window)) (win curw) (wlist (list curw)))
    (while (not (eq curw (setq win (next-window win))))
      (or (eq win (minibuffer-window))
	  (setq wlist (cons win wlist))))
    wlist))

(if YaTeX-emacs-21
    ;; Emacs-21's next-window returns other frame's window even if called
    ;; with argument ALL-FRAMES nil, when called from minibuffer context.
    ;; Therefore, check frame identity here.
    (defun YaTeX-window-list ()
      (let*((curw (selected-window)) (win curw) (wlist (list curw))
	    (curf (window-frame curw)))
	(while (and (not (eq curw (setq win (next-window win))))
		    (eq curf (window-frame win)))
	  (or (eq win (minibuffer-window))
	      (setq wlist (cons win wlist))))
	wlist)))

;;;###autoload
(defun substitute-all-key-definition (olddef newdef keymap)
  "Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears."
  (if YaTeX-emacs-19
      (substitute-key-definition olddef newdef keymap global-map)
    (mapcar
     (function (lambda (key) (define-key keymap key newdef)))
     (where-is-internal olddef keymap))))

;;;###autoload
(defun YaTeX-match-string (n &optional m)
  "Return (buffer-substring (match-beginning n) (match-beginning m))."
  (if (match-beginning n)
      (YaTeX-buffer-substring (match-beginning n)
			(match-end (or m n)))))

;;;###autoload
(defun YaTeX-minibuffer-complete ()
  "Complete in minibuffer.
  If the symbol 'delim is bound and is string, its value is assumed to be
the character class of delimiters.  Completion will be performed on
the last field separated by those delimiters.
  If the symbol 'quick is bound and is 't, when the try-completion results
in t, exit minibuffer immediately."
  (interactive)
  (save-restriction
    (narrow-to-region
     (if (fboundp 'field-beginning) (field-beginning (point-max)) (point-min))
     (point-max))
    (let ((md (match-data)) beg word compl
	  (quick (and (boundp 'quick) (eq quick t)))
	  (displist ;function to display completion-list
	   (function
	    (lambda ()
	      (with-output-to-temp-buffer "*Completions*"
		(display-completion-list
		 (all-completions word minibuffer-completion-table)))))))
      (setq beg (if (and (boundp 'delim) (stringp delim))
		    (save-excursion
		      (skip-chars-backward (concat "^" delim))
		      (point))
		  (point-min))
	    word (buffer-substring beg (point-max))
	    compl (try-completion word minibuffer-completion-table))
      (cond
       ((eq compl t)
	(if quick (exit-minibuffer)
	  (let ((p (point)) (max (point-max)))
	    (unwind-protect
		(progn
		  (goto-char max)
		  (insert " [Sole completion]")
		  (goto-char p)
		  (sit-for 1))
	      (delete-region max (point-max))
	      (goto-char p)))))
       ((eq compl nil)
	(ding)
	(save-excursion
	  (let (p)
	    (unwind-protect
		(progn
		  (goto-char (setq p (point-max)))
		  (insert " [No match]")
		  (goto-char p)
		  (sit-for 2))
	      (delete-region p (point-max))))))
       ((string= compl word)
	(funcall displist))
       (t (delete-region beg (point-max))
	  (insert compl)
	  (if quick
	      (if (eq (try-completion compl minibuffer-completion-table) t)
		  (exit-minibuffer)
		(funcall displist)))))
      (store-match-data md))))

(defun YaTeX-minibuffer-quick-complete ()
  "Set 'quick to 't and call YaTeX-minibuffer-complete.
See documentation of YaTeX-minibuffer-complete."
  (interactive)
  (let ((quick t))
    (self-insert-command 1)
    (YaTeX-minibuffer-complete)))

(defun foreach-buffers (pattern job)
  "For each buffer which matches with PATTERN, do JOB."
  (let ((list (buffer-list)))
    (save-excursion
      (while list
	(set-buffer (car list))
	(if (or (and (stringp pattern)
		     (buffer-file-name)
		     (string-match pattern (buffer-file-name)))
		(and (symbolp pattern) major-mode (eq major-mode pattern)))
	    (eval job))
	(setq list (cdr list))))))

(defun goto-buffer-window (buffer)
  "Select window which is bound to BUFFER.
If no such window exist, switch to buffer BUFFER."
  (interactive "BGoto buffer: ")
  (if (stringp buffer)
      (setq buffer (or (get-file-buffer buffer) (get-buffer buffer))))
  (if (get-buffer buffer)
      (cond
       ((get-buffer-window buffer)
	(select-window (get-buffer-window buffer)))
       ((and YaTeX-emacs-19 (get-buffer-window buffer t))
	(let*((win (get-buffer-window buffer t))
	      (frame (window-frame win)))
	  (select-frame frame)
	  (raise-frame frame)
	  (focus-frame frame)
	  (select-window win)
	  (set-mouse-position frame 0 0)
	  (and (featurep 'windows) (fboundp 'win:adjust-window)
	       (win:adjust-window))))
       ((and (featurep 'windows) (fboundp 'win:get-buffer-window)
	     (let ((w (win:get-buffer-window buffer)))
	       (and w (win:switch-window w))))
	(select-window (get-buffer-window buffer)))
       (t (switch-to-buffer buffer)))))

;; Here starts the functions which support gmhist-vs-Emacs19 compatible
;; reading with history.
;;;###autoload
(defun completing-read-with-history
  (prompt table &optional predicate must-match initial hsym)
  "Completing read with general history: gmhist, Emacs-19."
  (let ((minibuffer-history
	 (or (symbol-value hsym)
	     (and (boundp 'minibuffer-history) minibuffer-history)))
	(minibuffer-history-symbol (or hsym 'minibuffer-history)))
    (prog1
	(if (fboundp 'completing-read-with-history-in)
	    (completing-read-with-history-in
	     minibuffer-history-symbol prompt table predicate must-match initial)
	  (completing-read prompt table predicate must-match initial))
      (if (and YaTeX-emacs-19 hsym) (set hsym minibuffer-history)))))

;;;###autoload
(defun read-from-minibuffer-with-history (prompt &optional init map read hsym)
  "Read from minibuffer with general history: gmhist, Emacs-19."
  (cond
   (YaTeX-emacs-19
    (read-from-minibuffer prompt init map read hsym))
   (t
    (let ((minibuffer-history-symbol hsym))
      (read-from-minibuffer prompt init map read)))))

;;;###autoload
(defun read-string-with-history (prompt &optional init hsym)
  "Read string with history: gmhist(Emacs-18) and Emacs-19."
  (cond
   (YaTeX-emacs-19
    (read-from-minibuffer prompt init minibuffer-local-map nil hsym))
   ((featurep 'gmhist-mh)
    (read-with-history-in hsym prompt init))
   (t (read-string prompt init))))

;;;###autoload
(fset 'YaTeX-rassoc
      (if (and nil (fboundp 'rassoc) (subrp (symbol-function 'rassoc)))
	  (symbol-function 'rassoc)
	(function
	 (lambda (key list)
	   (let ((l list))
	     (catch 'found
	       (while l
		 (if (equal key (cdr (car l)))
		     (throw 'found (car l)))
		 (setq l (cdr l)))))))))

(defun YaTeX-insert-file-contents (file visit &optional beg end)
  (cond
   ((and (string< "19" emacs-version) (not (featurep 'xemacs)))
    (insert-file-contents file visit beg end))
   ((string-match "unix\\|linux" (symbol-name system-type))
    (let ((default-process-coding-system
	    (and (boundp '*noconv*) (list '*noconv*)))
	  (file-coding-system (and (boundp '*noconv*) '*noconv*))
	  kanji-fileio-code
	  (default-process-kanji-code 0))
      (call-process shell-file-name file (current-buffer) nil
		    (or (and (boundp 'shell-command-option)
			     shell-command-option)
			"-c")
		    (format "dd bs=1 count=%d | tail -c +%d" end beg))))
    (t (insert-file-contents file))))

(defun YaTeX-split-string (str &optional sep null)
  "Split string STR by every occurrence of SEP(regexp).
If the optional second argument SEP is nil, it defaults to \"[ \f\t\n\r\v]+\".
Do not include null string by default.  Non-nil for optional third argument
NULL includes null string in a list."
  (let ((sep (or sep "[ \f\t\n\r\v]+"))
	list m)
    (while str
      (if (setq m (string-match sep str))
	  (progn
	    (if (or (> m 0) null)
		(setq list (cons (substring str 0 m) list)))
	    (setq str (substring str (match-end 0))))
	(if (or null (string< "" str))
	    (setq list (cons str list)))
	(setq str nil)))
    (nreverse list)))

;;;###autoload
(defun YaTeX-delete1 (elt list)
  "Delete"
  (let (e)
    (while (setq e (YaTeX-member elt list))
      (setq list (delq (car e) list))))
  list)
(if (fboundp 'delete)
    (fset 'YaTeX-delete (symbol-function 'delete))
  (fset 'YaTeX-delete (symbol-function 'YaTeX-delete1)))

(defun YaTeX-member1 (elt list)
  (catch 'found
    (while list
      (if (equal elt (car list))
          (throw 'found list))
      (setq list (cdr list)))))

(if (and (fboundp 'member) (subrp (symbol-function 'member)))
    (fset 'YaTeX-member (symbol-function 'member))
  (fset 'YaTeX-member (symbol-function 'YaTeX-member1)))

;;;
;; Interface function for windows.el
;;;
;;;###autoload
(defun YaTeX-switch-to-window ()
  "Switch to windows.el's window decided by last pressed key."
  (interactive)
  (or (featurep 'windows) (error "Why don't you use `windows.el'?"))
  (win-switch-to-window 1 (- last-command-char win:base-key)))

;;;###autoload
(defun YaTeX-reindent (col)
  "Remove current indentation and reindento to COL column."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (/= col (current-column))
	(progn
	  (delete-region (point) (progn (beginning-of-line) (point)))
	  (indent-to col))))
  (skip-chars-forward " \t" (point-end-of-line)))

(defun YaTeX-inner-environment (&optional quick)
  "Return current inner-most environment.
Non-nil for optional argument QUICK restricts search bound to most
recent sectioning command.  Matching point is stored to property 'point
of 'YaTeX-inner-environment, which can be referred by
 (get 'YaTeX-inner-environment 'point)."
  (put 'YaTeX-inner-environment 'point (point-min))
  (put 'YaTeX-inner-environment 'indent 0)
  (let*((nest 0)
	(beg (YaTeX-replace-format-args
	      (regexp-quote YaTeX-struct-begin)
	      ;YaTeX-struct-begin		;=== TENTATIVE!! ==
	      YaTeX-struct-name-regexp
	      (if (eq major-mode 'yahtml-mode) "\\s *.*" "")
	      ""))
	(end (YaTeX-replace-format-args
	      (regexp-quote YaTeX-struct-end)
	      YaTeX-struct-name-regexp "" ""))
	(begend (concat "\\(" beg "\\)\\|\\(" end "\\)"))
	bound m0
	(htmlp (eq major-mode 'yahtml-mode))
	(open
	 (concat "^" (or (cdr (assq major-mode '((yahtml-mode . "<")))) "{")))
	(close
	 (concat "^"
		 (or (cdr(assq major-mode '((yahtml-mode . "\n\t >")))) "}"))))
    (save-excursion
      (if quick
	  (setq bound
		(save-excursion
		  (if htmlp 
		      ;;(re-search-backward YaTeX-sectioning-regexp nil 1)
		      ;;(goto-char (point-min)) ;Is this enough? 97/6/26
		      (re-search-backward yahtml-indentation-boundary nil 1)
		    (YaTeX-re-search-active-backward
		     (concat YaTeX-ec-regexp
			     "\\(" YaTeX-sectioning-regexp "\\)\\*?{")
		     YaTeX-comment-prefix nil 1))
		  (or (bobp) (end-of-line))
		  (point))))
      (if (catch 'begin
	    (if (and (numberp bound) (< (point) bound)) (throw 'begin nil))
	    (while (YaTeX-re-search-active-backward
		    begend YaTeX-comment-prefix bound t)
	      (setq m0 (match-beginning 0))
	      (if (looking-at end) ;;(match-beginning 2)
		  (setq nest (1+ nest))
		(setq nest (1- nest)))
	      (if (< nest 0)
		  (progn
		    (put 'YaTeX-inner-environment 'point m0)
		    (goto-char m0)
		    (put 'YaTeX-inner-environment 'indent (current-column))
		    (throw 'begin t)))))
	  (buffer-substring
	   (progn (skip-chars-forward open) (1+ (point)))
	   (progn (skip-chars-forward close) (point)))))))

(defun YaTeX-goto-corresponding-environment (&optional allow-mismatch noerr)
  "Go to corresponding begin/end enclosure.
Optional argument ALLOW-MISMATCH allows mismatch open/clese.  Use this
for \left(, \right).
Optional third argument NOERR causes no error for unballanced environment."
  (interactive)
  (if (not (YaTeX-on-begin-end-p)) nil
    (let ((p  (match-end 0)) b0 b1 env (nest 0) regexp re-s (op (point))
	  (m0 (match-beginning 0))	;whole matching
	  (m1 (match-beginning 1))	;environment in \begin{}
	  (m2 (match-beginning 2))	;environment in \end{}
	  (m3 (match-beginning 3)))	;environment in \[ \] \( \)
      ;(setq env (regexp-quote (buffer-substring p (match-beginning 0))))
      (if (cond
	   (m1				;if begin{xxx}
	    (setq env
		  (if allow-mismatch YaTeX-struct-name-regexp
		    (regexp-quote (buffer-substring m1 (match-end 1)))))
	;    (setq regexp (concat "\\(\\\\end{" env "}\\)\\|"
	;			 "\\(\\\\begin{" env "}\\)"))
	    (setq regexp
		  (concat
		   "\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-end) env "" "")
		   "\\)\\|\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-begin) env "" "")
		   "\\)"))
	    (setq re-s 're-search-forward))
	   (m2				;if end{xxx}
	    (setq env
		  (if allow-mismatch YaTeX-struct-name-regexp
		    (regexp-quote (buffer-substring m2 (match-end 2)))))
	;   (setq regexp (concat "\\(\\\\begin{" env "}\\)\\|"
	;			 "\\(\\\\end{" env "}\\)"))
	    (setq regexp
		  (concat
		   "\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-begin) env "" "")
		   "\\)\\|\\("
		   (YaTeX-replace-format-args
		    (regexp-quote YaTeX-struct-end) env "" "")
		   "\\)"))
	    (setq re-s 're-search-backward))
	   (m3				;math environment
	    (setq env (char-after (1+ m3))
		  regexp (format "\\(%s%s\\)\\|\\(%s%s\\)"
				 YaTeX-ec-regexp
				 (regexp-quote
				  (cdr (assq env '((?( . ")") (?) . "(")
						   (?[ . "]") (?] . "[")))))
				 YaTeX-ec-regexp
				 (regexp-quote (char-to-string env)))
		  re-s (if (memq env '(?\( ?\[))
			   're-search-forward
			 're-search-backward)))
	   (t (if noerr nil (error "Corresponding environment not found."))))
	  (progn
	    (while (and (>= nest 0) (funcall re-s regexp nil t))
	      (setq b0 (match-beginning 0) b1 (match-beginning 1))
	      (if (or (equal b0 m0)
		      (YaTeX-literal-p b0))
		  nil
		(setq nest (if (equal b0 b1)
			       (1- nest) (1+ nest)))))
	    (if (< nest 0)
		(goto-char (match-beginning 0)) ;found.
	      (goto-char op)
	      (funcall
	       (if noerr 'message 'error)
	       "Corresponding environment `%s' not found." env)
	      (sit-for 1)
	      nil))))))

(defun YaTeX-end-environment ()
  "Close opening environment"
  (interactive)
  (let ((env (YaTeX-inner-environment)))
    (if (not env) (error "No premature environment")
      (save-excursion
	(if (YaTeX-search-active-forward
	     (YaTeX-replace-format-args YaTeX-struct-end env "" "")
	     YaTeX-comment-prefix nil t)
	    (if (y-or-n-p
		 (concat "Environment `" env
			 "' may be already closed. Force close?"))
		nil
	      (error "end environment aborted."))))
      (message "")			;Erase (y or n) message.
      (YaTeX-insert-struc 'end env)
      (save-excursion
	(goto-char (or (get 'YaTeX-inner-environment 'point) (match-end 0)))
	(if (pos-visible-in-window-p)
	    (sit-for (if YaTeX-dos 2 1))
	  (message "Matches with %s at line %d"
		   (YaTeX-replace-format-args YaTeX-struct-begin env "" "")
		   (count-lines (point-min) (point))))))))

(defun YaTeX-beginning-of-environment (&optional limit-search-bound end)
  "Goto the beginning of the current environment.
Optional argument LIMIT-SEARCH-BOUND non-nil limits the search bound to
most recent sectioning command.  Non-nil for optional third argument END
goes to end of environment."
  (interactive)
  (let ((op (point)))
    (if (YaTeX-inner-environment limit-search-bound)
	(progn
	  (goto-char (get 'YaTeX-inner-environment 'point))
	  (and end (YaTeX-goto-corresponding-environment))
	  (if (interactive-p) (push-mark op))
	  (point)))))

(defun YaTeX-end-of-environment (&optional limit-search-bound)
  "Goto the end of the current environment.
Optional argument LIMIT-SEARCH-BOUND non-nil limits the search bound
to most recent sectioning command."
  (interactive)
  (YaTeX-beginning-of-environment limit-search-bound t))

(defun YaTeX-mark-environment ()
  "Mark current position and move point to end of environment."
  (interactive)
  (let ((curp (point)))
    (if (and (YaTeX-on-begin-end-p) (match-beginning 1)) ;if on \\begin
	(progn (goto-char (match-end 0)))
      (if (= (char-after (point)) ?\\) nil	;if on \\end
	(skip-chars-backward "^\n\\\\")
	(or (bolp) (forward-char -1))))
    (if (not (YaTeX-end-of-environment))   ;arg1 turns to match-beginning 1
	(progn
	  (goto-char curp)
	  (error "Cannot found the end of current environment."))
      (YaTeX-goto-corresponding-environment)
      (beginning-of-line)		;for confirmation
      (if (< curp (point))
	  (progn
	    (message "Mark this environment?(y or n): ")
	    (if (= (read-char) ?y) nil
	      (goto-char curp)
	      (error "Abort.  Please call again at more proper position."))))
      (set-mark-command nil)
      (YaTeX-goto-corresponding-environment)
      (end-of-line)
      (if (eobp) nil (forward-char 1)))))

(defun YaTeX-kill-buffer (buffer)
  "Make effort to show parent buffer after kill."
  (interactive "bKill buffer: ")
  (or (get-buffer buffer)
      (error "No such buffer %s" buffer))
  (let ((pf YaTeX-parent-file))
    (kill-buffer buffer)
    (and pf
	 (get-file-buffer pf)
	 (switch-to-buffer (get-file-buffer pf)))))

;;;VER2
(defun YaTeX-insert-struc (what env)
  (cond
   ((eq what 'begin)
    (insert (YaTeX-replace-format-args
	     YaTeX-struct-begin env (YaTeX-addin env))))
   ((eq what 'end)
    (insert (YaTeX-replace-format-args YaTeX-struct-end env)))
   (t nil)))

(defun YaTeX-string-width (str)
  "Return the display width of string."
  (if (fboundp 'string-width)
      (string-width str)
    (length str)))
(defun YaTeX-truncate-string-width (str width)
  (cond
   ((fboundp 'truncate-string-to-width) (truncate-string-to-width str width))
   ((fboundp 'truncate-string) (truncate-string str width))
   (t (substring str 0 width))))

;;; Function for menu support
(defun YaTeX-define-menu (keymap bindlist)
  "Define KEYMAP(symbol)'s menu-bindings according to BINDLIST.
KEYMAP should be a quoted symbol of newly allocated keymap.
BINDLIST consists of binding list.  Each element is as follows.

 '(menusymbol DOC_String . contents)

CONTENTS is one of lambda-form, interactive function, or other keymap.
See yatex19.el for example."
  (cond
   ((featurep 'xemacs)
    (let (name)
      (if (keymapp (symbol-value keymap))
	  (progn
	    (setq name (keymap-name (symbol-value keymap)))
	    (set keymap nil))
	(setq name (car (symbol-value keymap)))
	(set keymap (cdr (symbol-value keymap))))
      (mapcar
       (function
	(lambda (bind)
	  (setq bind (cdr bind))
	   (if (eq (car-safe (cdr bind)) 'lambda)
	       (setcar (cdr bind) 'progn))
	   (if (stringp (car-safe (cdr bind)))
	       (set keymap
		    (cons (cdr bind) (symbol-value keymap)))
	     (set keymap
		  (cons (vector (car bind) (cdr bind) t)
			(symbol-value keymap))))))
       bindlist)
      (set keymap (cons name (symbol-value keymap)))))
   (t
    (mapcar
     (function
      (lambda (bind)
	(define-key (symbol-value keymap) (vector (car bind)) (cdr bind))))
     bindlist))))

;;;
;; Emacs 21 compensational wrapper
;;;
(defun YaTeX-minibuffer-begin ()
 (if (fboundp 'field-beginning)
     (field-beginning (point-max))
   (point-min)))

(defun YaTeX-minibuffer-end ()
 (if (fboundp 'field-end)
     (field-end (point-max))
   (point-max)))

(defun YaTeX-minibuffer-string ()
  (buffer-substring (YaTeX-minibuffer-begin) (YaTeX-minibuffer-end)))

(defun YaTeX-minibuffer-erase ()
  (if (eq (selected-window) (minibuffer-window))
      (if (fboundp 'delete-field) (delete-field) (erase-buffer))))

(fset 'YaTeX-buffer-substring
      (if (fboundp 'buffer-substring-no-properties)
	  'buffer-substring-no-properties
	'buffer-substring))

;;;
;; hilit19 vs. font-lock
;;;
(defvar YaTeX-19-functions-font-lock-direct
  '(YaTeX-19-re-search-in-env))

(defun YaTeX-convert-pattern-hilit2fontlock (h19pa)
  "Convert hilit19's H19PA patterns alist to font-lock's one.
This function is a makeshift for YaTeX and yahtml."
  (let ((ignorecase (not (null (car h19pa))))
	(palist (cdr h19pa))
	flpa i newface
	(mapping
	 '((bold . YaTeX-font-lock-bold-face)
	   (italic . YaTeX-font-lock-italic-face)
	   (defun . font-lock-function-name-face)
	   (define . font-lock-variable-name-face)
	   (keyword . font-lock-keyword-face)
	   (decl . YaTeX-font-lock-declaration-face)
	   (label . YaTeX-font-lock-label-face)
	   (crossref . YaTeX-font-lock-crossref-face)
	   (include . YaTeX-font-lock-include-face)
	   (formula . YaTeX-font-lock-formula-face)
	   (delimiter . YaTeX-font-lock-delimiter-face)
	   (string . ignore) (comment . ignore)
	   )))
    (while (setq i (car palist))
      (setq newface (nth 2 i)
	    newface (or (cdr (assq newface mapping)) newface))
      (cond
       ((eq newface 'ignore) nil)	;no translation
       ((stringp (car i))		;hiliting by regexp
	(setq flpa
	      (cons
	       (if (numberp (car (cdr i)))
		   (list (car i)	;regexp
			 (car (cdr i))	;matching group number
			 newface nil) ;'keep)	;keep is hilit19 taste
		 (list
		  (concat
		   (car i)		;original regexp and..
		   ;;"[^"
		   ;;(regexp-quote (substring (car (cdr i)) 0 1))
		   ;;"]+" ;for shortest match
		   ".*"
		   (car (cdr i)))
		  0 (list 'quote newface) nil)) ;;'keep))
	       flpa)))
       ((and (symbolp (car i)) (fboundp (car i)))
	(if (memq (car i) YaTeX-19-functions-font-lock-direct)
	    ;; Put direct function call for it.
	    ;; When calling this function, fontify entire matched string.
	    (setq flpa
		  (cons
		   (list
		    (list 'lambda (list 'dummy) ;dummy should be boundary
			  (list (car i) (list 'quote (car (cdr i)))))
		    (list 0 newface))
		   flpa))
	  (setq flpa
		(cons
		 (list (car (cdr i))	;regexp
		       (list
			(list
			 'lambda (list 'dummy)
			 '(goto-char (match-beginning 0))
			 (if (eq (nth 3 i) 'overwrite)
			     nil
			   '(remove-text-properties
			     (point) (min (point-max) (1+ (point)))
			     '(face nil font-lock-multiline nil)))
			 (list
			  'let (list '(e (match-end 0))
				     (list 'm (list (car i) (car (cdr i)))))
			  (list
			   'if 'm
			   (list
			    'YaTeX-font-lock-fillin
			    (list 'car 'm)
			    (list 'cdr 'm)
			    (list 'quote 'face)
			    (list 'quote 'font-lock)
			    (list 'quote newface))
			   '(goto-char e)
			   ))
			 nil)		;retun nil to cheat font-lock
			nil nil))	;pre-match, post-match both nil
		 flpa)))))
      (setq palist (cdr palist)));while
    (if (featurep 'xemacsp)
	(nreverse flpa)
      flpa)))

(if (and (boundp 'YaTeX-use-font-lock)
	 YaTeX-use-font-lock)
    (require 'font-lock))

(cond
 ((and (featurep 'font-lock) (fboundp 'defface))
  ;; In each defface, '(class static-color) is for Emacs-21 -nw
  ;; '(class tty) is for XEmacs-21 -nw
  (defface YaTeX-font-lock-label-face
    '((((class static-color)) (:foreground "yellow" :underline t))
      (((type tty)) (:foreground "yellow" :underline t))
      (((class color) (background dark)) (:foreground "pink" :underline t))
      (((class color) (background light)) (:foreground "red" :underline t))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight labels."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-label-face 'YaTeX-font-lock-label-face)

  (defface YaTeX-font-lock-declaration-face
    '((((class color) (background dark)) (:foreground "cyan"))
      (((class color) (background light)) (:foreground "RoyalBlue"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight some declarations."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-declaration-face 'YaTeX-font-lock-declaration-face)

  (defface YaTeX-font-lock-include-face
    '((((class color) (background dark)) (:foreground "Plum1"))
      (((class color) (background light)) (:foreground "purple"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight expression for including."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-include-face 'YaTeX-font-lock-include-face)

  (defface YaTeX-font-lock-formula-face
    '((((class static-color)) (:bold t))
      (((type tty)) (:bold t))
      (((class color) (background dark)) (:foreground "khaki" :bold t))
      (((class color) (background light)) (:foreground "Goldenrod"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight formula."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-formula-face 'YaTeX-font-lock-formula-face)

  (defface YaTeX-font-lock-delimiter-face
    '((((class static-color)) (:bold t))
      (((type tty)) (:bold t))
      (((class color) (background dark))
       (:foreground "saddlebrown" :background "ivory" :bold t))
      (((class color) (background light)) (:foreground "red"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight delimiters."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-delimiter-face 'YaTeX-font-lock-delimiter-face)

  (defface YaTeX-font-lock-math-sub-face
    '((((class static-color)) (:bold t))
      (((type tty)) (:bold t))
      (((class color) (background dark))
       (:foreground "khaki" :bold t :underline t))
      (((class color) (background light))
       (:foreground "Goldenrod" :underline t))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight subscripts in formula."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-math-sub-face 'YaTeX-font-lock-math-sub-face)

  (defface YaTeX-font-lock-math-sup-face
    '((((class static-color)) (:bold t))
      (((type tty)) (:bold t))
      (((class color) (background dark))
       (:bold nil :foreground "ivory" :background "lightyellow4"))
      (((class color) (background light))
       (:underline t :foreground "gold"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight superscripts in formula."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-math-sup-face 'YaTeX-font-lock-math-sup-face)

  (defface YaTeX-font-lock-crossref-face
    '((((class color) (background dark)) (:foreground "lightgoldenrod"))
      (((class color) (background light)) (:foreground "DarkGoldenrod"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight cross references."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-crossref-face 'YaTeX-font-lock-crossref-face)

  (defface YaTeX-font-lock-bold-face
    '((t (:bold t)))
    "Font Lock mode face used to express bold itself."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-bold-face 'YaTeX-font-lock-bold-face)

  (defface YaTeX-font-lock-italic-face
    '((t (:italic t)))
    "Font Lock mode face used to express italic itself."
    :group 'font-lock-faces)
  (defvar YaTeX-font-lock-italic-face 'YaTeX-font-lock-italic-face)

  ;; Make sure the 'YaTeX-font-lock-{italic,bold}-face is bound with
  ;; italic/bold fontsets
  (if (and (fboundp 'fontset-list) YaTeX-use-italic-bold)
      (let ((flist (fontset-list)) fnt italic bold
	    (df (or (and (fboundp 'face-font-name) (face-font-name 'default))
		    (face-font 'default)
		    (face-font 'italic)
		    (face-font 'bold)
		    "giveup!"))
	    sz medium-i bold-r)
	(string-match
	 "^-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-\\(\\([0-9]+\\)\\)" df)
	(setq sz (or (match-string 1 df) "16"))
	(setq medium-i (format "-medium-i-[^-]+--%s" sz)
	      bold-r (format "-bold-r-[^-]+--%s" sz))
	(while flist
	  (setq fnt (car flist))
	  (condition-case err
	      (cond
	       ((and (string-match medium-i fnt)
		     (null italic))
		(set-face-font 'YaTeX-font-lock-italic-face (setq italic fnt)))
	       ((and (string-match bold-r fnt) (null bold))
		(set-face-font 'YaTeX-font-lock-bold-face (setq bold fnt))))
	    (error nil))
	  (setq flist (cdr flist)))))

  ;;Borrowed from XEmacs's font-lock.el
  (defsubst YaTeX-font-lock-fillin (start end setprop markprop value &optional object)
    "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
    (let ((start (text-property-any start end markprop nil object)) next
	  (putfunc (if (fboundp 'put-nonduplicable-text-property)
		       'put-nonduplicable-text-property
		     'put-text-property)))
      (if (eq putfunc 'put-text-property)
	  (setq markprop setprop))
      (while start
	(setq next (next-single-property-change start markprop object end))
	(funcall putfunc start next setprop value object)
	(funcall putfunc start next markprop value object)
	(setq start (text-property-any next end markprop nil object)))))

  (defun YaTeX-warning-font-lock (mode)
    (let ((sw (selected-window)))
      ;;(pop-to-buffer (format " *%s warning*" mode))
      ;;(erase-buffer)
      (momentary-string-display
      (cond
       (YaTeX-japan
	(concat mode " は、既に font-lock に対応しました。\n"
		"~/.emacs などにある\n"
		"\t(put 'yatex-mode 'font-lock-keywords 'tex-mode)\n"
		"\t(put 'yahtml-mode 'font-lock-keywords 'html-mode)\n"
		"などの間に合わせの記述はもはや不要です。"))
       (t
	(concat mode " now supports the font-lock by itself.\n"
		"So you can remove the descriptions such as\n"
		"\t(put 'yatex-mode 'font-lock-keywords 'tex-mode)\n"
		"\t(put 'yahtml-mode 'font-lock-keywords 'html-mode)\n"
		"in your ~/.emacs file.  Thank you."))) (point))
      (select-window sw)))
  ))

(defun YaTeX-assoc-regexp (elt alist)
  "Like assoc, return a list of whose car match with ELT.  Search from ALIST.
Note that each car of cons-cell is regexp.  ELT is a plain text to be
compared by regexp."
  (let (x)
    (catch 'found
      (while alist
	(setq x (car (car alist)))
	(if (string-match x elt)
	    (throw 'found (car alist)))
	(setq alist (cdr alist))))))

;;;
;; Functions for the Installation time
;;;

(defun bcf-and-exit ()
  "Byte compile rest of argument and kill-emacs."
  (if command-line-args-left
      (let ((load-path (cons "." load-path)))
	(and (fboundp 'set-language-environment)
	     (featurep 'mule)
	     (set-language-environment "Japanese"))
	(mapcar 'byte-compile-file command-line-args-left)
	(kill-emacs))))

(defun tfb-and-exit ()
  "Texinfo-format-buffer and kill-emacs."
  (if command-line-args-left
      (let ((load-path (cons ".." load-path)))
	(and (fboundp 'set-language-environment)
	     (featurep 'mule)
	     (set-language-environment "Japanese"))
	(mapcar (function
		 (lambda (arg)
		   (find-file arg)
		   (texinfo-format-buffer)
		   (basic-save-buffer)))
		command-line-args-left)
	(kill-emacs))))

(provide 'yatexlib)
; Local variables:
; fill-prefix: ";;; "
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; coding: sjis
; End:
