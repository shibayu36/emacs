;;; -*- Emacs-Lisp -*-
;;; YaTeX add-in function generator.
;;; yatexgen.el rev.5
;;; (c)1991-1995,1999,2000 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Fri Jun 27 12:09:37 2003 on firestorm
;;; $Id: yatexgen.el,v 1.72 2003/12/25 04:10:54 yuuji Rel $

(require 'yatex)

(defmacro YaTeX-setq (var japanese english)
  (list 'setq var
	(if YaTeX-japan japanese english))
)

(put 'YaTeX-setq 'lisp-indent-hook 1)

(YaTeX-setq YaTeX-generate-initial-message
  "             自動生成モードへようこそ!!

初めてやる人はこのバッファの例にしたがって指示通りにやって練習してね。
本番の時もこのバッファに出るメッセージを *よく読んで* 操作しないとう
まく関数が作れないよ!!

  ではリターンキーを押して下さい。"
  "             Welcome to auto-generation mode!!

If this is your first trial, exercise this according to example and
following my messages.  Then, at making actual function, operate
reading my messages *carefully*, or you'll fail to generate appropriate
function.

  Hit return key!"
)

(YaTeX-setq YaTeX-generate-start-message
  "さぁはじめるよ.\n1.登録したい補完をやってみて.
たとえば section 型補完の \\documentstyle だったら \\documentstyle{}
だけをいれてみてね. ちゃんと『～型補完』を使わないとダメよ!。
で、おわったらりたーん!!"
  "Let's begin completion for which you want to make add-in function.
If you want to make add-in function for \\documentstyle input only
`\\documentstyle{}' *with* completion of yatex-mode.
If you finish this, please press RET."
)

(YaTeX-setq YaTeX-generate-abort-message
  "やめた、やめた～いめんどくせ～"
  "Aborted."
)

(YaTeX-setq YaTeX-generate-same-message
  "それじゃ、なにも変わってねぇだろーが! やめた。"
  "I found no difference between them.  So I'm quitting."
)

(YaTeX-setq YaTeX-generate-invalid-message
  "それは、ちと無理というものじゃ."
  "It's impossible."
)

(YaTeX-setq YaTeX-generate-idontknow-message
  "う～ん、難しくてよくわからないなぁ。ばかでごめんねェ～"
  "Sorry I can't tell your adding method."
)

(YaTeX-setq YaTeX-generate-confirm-message
  "ということは、付け足したい部分はこれでいいのね"
  "Is it additional string of add-in function?"
)

(YaTeX-setq YaTeX-generate-output-message
  "2.じゃ、それにくっつけたいものを *カーソルの位置に* 足してみて. 
さっきの \\documentstyle{} の例だと \\documentstyle[12pt]{} とかにするの。
しつこいようだけど、今の位置からカーソル動かしちゃダメよ!!
で、またおわったらりたーん!!"
  "2.Then input additional string *at CURSOR POSITION*
According to last example \\documentstyle{},
modify it \\documentstyle[12pt]{}.  RET to finish."
)

(YaTeX-setq YaTeX-generate-put-arg-message
  "3.このうち、キーボードから読み込んで欲しい文字列を順に入れて。
さっきの \\documentstyle[12pt]{} だったら、付加する文字は[12pt]だけど
手で入れたいのは 12pt の部分だけですね。
で、全部入れ終ったら、りたーんだけうってね!!"
  "3.In this string, extract string which you want to input from key
board with quiry afterwards.  For example, though additional string is
\\documentstyle[12pt]{}, but you want enter only `12pt' by hand.
RET to finish!"
)

(YaTeX-setq YaTeX-generate-read-prompt-message
  "4.では、あとでこれらの文字列を読み込む時に、どういうプロンプトを
出したいですか? 順に入れて下さい。面倒なら単にリターンを打ってね。
さっきの 12pt の部分だったら、『サイズは』とかがおすすめ。"
  "4.When you use this add-in function afterwards, what message
do you like to be prompted with to enter these values.  In last
example `12pt', typical prompt string may be `Size?: '."
)

(YaTeX-setq YaTeX-generate-done-message
  "よし! これが、君の作りたかった関数だ。~/.emacs にでも入れてせいぜい
楽してくれ。このバッファ(*ご案内*)を yatex-mode にしておくから
できた関数が本当にお望みの動作をするか確かめてみるといいかもね。
  ところで、この関数こんなに簡単だろう? そろそろ自分で書いたらどう?
"
  "OK! This is the definition of function you want to make!  Add
this description to your ~/.emacs or so.  Use this buffer(*Guide*)
for testing of this function please.
  But you can see this function quite easy, can't you? You had better
write your most favorite add-in function yourself!
"
)

(YaTeX-setq YaTeX-generate-nomatch-message
  "こらこら、そんな文字列どこにもねーぞ!!"
  "No such string in additional string."
)
(YaTeX-setq YaTeX-generate-buffer
  "*付加関数生成バッファ*"
  "*Generate-add-in-function*"
)

(YaTeX-setq YaTeX-generate-message-buffer
  "*ご案内*"
  "*Guide*"
)

(YaTeX-setq YaTeX-generate-bug-message
  "ごめ～ん!! ちょっと、このアドイン関数つくるの失敗しちゃったみたい!!
作者まで連絡してくださ～～～い!"
  "Sorry I failed to make add-in function for you...
Send bug report to me."
)

(YaTeX-setq YaTeX-generate-narrow-message
  "画面がせますぎるような気がします。"
  "Too narrow screen height."
)

(defvar YaTeX-generate-message-height
  10 "Window height of YaTeX-generate-message-buffer")

;; Do you need learning for generated function?
;; If you need, please tell me (yuuji@yatex.org)
;;(defvar YaTeX-generate-variables-for-learning nil)
;;(defvar YaTeX-generate-current-completion-table nil)

;;;
;Generate mode.
;;;
(defun YaTeX-generate ()
  "Genarate YaTeX add-in function with enquiry."
  (interactive)
  (if (< (YaTeX-screen-height) (+ YaTeX-generate-message-height 10))
      (error YaTeX-generate-narrow-message))
  (put 'YaTeX-generate 'disabled t)
  (save-window-excursion
    (unwind-protect
	(let (input output (i 0) (beg 0) end add-in map map1 si str slist
		    (from (make-marker)) (to (make-marker)))
	  (delete-other-windows)
	  (switch-to-buffer YaTeX-generate-message-buffer)
	  (yatex-mode)
	  (erase-buffer)
	  (insert YaTeX-generate-initial-message)
	  (read-string
	   (if YaTeX-japan "リターンキーを押して下さい." "Press RETURN."))
	  (erase-buffer)
	  (insert YaTeX-generate-start-message)
	  (pop-to-buffer (get-buffer-create YaTeX-generate-buffer))
	  (enlarge-window (- (window-height) YaTeX-generate-message-height 1))
	  (erase-buffer)
	  (yatex-mode)
	  (use-local-map (setq map (copy-keymap YaTeX-mode-map)))
	  (define-key (current-local-map) "\n" 'exit-recursive-edit)
	  (define-key (current-local-map) "\r" 'exit-recursive-edit)
	  (define-key (current-local-map) "\C-g" 'abort-recursive-edit)
	  (setq map1 (copy-keymap map))
	  (YaTeX-suppress-sparse-keymap map)
	  ;;First get input form.
	  (recursive-edit)
	  (setq input (YaTeX-minibuffer-string)
		end (1- (length input)))
	  (if (string= "" input) (error YaTeX-generate-abort-message))
	  (YaTeX-generate-move-to-add-in-position)
	  (set-marker from (1- (point)))  ;;Can't write before `from'
	  (set-marker to (1+ (point)))    ;;Can't write after `to'
	  ;;Second get output form.
	  (setq beg (1- (point)));;Cheat begin point!
	  (YaTeX-generate-display-message YaTeX-generate-output-message)
	  (use-local-map map1)
	  (fset 'si (symbol-function 'self-insert-command))
	  (defun self-insert-command (arg)
	    (interactive "p")
	    (if (or (not (equal (buffer-name) YaTeX-generate-buffer))
		    (and (> (point) (marker-position from))
			 (< (point) (marker-position to))))
		(insert (this-command-keys)) (ding)))
	  (unwind-protect
	      (recursive-edit)
	    (fset 'self-insert-command (symbol-function 'si)))
	  (setq output (YaTeX-minibuffer-string))
	  (cond ((string= "" output)	(error YaTeX-generate-abort-message))
		((string= input output)	(error YaTeX-generate-same-message))
		((< (length output) (length input))
		 (error YaTeX-generate-invalid-message)))
	  ;;(while (and (< beg end) (= (aref input beg) (aref output i)))
	  ;;  (setq beg (1+ beg) i (1+ i))) ;;for universal use.
	  (setq i (1- (length output)))
	  (while (and (>= end beg) (= (aref output i) (aref input end)))
	    (setq end (1- end) i (1- i)))
	  (setq add-in (substring output beg
				  (if (= i (1- (length output))) nil (1+ i))))
	  (erase-buffer)
	  (insert add-in)
	  (if (not (y-or-n-p YaTeX-generate-confirm-message))
	      (error YaTeX-generate-idontknow-message))
	  ;;Extract arguments.
	  (YaTeX-generate-display-message YaTeX-generate-put-arg-message)
	  (setq i 1)
	  (while (not (string=
		       "" (setq str (read-string (format "Arg %d: " i)))))
	    (if (not (string-match (regexp-quote str) add-in))
		(progn
		  (ding)
		  (YaTeX-generate-display-message
		   YaTeX-generate-nomatch-message -1))
	      (setq slist (append slist (list (list str))) i (1+ i)))
	    );input all of arguments.
	  ;;Compare with output string.
	  (set-buffer YaTeX-generate-buffer) ;;for safety
	  (emacs-lisp-mode)
	  (if (> i 1)
	      (YaTeX-generate-parse-add-in slist add-in)
	    (erase-buffer)
	    (insert "(defun " (YaTeX-generate-function-name) " ()\n")
	    (insert "\"" (YaTeX-generate-lisp-quote add-in) "\")\n")
	    (indent-region (point-min) (point-max) nil)
	    (message (if YaTeX-japan
			 "このくらいの関数手で書け!!"
		       "You don't need me to make such easy function.")))
	  );let
      (put 'YaTeX-generate 'disabled nil)
      (put 'YaTeX-addin 'disabled nil)
    ))
  (YaTeX-generate-display-message YaTeX-generate-done-message)
  (switch-to-buffer YaTeX-generate-buffer)
  (condition-case error
      (eval-current-buffer)
    (error (insert YaTeX-generate-bug-message)))
  (pop-to-buffer YaTeX-generate-message-buffer)
)

(defun YaTeX-generate-parse-add-in (args add-in)
  "Parse add-in string and extract argument for it.
Variable add-in is referred in parent function."
  (let ((i 1) j (case-fold-search nil) ;i holds argument number
	(prompt (make-vector (length args) ""))
	(used (make-vector (length add-in) nil))
	func-name (string ""))
    ;;Phase 1. extract argument from add-in string.
    (mapcar
     '(lambda (arg)
	(let ((index 0) (match 0) beg end (carg (car arg)))
	  (YaTeX-generate-display-message
	   YaTeX-generate-read-prompt-message)
	  (aset prompt (1- i)
		(read-string
		 (format
		  (if YaTeX-japan "%d番目(%s)を読む時?: "
		    "When reading argument #%d(%s)?: ") i (car arg))))
	  (while (string-match (regexp-quote carg) (substring add-in index))
	    (setq beg (+ index (match-beginning 0))
		  end (+ index (match-end 0)))
	    (if (aref used beg) nil
	      (setq match (1+ match))
	      (cond
	       ((= match 1)
		;;(setq arg (append arg (list (list beg end))))
		(YaTeX-generate-register-match))
	       ((YaTeX-generate-ask-match-position)
		(YaTeX-generate-register-match))))
	    (setq index end))
	  (setq i (1+ i))))
     args)
    ;;Phase 2. Generate function!!
    (setq i 0)
    (setq func-name (YaTeX-generate-function-name))
    (while (< i (length add-in))
      (setq beg i j (aref used i))
      (while (and (< i (length add-in)) (equal j (aref used i)))
	(setq i (1+ i)))
      (if j		;If it is argument.
	  (setq string (concat string (format " arg%d" j)))
	(setq string
	      (concat string " \""
		      (YaTeX-generate-quote-quote (substring add-in beg i))
		      "\""))
	))
    (erase-buffer)
    (setq i 1)
    (insert
     "(defun " func-name " ()\n"
     "  (let (")
    (mapcar
     '(lambda (arg)
	(insert (format "(arg%d (read-string \"%s: \"))\n"
			i (aref prompt (1- i))))
	(setq i (1+ i)))
     args)
    (delete-region (point) (progn (forward-line -1) (end-of-line) (point)))
    (insert ")\n(concat " (YaTeX-generate-lisp-quote string)
	    ")))\n")
    (indent-region (point-min) (point) nil)
    used)
)

(defun YaTeX-generate-ask-match-position ()
  "Ask user whether match-position is in his expectation,
Referencing variables in parent function YaTeX-generate-parse-add-in."
  (pop-to-buffer YaTeX-generate-message-buffer)
  (goto-char (point-max))
  (insert "\n\n"
	  (format (if YaTeX-japan "%d 番目の引数 %s って"
		    "Is argument #%d's value `%s' also corresponding to")
		  i carg) "\n" add-in "\n")
  (indent-to-column beg)
  (let ((c beg))
    (while (< c end) (insert "^") (setq c (1+ c))))
  (insert "\n" (if YaTeX-japan "ここにも対応してるの?"
		 "this underlined part too?"))
  (other-window -1)
  (y-or-n-p (if YaTeX-japan "下線部はあってますか" "Is underline right"))
)

(defun YaTeX-generate-register-match ()
  (nconc arg (list (list beg end)))
  (let ((x beg))
    (while (< x end) (aset used x i)(setq x (1+ x))))
)

(defun YaTeX-generate-display-message (mes &optional bottom)
  "Display message to generation buffer."
  (pop-to-buffer YaTeX-generate-message-buffer)
  (goto-char (point-max))
  (insert "\n\n")
  (if bottom (recenter (1- bottom)) (recenter 0))
  (insert mes)
  (other-window -1)
)

(defun YaTeX-generate-move-to-add-in-position ()
  "Move cursor where add-in function should insert string."
  (cond
   ((eq YaTeX-current-completion-type 'begin)
    (goto-char (point-min))
    (skip-chars-forward "^{")
    (setq YaTeX-env-name
	  (buffer-substring (1+ (point))
			    (progn (skip-chars-forward "^}") (point))))
    (forward-char 1))
   ((eq YaTeX-current-completion-type 'section)
    (goto-char (point-min))
    (skip-chars-forward "^{"))
   ((eq YaTeX-current-completion-type 'maketitle)
    (goto-char (point-max))
    (if (= (preceding-char) ? )
	(forward-char -1)))
   )
)

(defun YaTeX-generate-function-name ()
  (concat
   "YaTeX:"
   (cond
    ((eq YaTeX-current-completion-type 'begin) YaTeX-env-name)
    ((eq YaTeX-current-completion-type 'section) YaTeX-section-name)
    ((eq YaTeX-current-completion-type 'maketitle) YaTeX-single-command)))
)

(defun YaTeX-generate-lisp-quote (str)
  (let ((len (length str))(i 0) (quote ""))
    (while (< i len)
      (if (= (aref str i) ?\\)
	  (setq quote (concat quote "\\")))
      (if (= (aref str i) 127)
	  (setq quote (concat quote "\""))
	(setq quote (concat quote (substring str i (1+ i)))))
      (setq i (1+ i)))
    quote)
)

(defun YaTeX-generate-quote-quote (str)
  (let ((len (length str))(i 0) (quote ""))
    (while (< i len)
      (if (= (aref str i) ?\")
	  (setq quote (concat quote (char-to-string 127))))
      (setq quote (concat quote (substring str i (1+ i))))
      (setq i (1+ i)))
    quote)
)

(defun YaTeX-suppress-sparse-keymap (map)
  (let ((i ? ))
    (while (< i 127)
      (define-key map (char-to-string i) 'undefined)
      (setq i (1+ i))))
)

;;;
;; Auto-generate Function for Lispers.
;;;
(defun YaTeX-generate-read-completion-type (nth)
  (message
"Read type(%d): (S)tring (C)omplete (F)ile ([)option (P)osition co(O)rd. (q)uit" nth)
  (let ((c (read-char)))
    (cond
     ((= c ?s) 'string)
     ((= c ?c) 'completion)
     ((= c ?f) 'file)
     ((= c ?\[) 'option)
     ((= c ?p) 'oneof)
     ((= c ?o) 'coord)
     ;;((= c ?m) 'macro)
     (t        'quit)))
 )
(defun YaTeX-generate-read-completion-table ()
  (let ((i 1) cand (cands "(") (cb (current-buffer))
	(buf (get-buffer-create " *Candidates*")))
    (save-window-excursion
      (save-excursion
      (YaTeX-showup-buffer buf nil)
      (set-buffer buf)
      (erase-buffer)
      (while (string<
	      ""
	      (setq cand (read-string (format "Item[%d](RET to exit): " i))))
	(setq cands (concat cands (format "(\"%s\")\n" cand))
	      i (1+ i))
	(insert cand "\n"))
      (kill-buffer buf)))
    ;;(set-buffer cb)
    (setq YaTeX-generate-current-completion-table (concat cands ")")))
)
(defun YaTeX-generate-corresponding-paren (left)
  (cond
   ((equal left "{") "}")
   ((equal left "[") "]")
   ((equal left "(") ")")
   ((equal left "<") ">")
   ((equal left "\\begin{") "}")
   (t left))
)
(defun YaTeX-generate-create-read-string (&optional nth)
  (concat
   "(read-string \""
   (read-string (if nth (format "Prompt for argument#%d: " nth) "Prompt: "))
   ": \"\n"
   "\"" (read-string "Default: ") "\""
   ")\n")
)
(defun YaTeX-generate-create-completing-read (&optional nth)
  (prog1
      (concat
       "(completing-read \""
       (read-string
	(if nth (format "Prompt for argument#%d: " nth) "Prompt: "))
       ": \"\n"
       (format "'%s\n" (YaTeX-generate-read-completion-table))
       "nil "
       (format "%s)" (y-or-n-p "Require match? ")))
    (if nil ;;;(y-or-n-p "Do you need learning for this completion?")
	(setq YaTeX-generate-variables-for-learning
	      (cons
	       (cons (format "YaTeX-%s-%d" command (or nth 0))
		     YaTeX-generate-current-completion-table)
	       YaTeX-generate-variables-for-learning))))
)
(defun YaTeX-generate-create-read-file-name (&optional nth)
  (concat
   "(read-file-name \""
   (read-string (if nth (format "Prompt for argument#%d: " nth) "Prompt: "))
   ": \" "" \"\" t \"\")\n")
)
(defun YaTeX-generate-create-read-oneof (&optional nth readpos)
  (concat
   (if readpos
       "(YaTeX:read-position \""
     "(YaTeX:read-oneof \"")
   (read-string "Acceptable characters: " "lcr") "\")\n")
)
(defun YaTeX-generate-option-type (command)
  (let ((func (format "YaTeX:%s" command)) leftp
	(buf (get-buffer-create YaTeX-generate-buffer)) type (n 1))
    (set-buffer buf)
    (erase-buffer)
    (insert "(defun " func " ()\n  (concat\n")
    (catch 'done
      (while t
	(setq type (YaTeX-generate-read-completion-type n))
	(insert 
	 (cond
	  ;;Read string
	  ((eq type 'string)
	   (concat "\"" (setq leftp (read-string "Left parenthesis: " "("))
		   "\"\n"
		   (YaTeX-generate-create-read-string)
		   "\"" (YaTeX-generate-corresponding-paren leftp) "\"")
	   )
	  ;;Completing-read
	  ((eq type 'completion)
	   (concat "\"" (setq leftp (read-string "Left parenthesis: " "{"))
		   "\"\n"
		   (YaTeX-generate-create-completing-read)
		   "\"" (YaTeX-generate-corresponding-paren leftp) "\"")
	   )
	  ((eq type 'file)
	   (concat "\"" (setq leftp (read-string "Left parenthesis: " "("))
		   "\"\n"
		   (YaTeX-generate-create-read-file-name)
		   "\"" (YaTeX-generate-corresponding-paren leftp) "\"")
	   )
	  ((eq type 'oneof)
	   (YaTeX-generate-create-read-oneof nil t)
	   )
	  ((eq type 'option)
	   (concat "(let ((op (read-string \""
		   (read-string "Prompt: ")
		   ": \")))\n"
		   "(if (string< \"\" op)\n"
		   "    (concat \"[\" op \"]\")\n"
		   "  \"\"))\n")
	   )
	  ((eq type 'coord)
	   (concat "(YaTeX:read-coordinates \""
		   (read-string "Prompt for coordinates: ")
		   ": \")\n")
	   )
	  ((eq type 'macro)
	   (error "not yet supported")
	   )
	  (t (throw 'done t))))
	(setq n (1+ n))))
    (insert "))\n")			;close defun
    (goto-char (point-min))
    (while (not (eobp)) (lisp-indent-line) (forward-line 1))
    (eval-current-buffer)
    buf)
)
(defun YaTeX-generate-argument-type (command argc)
  "Create an argument-type add-in function."
  (interactive)
  (let ((func (format "YaTeX::%s" command)) (argp 1)
	(cb (current-buffer))
	(buf (get-buffer-create YaTeX-generate-buffer)))
    (set-buffer buf)
    (erase-buffer)
    (insert "(defun " func " (&optional argp)\n(cond\n")
    (while (<= argp argc)
      (insert (format "((equal argp %d)\n" argp))
      (setq type (YaTeX-generate-read-completion-type argp))
      (insert
       (cond
	((eq type 'string)
	 (concat (YaTeX-generate-create-read-string argp)))
	((eq type 'completion)
	 (concat (YaTeX-generate-create-completing-read argp)))
	((eq type 'oneof)
	 (YaTeX-generate-create-read-oneof))
	((eq type 'file)
	 (concat (YaTeX-generate-create-read-file-name argp)))
	(t ""))
       ")\n")
      (setq argp (1+ argp)))
    (insert "))\n")
    (goto-char (point-min))
    (while (not (eobp)) (lisp-indent-line) (forward-line 1))
    (eval-current-buffer)
    (set-buffer cb)
    (YaTeX-update-table
     (if (> argc 1) (list command argc) (list command))
     'section-table 'user-section-table 'tmp-section-table)
    buf)
)
(defun YaTeX-generate-simple (&optional command)
  "Simple but requiring some elisp knowledge add-in generator."
  (interactive)
  (setq YaTeX-generate-variables-for-learning nil)
  (or command
      (setq command
	    (completing-read
	     (format
	      "Making add-in function for (default %s): " YaTeX-section-name)
	     (append
	      section-table user-section-table tmp-section-table
	      env-table     user-env-table     tmp-env-table
	      singlecmd-table user-singlecmd-table tmp-singlecmd-table)
	     nil nil)
	    command (if (string= "" command) YaTeX-section-name command)))
  (message
   (cond
    (YaTeX-japan "(o)追加型? (a)引数型? (yatexadd.docを参照のこと) :")
    (t "(O)ption? (A)rgument?")))
  (YaTeX-showup-buffer
   (if (= (read-char) ?o)
       (YaTeX-generate-option-type command)
     (YaTeX-generate-argument-type
      command
      (string-to-int (read-string "How many arguments?: ")))) nil)
)
(provide 'yatexgen)
