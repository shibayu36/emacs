; The functions in this file augment python-mode.el

; Time-stamp: <2007-06-02 08:01:19 rozen>

; It provides a slightly different execution of the current buffer and for
; engaging the python debugger.

; The main problem that I have with python-mode is that when executing
; code within emacs, it saves the current buffer into a temporary file
; with a constructed name in the tmp directory.  That won't work with
; a multi module program.

; I originally attempted to modify python-mode.el to reflect the
; behavior that I am after but got into a terrible mess of changes and
; realized it would be cleaner to just write a seperate module with
; the functions needed and connect to them by modifying the
; py-mode-map to invoke my functions. 'py-execute-prog' executes the
; program with the interpreter, while 'py-call-pdb' passes the program
; and the argument string to pdb.  These are all new functions and so
; do not clash with any functions in python-mode.el.  Of course, it
; uses functions and ideas and code from python-mode.

; When one of these new functions executes a python program, 

; 1. It first reads from the mini-buffer the name of the initial file
; to be executed by the interpreter. The file behind the current
; buffer is suggested.  The user may then substitute any file he
; wishes as the initial file to be executed by the interpreter. Of
; course, one may move up and down thru the history list to select an
; entry in the history list. The file selected will be save in a
; history list.

; 2. Next, it reads from the mini-buffer the argument string to be
; passed to the program being interpreted.  Again, if there is a
; history of arguments, the most recent will be selected.  The user
; may over ride the suggestion.  The argument string passed to the
; program will be saved in a history list.

; 3. It offers to save all the modified buffers based on a query.

; 4. Finally, it invokes python on the initial file or it invokes pdb
; on the initial file.
  
; To use, I put the following in my .emacs file and this file in my
; load path.


;(add-hook 'python-mode-hook
;       	  '(lambda ()
;	    		 (load "py-mode-ext")  
;       		 (define-key py-mode-map [f12] 'pyp)
;       		 (define-key py-mode-map "\C-c\C-c" 'py-execute-prog)
;       		 (define-key py-mode-map "\C-c\C-g" 'py-call-pdb)
;       		 (define-key py-mode-map "\C-c\C-w" 'pychecker)))

; If you want to play it safe and preserve the current operation you
; can change the "\C-c\C-c" to "\C-c\C-z" and have both behaviors.  I
; did not figure out how to modify the Python menu.  If someone could
; help me with that I would be glad to include it.
;
; I had a hard time invoking the the python debugger in emacs. The
; trick which I finally used was to (a) move pdb.py to my home
; directory and make it executable, and (b) customize
; gud-pdb-command-name to be '~/pdb.py'.
;
; I have adopted the convention of always have starting a program with 
; a main function.  I have the file '~/.pdbrc' which contains the line 
; 'break main' so that when I run the debugger it will put a breakpoint 
; at function 'main'.

; The code has several added commands that let you run around the
; trace back.  The functions are:
;
;   py-last-exception         which will take you to the source line referred
;                             to in the inner most level of the traceback.
;                             That is, the bottom or often most interesting line

;   py-previous-exception     which will take you to the source line referred
;                             to in preceding level of the trace back.

;   py-next-exception         which will take you to the source line referred
;                             to in next level of the trace back.

;   py-current-line-exception which will take you to the source line referred
;                             to in the selected line of the trace back, i.e.,
;                             select the line you want to go to in the 
;                             *Python Output* window and invoke this function.
;
; You may invoke these functions with the following key sequences:
;
;       "\C-x\C-l" 'py-last-exception
;       "\C-x\C-p" 'py-previous-exception
;       "\C-x\C-n" 'py-next-exception
;       "\C-x\C-e" 'py-current-line-exception
;
; Start with \C-x\C-l.

; The EASIST way to run around an error trace when executing a program
; is to use mouse-3.  [mouse-3] is defined to invoke
; 'py-current-line-exception.  Just go to the *Python Output* window,
; select the traceback line you want to visit with mouse-1 and push
; the mouse-3 button. This is the variation that I use most often.

; I don't consider myself skilled in writing elisp, so I welcome any
; and all comments.

(defun py-execute-prog () 
  "Invoke python on the file being edited in the current buffer using
arguments obtained from the minibuffer.  It will offer to save all of
the modified buffers before trying to execute the file."
  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
		 (cmd (concat "python"))
;		 (module (read-from-minibuffer "Starting module: " 
;										  (if (consp py-execute-module-history)
;											  (car py-execute-module-history)
;											file) nil nil
;										  '(py-execute-module-history . 1)))

         ; The following code was inserted in order to allow file name
		 ; completion in getting the module name in which to start execution.
		 ; The earlier version restarts with the last module used but does not
		 ; have file name completion.  I don't know how to do both but will 
		 ; leave the old code in case I decide I like that functioning better.
	     (file-name nil)
	     (file-dir nil)
	     (file-name (file-name-nondirectory file))
	     (file-dir (file-name-directory file))

		 (module (expand-file-name (read-file-name
 	      "Starting module: " file-dir nil nil file-name)))
         ; end of the file completion code.		  
		 
		 (input-args (read-from-minibuffer "Application args: " 
										  (if (consp py-execute-arg-history)
											  (car py-execute-arg-history)
											"") nil nil
										  '(py-execute-arg-history . 1)))
		 (args (cons 
				module (py-chop-words input-args))))
	(save-some-buffers (not py-ask-about-save) nil)
	(setq py-pdbtrack-do-tracking-p t)
	(if (get-buffer py-output-buffer)
		(kill-buffer py-output-buffer)) ; Get rid of buffer if it exists.
	(global-unset-key "\C-x\C-l" )
	(global-unset-key  "\C-x\C-p" )
	(global-unset-key  "\C-x\C-e" )
	(global-unset-key  "\C-x\C-n" )
	(global-set-key  "\C-x\C-l" 'py-last-exception)
	(global-set-key  "\C-x\C-p" 'py-previous-exception)
	(global-set-key  "\C-x\C-e" 'py-current-line-exception)
	(global-set-key  "\C-x\C-n" 'py-next-exception) 
	(define-key comint-mode-map [mouse-3] 'py-current-line-exception)
	(apply 'make-comint "Python Output" cmd nil args)
	(if (not (get-buffer py-output-buffer)) ; Don't think that this
					                        ; will ever be true
	    (message "No output.")
	  (setq py-exception-buffer (current-buffer))
	  (pop-to-buffer py-output-buffer)  
	  )))


(defun py-last-exception () 
  "Analyze the py-output-buffer for errors. This is just an indirect
   way of calling new-py-postprocess-output-buffer."
  (interactive)  
  (new-py-postprocess-output-buffer py-output-buffer))

(defun new-py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist.
The only difference between this and the original one is that it leaves the
point on the line with the error message that is shown in the source window.
That is, I commented out the save-excursion function. Rozen"
  (let (line file bol err-p)
    ;(save-excursion
      (set-buffer buf)
	  (pop-to-buffer py-output-buffer)
      (beginning-of-buffer)
      ;  [ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\) 
      ;  this is py-traceback-line-re
      (while (re-search-forward py-traceback-line-re nil t)
		(setq file (match-string 1)
			  line (string-to-int (match-string 2))
			  bol (py-point 'bol))
		(py-highlight-line bol (py-point 'eol) file line));)
    (when (and py-jump-on-exception line)
      (beep)
      (py-jump-to-exception file line)
      (setq err-p t))
    err-p))

(defun py-previous-exception ()
  "Move to previous exception in the tracback stack."
  (interactive)
  (let (line file bol err-p)
      (set-buffer py-output-buffer)
	  (pop-to-buffer py-output-buffer)
	  (beginning-of-line)
	  (re-search-backward py-traceback-line-re nil t)
	  (setq file (match-string 1)
			line (string-to-int (match-string 2))
			bol (py-point 'bol))
	  (beep)
      (py-jump-to-exception file line)))

(defun py-current-line-exception ()
  "Move to exception on the line of the traceback stack selected."
  (interactive)
  (let (line file bol err-p)
      (set-buffer py-output-buffer)
	  (beginning-of-line)
	  (re-search-forward py-traceback-line-re nil t)
	  (setq file (match-string 1)
			line (string-to-int (match-string 2))
			bol (py-point 'bol))
	  (beep)
	  (pop-to-buffer py-output-buffer)
      (py-jump-to-exception file line)))

(defun py-next-exception ()
  "Move to exception beyond the line selected in the traceback stack."
  (interactive)
  (let (line file bol err-p)
      (set-buffer py-output-buffer)
	  (pop-to-buffer py-output-buffer)
	  (end-of-line)
	  (re-search-forward py-traceback-line-re nil t)
	  (setq file (match-string 1)
			line (string-to-int (match-string 2))
			bol (py-point 'bol))
	  (beep)
      (py-jump-to-exception file line)))

(defun py-call-pdb ()       
   "Invoke pdb on the current buffer, with arguments from the mini-buffer.
    It will save all of the modified buffers before trying to execute the file.
    Note that this is a take off on py-execute-prog"
  (interactive)
  (require 'gud)
  (let* ((file (buffer-file-name (current-buffer)))
		 (cmd (concat "python"))
;		 (module (read-from-minibuffer "Starting module: " 
;										  (if (consp py-execute-module-history)
;											  (car py-execute-module-history)
;											file) nil nil
;										  '(py-execute-module-history . 1)))
         ; The following code was inserted in order to allow file name
		 ; completion in getting the module name in which to start execution.
		 ; The earlier version restarts with the last module used but does not
		 ; have file name completion.  I don't know how to do both but will 
		 ; leave the old code in case I decide I lik that functioning better.
	     (file-name nil)
	     (file-dir nil)
	     (file-name (file-name-nondirectory file))
	     (file-dir (file-name-directory file))

		 (module (expand-file-name (read-file-name
 	      "Starting module: " file-dir nil nil file-name)))
         ; end of the file completion code.		  
		 (input-args (read-from-minibuffer "Application args: " 
										  (if (consp py-execute-arg-history)
											  (car py-execute-arg-history)
											"") nil nil
										  '(py-execute-arg-history . 1)))
		 (args (concat 
				gud-pdb-command-name " " module " " input-args)))
	(save-some-buffers (not py-ask-about-save) nil) ; save changed files.
	(setq py-pdbtrack-do-tracking-p nil)
	(pdb args)))


;; Chop STRING into words separated by SPC or TAB and return a list of them.
;; Borrowed from gud-chop-words.
(defun py-chop-words (string)
  (let ((i 0) (beg 0)
	(len (length string))
	(words nil))
    (while (< i len)
      (if (memq (aref string i) '(?\t ? ))
		  (progn
			(setq words (cons (substring string beg i) words)
				  beg (1+ i))
			(while (and (< beg len) (memq (aref string beg) '(?\t ? )))
			  (setq beg (1+ beg)))
			(setq i (1+ beg)))
		(setq i (1+ i))))
    (if (< beg len)
		(setq words (cons (substring string beg) words)))
    (nreverse words)))

(defvar py-execute-arg-history nil
"History of application arguments read from the mini-buffer")

(defvar py-execute-module-history nil
"History of main modules read from the mini-buffer")

(setq pychecker-regexp-alist
  '(("\\([a-zA-Z]?:?[^:(\t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2)))
;;;###autoload
(defun pychecker ()
  "Run pychecker"
  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
		 (command (concat "pychecker " file)))
		 (save-some-buffers (not compilation-ask-about-save) nil) ; save  files.
		 (compile-internal command "No more errors or warnings" "pychecker"
						   nil pychecker-regexp-alist)))
