; This is a function which simplifies the insertion of debugging
; print statements in a python program.
;
; To make better use of this function, I added the following line 
; to my .emacs files 
;       		 (define-key py-mode-map [f12] 'pyp)
; augmenting the following add-hook function. The other lines are for
; using my pyton mode extensions. See http://page.sourceforge.net.
; Anyway, I just hit f12 and then enter the expression I want to print 
; and follow with <RET>

;(add-hook 'python-mode-hook
;       	  '(lambda ()
;       		 (define-key py-mode-map [f12] 'pyp)))


(defun pyp (expr)
  "Insert a print statement as the next statement of the program.
   Include the name of the enclosing function  and accept from
   the minibuffer the expresion to be printed. The form of the
   statement to be entered and indented is:
      print 'function: expr =', expr
   If the print statement preceeds any 'def' statement, then the
   value of function will be 'module-level'.  Note also that the
   function name is not necessarily an eclosing function but the 
   most recent function name. Remember that the idea of having the
   function name is to help you find and identify the print statement."
  (interactive "sExpression to be printed: ")
  (save-excursion
	; The previous vesion of pyp did not correctly handle nested functions 
	; I discovered that when I started using more nested functions.
	; In an effort to get things working a bit better, I substitiuted the
	; following two lines for the third which is now commented our. 5/24/06

	; Modified this code to put in an enclosing class name if exists 1/15/07
	(condition-case nil
		(py-mark-def-or-class)
		(error (push-mark (point-min))))
	(exchange-point-and-mark)
	;(py-beginning-of-def-or-class)
	(if (= (point) (point-min))
		(setq pyp-funct "module-level")
	(re-search-forward "\\([ ]*\\)def[ ]+\\(\\w+\\)\\W")
	(setq pyp-funct (buffer-substring (match-beginning 2) (match-end 2))))
	; Try to find enclosing class
    (setq pyp-class "")
	(condition-case nil
		(py-mark-def-or-class 'class)
		(error (push-mark (point-min))))
	(exchange-point-and-mark)
	(if (= (point) (point-min))
		(setq pyp-class "")
	(re-search-forward "[ ]*class[ ]+\\(\\w+\\)\\W")
	(setq pyp-class (buffer-substring (match-beginning 1) (match-end 1)))))
  (end-of-line)
  (newline)
  (setq user (getenv "USER"))
  (if (string= pyp-class "")
     (insert "print '" pyp-funct ": " expr " =', " expr "    # "  user "   pyp")
   (insert "print '" pyp-class ": " pyp-funct ": " expr " =', " expr "    # " user " pyp"))
  (indent-for-tab-command)
)

; For more complex python data structures like lists or dictionaries, 
; pretty printing is often more appropiate than just a simple print.

(defun ppyi ()	
  "Insert the code necessary to import pprint and create the necessary
   PrettyPrinter object at the current point."
  (interactive)
  (newline)
  (setq user (getenv "USER"))
  (insert "import pprint # " user "  ppyi")
  (indent-for-tab-command)
  (newline)
  (insert "pp = pprint.PrettyPrinter()")
  (indent-for-tab-command)
)

(defun ppy (variable)
  "Insert statements following the next statements of the program which 
   will pretty print the variable. Note that this is a variable and not
   an expression."
  (interactive "sVariable to be printed: ")
  (save-excursion	
	; See comment above for modivation of change.
	(condition-case nil
		(py-mark-def-or-class)
		(error (push-mark (point-min))))
	(exchange-point-and-mark)
	;(py-beginning-of-def-or-class)
	(if (= (point) (point-min))
		(setq pyp-funct "module-level")
	(re-search-forward "\\([ ]*\\)def[ ]+\\(\\w+\\)\\W")
	(setq pyp-funct (buffer-substring (match-beginning 2) (match-end 2))))
	; Try to find enclosing class
    (setq pyp-class "")
	(condition-case nil
		(py-mark-def-or-class 'class)
		(error (push-mark (point-min))))
	(exchange-point-and-mark)
	(if (= (point) (point-min))
		(setq pyp-class "")
	(re-search-forward "[ ]*class[ ]+\\(\\w+\\)\\W")
	(setq pyp-class (buffer-substring (match-beginning 1) (match-end 1)))))
  (end-of-line)
  (newline)
  (setq user (getenv "USER"))
  (if (string= pyp-class "")
	  (insert "print '" pyp-funct ": pretty printing " variable "'  # " user " ppy")
	(insert "print '"  pyp-class ": " pyp-funct ": pretty printing " variable "'  # " user " ppy"))
  (indent-for-tab-command)
  (newline)
  (insert "pp.pprint(" variable ")")
  (indent-for-tab-command)
)