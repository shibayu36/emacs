;;; -*- Emacs-Lisp -*-
;;; YaTeX hierarchy browser.
;;; yatexhie.el
;;; (c)1995 by HIROSE Yuuji [yuuji@yatex.org]
;;; Last modified Fri Jun 27 12:09:49 2003 on firestorm
;;; $Id: yatexhie.el,v 1.72 2003/12/25 04:10:54 yuuji Rel $

;; ----- Customizable variables -----
(defvar YaTeX-hierarchy-ignore-heading-regexp
  "\\$[A-Z][a-z]+: .* \\$\\|-\\*- .* -\\*-"
  "*Regexp of lines to ignore as files' headline.")

;; ----- General variables -----
(defvar YaTeX-default-TeX-extensions "\\.\\(tex\\|sty\\)")
(defvar YaTeX-hierarchy-current-main nil)
(defvar YaTeX-hierarchy-buffer-message
  (concat
   "n)ext p)rev N)extsame P)revsame u)p K)illbuf RET)select"
   (if (and YaTeX-emacs-19 window-system) " Mouse2)select" "")
   " ?)help"))
(defvar YaTeX-hierarchy-saved-wc nil "Saved window configuration.")

;; ----- Functions for parsing hierarchy -----

(defun YaTeX-all-included-files (&optional file)
  "Return all included files from FILE as a list.
If FILE is nil, use current buffer."
  (save-excursion
    (let ((include-regex (concat YaTeX-ec-regexp
				 "\\(\\(input\\)\\|"		;match#2
				 "\\(include\\)\\)\\b"))	;match#3
	  list file (cb (current-buffer)))
      (if file (set-buffer (YaTeX-switch-to-buffer file t)))
      (goto-char (point-min))
      (while (YaTeX-re-search-active-forward
	      include-regex YaTeX-comment-prefix nil t)
	(cond
	 ((match-beginning 2)		;\input, {} is optional, 1 argument
	  (skip-chars-forward " {")
	  (setq file (buffer-substring
		      (point)
		      (progn
			(skip-chars-forward
			 (concat "^ \t\n\r" YaTeX-ec-regexp "{}"))
			(point)))))
	 ((and (match-beginning 3) (looking-at "{"))
	  (skip-chars-forward "{")
	  (setq file (buffer-substring
		      (point)
		      (progn
			(forward-char -1) (forward-list 1) (1- (point)))))))
	(or (string-match YaTeX-default-TeX-extensions file)
	    (setq file (concat file ".tex")))
	(setq list (cons file list)))
      (set-buffer cb)
      (nreverse list))))

(defun YaTeX-document-hierarchy (&optional file basedir)
  "Return the document hierarchy beginning from FILE as a list.
If FILE is nil, beginning with current buffer's file."
  (setq file (or file buffer-file-name))
  (and YaTeX-search-file-from-top-directory
       (not (file-exists-p file))
       (string-match "^[^/].*/" file)
       (setq file (expand-file-name file basedir)))
  (message "Parsing [%s]..." (file-name-nondirectory file))
  (prog1
      (save-excursion
	(if (or (file-exists-p file) (null file))
	    (progn
	      (if file
		  (let ((parent buffer-file-name))
		    (YaTeX-switch-to-buffer file t)	;set buffer to file
		    (or YaTeX-parent-file
			(YaTeX-get-builtin "!")
			(setq YaTeX-parent-file parent))))
	      (cons (buffer-file-name (current-buffer))
		    (mapcar '(lambda (f) 	;return value
			       (YaTeX-document-hierarchy f basedir))
			    (YaTeX-all-included-files))))))
    (message "Parsing [%s]...done" (file-name-nondirectory file))))

;; ----- Functions for displaying hierarchy -----

(defun YaTeX-hierarchy-get-file-heading (file)
  "Get a FILE's heading."
  (save-excursion
    (set-buffer (find-file-noselect file))
    (save-excursion
      (let (p)
	(goto-char (point-min))
	(cond
	 ((re-search-forward
	   (concat YaTeX-ec-regexp YaTeX-sectioning-regexp) nil t)
	  (search-forward "{")
	  (forward-char -1)
	  (setq p (condition-case nil
		      (progn (forward-list 1) (1- (point)))
		    (error (point-end-of-line))))
	  (goto-char (1+ (match-beginning 0)))
	  (skip-chars-forward " \t\n")
	  (buffer-substring (point) (min (point-end-of-line) p)))
	 ((catch 'found
	    (while (re-search-forward "^ *%\\([^#]\\)" nil t)
	      (or (re-search-forward
		   YaTeX-hierarchy-ignore-heading-regexp
		   (point-end-of-line) t)
		  (throw 'found t))))
	  (beginning-of-line)
	  (search-forward "%")
	  (skip-chars-forward "% \t")
	  (buffer-substring (point) (point-end-of-line)))
	 (t ""))))))

(defun YaTeX-display-a-hierachy (hier level)
  "Put a HIER of document hierarchy.
LEVEL is including depth."
  (message "Formatting hierarchy buffer...")
  (let ((lastatomcol 0) list i p)
    (cond
     ((listp hier)
      (setq  list hier)
      (while list
	(YaTeX-display-a-hierachy (car list) (1+ level))
	(setq list (cdr list))))
     ((stringp hier)			;is an atom
      (insert "  ")
      (setq i level)
      (while (> i 2)
	(insert "|   ")
	(setq i (1- i)))
      (if (> level 1) (insert "+---"))
      (setq p (point))
      (insert (or (buffer-name (get-file-buffer hier))
		  (file-name-nondirectory hier)))
      (if (and window-system YaTeX-emacs-19)
	  (put-text-property p (point) 'mouse-face 'underline))
      (insert " ")
      (indent-to-column (1- (/ (window-width) 2)))
      (insert "% " (YaTeX-hierarchy-get-file-heading hier))
      (insert "\n"))))
  (message "Formatting hierarchy buffer..."))

(defun YaTeX-display-hierarchy (file &optional use-default)
  "Display document hierarchy that is beginning from FILE."
  (interactive "P")
  (setq YaTeX-hierarchy-saved-wc
	(list (current-window-configuration)
	      (and (featurep 'windows)
		   (boundp 'win:current-config)
		   win:current-config)))
  (let*((b-in (YaTeX-get-builtin "!"))
	default)
    ;;むーん↓このへんの仕様どうしたらいいか良く分からん...
    (if default (setq default (expand-file-name default)))
    (YaTeX-visit-main t)		;move to parent file
    (setq default buffer-file-name)
    (setq file
	  (or (if use-default default file)
	      (read-file-name
	       (format
		"Main .tex file%s: "
		(if default
		    (format "(default %s)"(file-name-nondirectory default))
		  ""))
	       "" default 1))))
  (setq file (expand-file-name file))
  (setq YaTeX-hierarchy-current-main file)
  (let ((dbuf "*document hierarchy*")
	(topdir default-directory))
    (YaTeX-showup-buffer dbuf nil t)
    (set-buffer (get-buffer dbuf))
    (setq truncate-lines t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (YaTeX-display-a-hierachy (YaTeX-document-hierarchy file topdir) 0))
    (goto-char (point-min))
    (YaTeX-hierarchy-next 0)
    (set-buffer-modified-p nil)
    (YaTeX-hierarchy-mode)
    ))

(defun YaTeX-display-hierarchy-directly ()
  "Same as YaTeX-display-hierarchy.  Call from mouse."
  (interactive)
  (YaTeX-display-hierarchy nil t))

(defun YaTeX-hierarchy-mode ()
  "Major mode to browse and select document hierarchy.

\\[YaTeX-hierarchy-next]	next line
\\[YaTeX-hierarchy-prev]	previous line
\\[YaTeX-hierarchy-forward]	move forward in the same level
\\[YaTeX-hierarchy-backward]	move backward in the same level
\\[YaTeX-hierarchy-up-document]	move to parent file
\\[delete-other-windows]	delete other windows
\\[other-window]	other window
\\[shrink-window]	shrink window
\\[enlarge-window]	enlarge window
\\[YaTeX-hierarchy-show]	show file contents in the next window
\\[YaTeX-hierarchy-scroll-up]	scroll up file contents buffer
\\[YaTeX-hierarchy-scroll-down]	scroll down file contents buffer
\\[YaTeX-hierarchy-top]	show the top of file contents
\\[YaTeX-hierarchy-bottom]	show the bottom of file contents
\\[YaTeX-hierarchy-lastpos]	return to the previous position
\\[YaTeX-hierarchy-select]	select file
\\[YaTeX-hierarchy-mouse-select]	select
"
  (setq major-mode 'YaTeX-hierarchy-mode
	mode-name "YaTeX hier")
  (use-local-map YaTeX-hierarchy-mode-map)
  (setq buffer-read-only t)
  (message YaTeX-hierarchy-buffer-message))

;; ----- Subfunctions for interactive functions -----
(defun YaTeX-hierarchy-get-current-file-buffer ()
  "Return the buffer associated with current line's file."
  (let ((file (buffer-substring
	       (point)
	       (save-excursion
		 (skip-chars-forward "^ \t" (point-end-of-line)) (point))))
	(hilit-auto-highlight) buffer)
    (set-buffer (find-file-noselect YaTeX-hierarchy-current-main))
    (if (get-buffer file)		;buffer is active
	(setq buffer (get-buffer file)) ;may contain `<2>'
      (if (string-match "<[2-9]>$" file)
	  (setq file (substring file 0 -3)))
      (save-excursion
	(setq buffer (YaTeX-switch-to-buffer file t)))))) ; open it!

;; ----- Interactive functions -----
(defun YaTeX-hierarchy-next (arg &optional quiet)
  "Move to next line's file in YaTeX document hierarchy buffer."
  (interactive "p")
  (forward-line arg)
  (skip-chars-forward "- +\\|")
  (if (and (/= arg 0) (not quiet))
      (YaTeX-hierarchy-select t))
  (message YaTeX-hierarchy-buffer-message))

(defun YaTeX-hierarchy-prev (arg)
  "Move to previous line's file in YaTeX document hierarchy buffer."
  (interactive "p")
  (YaTeX-hierarchy-next (- arg)))

(defun YaTeX-hierarchy-next-line (arg)
  (interactive "p")
  (YaTeX-hierarchy-next arg t))

(defun YaTeX-hierarchy-prev-line (arg)
  (interactive "p")
  (YaTeX-hierarchy-next (- arg) t))

(defun YaTeX-hierarchy-forward (arg)
  "Move to forward file in same hierarchy level."
  (interactive "p")
  (YaTeX-hierarchy-next 0)
  (let ((p (point))(column (current-column)) (i (if (> arg 0) arg (- arg))))
    (if (= column 0) (error "Not on file line."))
    (while (> i 0)
      (if (catch 'found
	    (while (and (not (eobp)) (not (bobp)))
	      (forward-line (if (> arg 0) 1 -1))
	      (move-to-column column)
	      (if (looking-at "[- +\\|]") nil
		(YaTeX-hierarchy-next 0)
		(if (= (current-column) column) (throw 'found t)))
	      (beginning-of-line)))
	  nil
	(goto-char p)
	(error "No same level file."))
      (setq i (1- i)))))

(defun YaTeX-hierarchy-backward (arg)
  "Move to backward file in same hierarchy level."
  (interactive "p")
  (YaTeX-hierarchy-forward (- arg)))

(defun YaTeX-hierarchy-up-document ()
  "Up level, that is, move to parent file position."
  (interactive)
  (YaTeX-hierarchy-next 0)		;adjust column
  (let ((p (point)) (line (count-lines (point-min) (point))) column)
    (if (or (<= line 1) (< (current-column) 6))
	(message "No more parent")
      (backward-char 1)
      (or (= (char-after (point)) ?-) (error "Unexpected hierarchy buffer"))
      (setq column (current-column))
      (while (and (> line 1) (looking-at "[- +\\|]"))
	(forward-line -1)
	(move-to-column column))
      (YaTeX-hierarchy-next 0)
      (push-mark p t)
      (message "Mark set to last position"))))

(defun YaTeX-hierarchy-kill-buffer (arg)
  "Kill buffer associated with current line's file."
  (interactive "p")
  (YaTeX-hierarchy-next 0)		;move to file name column
  (if (bolp) (error "Not on file name line"))
  (let ((file (buffer-substring
	       (point)
	       (progn (skip-chars-forward "^ \t") (point)))))
    (YaTeX-hierarchy-next arg)
    (cond
     ((get-buffer file)
      (kill-buffer (get-buffer file))
      (message "Buffer [%s] was killed" file))
     (t (message "Buffer [%s] is not active." file)))))

(defun YaTeX-hierarchy-select (arg)
  "Select current line's file in YaTeX document hierarchy buffer.
If ARG is non-nil, show the buffer in the next window."
  (interactive "P")
  (beginning-of-line)
  (skip-chars-forward "- +\\|")
  (or (eolp)
      (let ((buffer (YaTeX-hierarchy-get-current-file-buffer)))
	(if buffer			;if file was found
	    (if arg
		(YaTeX-showup-buffer buffer nil)
	      (if (and YaTeX-emacs-19 window-system
		       (get-buffer-window buffer t))
		  (goto-buffer-window buffer) ;select currently displaying
		(YaTeX-switch-to-buffer-other-window buffer)))))))

(defun YaTeX-hierarchy-show ()
  "Show current line's file in the next window."
  (interactive)
  (YaTeX-hierarchy-select t))

(defun YaTeX-hierarchy-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (YaTeX-hierarchy-select nil))

(defun YaTeX-hierarchy-quit ()
  "Quit from YaTeX-hierarchy buffer and restore window configuration."
  (interactive)
  (if (or (not (featurep 'windows))
	  (car YaTeX-hierarchy-saved-wc)
	  (and (= (car (cdr YaTeX-hierarchy-saved-wc)) win:current-config)))
      (set-window-configuration (car YaTeX-hierarchy-saved-wc))
    (bury-buffer nil)))

(defun YaTeX-hierarchy-scroll-up (arg &optional action)
  "Scroll up file contents of YaTeX-hierarchy."
  (interactive "P")
  (YaTeX-hierarchy-next 0 t)
  (let*((bufname (buffer-substring
		  (point)
		  (save-excursion (skip-chars-forward "^ \t") (point))))
	(buf (get-buffer bufname))
	(cw (selected-window)))
    (cond
     ((and buf (get-buffer-window buf))
      (select-window (get-buffer-window buf)))
     ((and buf (YaTeX-showup-buffer buf nil t)) t)
     (t (YaTeX-hierarchy-select nil)))
    (unwind-protect
	(cond
	 ((eq action 'down)	(scroll-down arg))
	 ((eq action 'top)	(beginning-of-buffer))
	 ((eq action 'bottom)	(end-of-buffer))
	 ((eq action 'last)	(exchange-point-and-mark))
	 (t (scroll-up arg)))
      (select-window cw))))

(defun YaTeX-hierarchy-scroll-down (arg)
  "Scroll down file contents of YaTeX-hierarchy."
  (interactive "P")
  (YaTeX-hierarchy-scroll-up arg 'down))

(defun YaTeX-hierarchy-top ()
  "Show the top of YaTeX-hierarchy inspection buffer's."
  (interactive)
  (YaTeX-hierarchy-scroll-up nil 'top)
)

(defun YaTeX-hierarchy-bottom ()
  "Show the top of YaTeX-hierarchy inspection buffer's."
  (interactive)
  (YaTeX-hierarchy-scroll-up nil 'bottom)
)

(defun YaTeX-hierarchy-lastpos ()
  "Go to last position in YaTeX-hierarchy buffer."
  (interactive)
  (YaTeX-hierarchy-scroll-up nil 'last)
)

;; ----- Setting up keymap -----
(defvar YaTeX-hierarchy-mode-map nil "Keymap used in YaTeX-hierarchy-mode.")
(if YaTeX-hierarchy-mode-map nil
  (setq YaTeX-hierarchy-mode-map (make-sparse-keymap))
  (define-key YaTeX-hierarchy-mode-map "n"	'YaTeX-hierarchy-next)
  (define-key YaTeX-hierarchy-mode-map "p"	'YaTeX-hierarchy-prev)
  (define-key YaTeX-hierarchy-mode-map "j"	'YaTeX-hierarchy-next-line)
  (define-key YaTeX-hierarchy-mode-map "k"	'YaTeX-hierarchy-prev-line)
  (substitute-all-key-definition
   'next-line 'YaTeX-hierarchy-next-line YaTeX-hierarchy-mode-map)
  (substitute-all-key-definition
   'previous-line 'YaTeX-hierarchy-prev-line YaTeX-hierarchy-mode-map)
  (define-key YaTeX-hierarchy-mode-map "N"	'YaTeX-hierarchy-forward)
  (define-key YaTeX-hierarchy-mode-map "P"	'YaTeX-hierarchy-backward)
  (define-key YaTeX-hierarchy-mode-map "u"	'YaTeX-hierarchy-up-document)
  (define-key YaTeX-hierarchy-mode-map "K"	'YaTeX-hierarchy-kill-buffer)
  (define-key YaTeX-hierarchy-mode-map "1"	'delete-other-windows)
  (define-key YaTeX-hierarchy-mode-map "o"	'other-window)
  (define-key YaTeX-hierarchy-mode-map "-"	'shrink-window)
  (define-key YaTeX-hierarchy-mode-map "+"	'enlarge-window)
  (define-key YaTeX-hierarchy-mode-map "."	'YaTeX-hierarchy-show)
  (define-key YaTeX-hierarchy-mode-map " "	'YaTeX-hierarchy-scroll-up)
  (define-key YaTeX-hierarchy-mode-map "b"	'YaTeX-hierarchy-scroll-down)
  (define-key YaTeX-hierarchy-mode-map "\C-?"	'YaTeX-hierarchy-scroll-down)
  (define-key YaTeX-hierarchy-mode-map "\C-m"	'YaTeX-hierarchy-select)
  (define-key YaTeX-hierarchy-mode-map "<"	'YaTeX-hierarchy-top)
  (define-key YaTeX-hierarchy-mode-map ">"	'YaTeX-hierarchy-bottom)
  (define-key YaTeX-hierarchy-mode-map "'"	'YaTeX-hierarchy-lastpos)
  (define-key YaTeX-hierarchy-mode-map "g"	'YaTeX-hierarchy-select)
  (define-key YaTeX-hierarchy-mode-map "q"	'YaTeX-hierarchy-quit)
  (define-key YaTeX-hierarchy-mode-map "?"	'describe-mode)
  (if (and YaTeX-emacs-19 window-system)
      (define-key YaTeX-hierarchy-mode-map
	[mouse-2] 'YaTeX-hierarchy-mouse-select))
  )

(provide 'yatexhie)
;;end of yatexhie.el
