;;; -*- Emacs-Lisp -*-
;;; YaTeX helper with LaTeX commands and macros.
;;; yatexhlp.el
;;; (c)1994,1998,2004 by HIROSE Yuuji.[yuuji@yatex.org]
;;; Last modified Tue Oct 19 01:13:21 2004 on firestorm
;;; $Id: yatexhlp.el,v 1.73 2006/12/24 06:17:15 yuuji Rel $

(let ((help-file (concat "YATEXHLP."
			 (cond (YaTeX-japan "jp")
			       (t "eng"))))
      (help-dir
       (cond
	((and (boundp 'site-directory) site-directory) site-directory)
	((string-match "\\.app/" doc-directory)	;For Emacs.app(Darwin)
	 (expand-file-name "../site-lisp" doc-directory))
	(YaTeX-emacs-19 (expand-file-name "../../site-lisp" doc-directory))
	(t exec-directory))))
  (defvar YaTeX-help-file
    (expand-file-name help-file help-dir)
    "*Help file of LaTeX/TeX commands or macros.")
  (defvar YaTeX-help-file-private
    (expand-file-name (concat "~/" help-file))
    "*Private help file of LaTeX/TeX macros."))

(defvar YaTeX-help-delimiter "\C-_" "Delimiter of each help entry.")
(defvar YaTeX-help-entry-map (copy-keymap YaTeX-mode-map)
  "Key map used in help entry.")
(defvar YaTeX-help-file-current nil
  "Holds help file name to which the description in current buffer should go.")
(defvar YaTeX-help-command-current nil
  "Holds command name on which the user currently write description.")
(defvar YaTeX-help-saved-config nil
  "Holds window configruation before the editing of manual.")
(defvar YaTeX-help-synopsis
  (cond (YaTeX-japan "ÅyèëéÆÅz")
	(t "[[ Synopsis ]]"))
  "Section header of synopsis.")
(defvar YaTeX-help-description
  (cond (YaTeX-japan "Åyê‡ñæÅz")
	(t "[[ Description ]]"))
  "Section header of description.")

(defvar YaTeX-help-mode-map nil "Keymap used in YaTeX-help buffer")
(if YaTeX-help-mode-map nil
  (setq YaTeX-help-mode-map (make-sparse-keymap))
  (let ((map YaTeX-help-mode-map))
    (suppress-keymap map)
    (define-key map "j" '(lambda () (interactive) (scroll-up 1)))
    (define-key map "k" '(lambda () (interactive) (scroll-up -1)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "o" 'other-window)
    (define-key map "h" 'describe-bindings)
    (define-key map "q" 'YaTeX-help-quit)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)))

(defun YaTeX-help-quit ()
  "Close help and return to privious buffer"
  (interactive)
  (bury-buffer (current-buffer))
  (set-window-configuration YaTeX-help-saved-config))

(defvar YaTeX-help-reference-regexp "<refer\\s +\\([^>]+\\)>"
  "Regexp of reference format of YaTeX-help file.")
(defvar YaTeX-help-buffer "** YaTeX HELP **" "Help buffer name for yatexhlp")

(defun YaTeX-help-entries ()
  "Return the alist which contains all the entries in YaTeX-help file."
  (let (entries entry)
    (save-excursion
      (mapcar
       (function
	(lambda (help)
	  (if (file-exists-p help)
	      (progn
		(set-buffer (find-file-noselect help))
		(save-excursion
		  (goto-char (point-min))
		  (while (re-search-forward
			  (concat "^" (regexp-quote YaTeX-help-delimiter)
				  "\\(.+\\)$") nil t)
		    (setq entry (buffer-substring
				 (match-beginning 1) (match-end 1)))
		    (or (assoc entry entries)
			(setq entries (cons (list entry) entries)))))))))
       (list YaTeX-help-file YaTeX-help-file-private)))
    entries))

(defvar YaTeX-help-entries nil
  "Helo entries alist.")
(setq YaTeX-help-entries (YaTeX-help-entries))

(defun YaTeX-help-resolve-reference (buffer1 buffer2 &optional done-list)
  "Replace reference format in buffer1 with refered contents in buffer2."
  (let (ref ref-list beg end)
    (save-excursion
      (switch-to-buffer buffer1)
      (goto-char (point-min))
      (while (re-search-forward YaTeX-help-reference-regexp nil t)
	(setq ref (buffer-substring (match-beginning 1) (match-end 1))
	      ref-list (cons (list ref) ref-list))
	(replace-match "")
	(if (assoc ref done-list) nil	;already documented.
	  (switch-to-buffer buffer2)
	  (save-excursion
	    (goto-char (point-min))
	    (if (re-search-forward
		 (concat (regexp-quote YaTeX-help-delimiter)
			 (regexp-quote ref)
			 "$") nil t)
		(progn
		  (setq beg (progn (forward-line 2) (point))
			end (progn
			      (re-search-forward
			       (concat "^" (regexp-quote YaTeX-help-delimiter))
			       nil 1)
			      (goto-char (match-beginning 0))
			      (forward-line -1)
			      (while (and (bolp) (eolp) (not (bobp)))
				(forward-char -1))
			      (point)))
		  (switch-to-buffer buffer1)
		  (insert-buffer-substring buffer2 beg end))))
	  (switch-to-buffer buffer1)))
      (if beg (YaTeX-help-resolve-reference
	       buffer1 buffer2 (append done-list ref-list))))))

(defun YaTeX-refer-help (command help-file &optional append)
  "Refer the COMMAND's help into HELP-FILE.
\[Help-file format\]
<DELIM><LaTeX/TeX command without escape character(\\)><NL>
<Synopsis><NL>
<Documentation><TERM>
Where:	<DELIM> is the value of YaTeX-help-delimiter.
	<NL> is newline.
	<TERM> is newline or end of buffer."
  (let ((hfbuf (find-file-noselect help-file))
	(hbuf (get-buffer-create YaTeX-help-buffer))
	(curwin (selected-window))
	sb se db de)
    (set-buffer hfbuf)
    (goto-char (point-min))
    (if (null
	 (let ((case-fold-search nil))
	   (re-search-forward
	    (concat (regexp-quote YaTeX-help-delimiter)
		    (regexp-quote command)
		    "$") nil t)))
	nil				;if not found, return nil
      (forward-line 1)
      (setq sb (point)
	    se (progn (forward-line 1) (point))
	    db (point)
	    de (progn
		 (re-search-forward
		  (concat "^" (regexp-quote YaTeX-help-delimiter)) nil 1)
		 (- (point) (length YaTeX-help-delimiter))))
      (YaTeX-showup-buffer
       hbuf (function (lambda (x) (nth 3 (window-edges x)))) t)
      (set-buffer hbuf)
      (setq buffer-read-only nil)
      (if append (goto-char (point-max)) (erase-buffer))
      (insert YaTeX-help-synopsis "\n")
      (insert-buffer-substring hfbuf sb se)
      (insert "\n" YaTeX-help-description "\n")
      (insert-buffer-substring hfbuf db de)
      (YaTeX-help-resolve-reference hbuf hfbuf (list (list command)))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (YaTeX-help-mode)
      (select-window curwin)
      t)))

(defun YaTeX-help-mode ()
  (interactive)
  (use-local-map YaTeX-help-mode-map)
  (setq major-mode 'yatex-help-mode
	mode-name "YaTeX-HELP"))

(defun YaTeX-help-newline (&optional arg)
  (interactive "P")
  (if (and (= (current-column) 1) (= (preceding-char) ?.) (eolp))
      (let ((cbuf (current-buffer)))
	(beginning-of-line)
	(delete-region (point) (progn (forward-line 1) (point)))
	(save-excursion
	  (YaTeX-help-add-entry
	   YaTeX-help-command-current YaTeX-help-file-current))
	(set-window-configuration YaTeX-help-saved-config)
	(bury-buffer cbuf))
    (newline arg)))

(defun YaTeX-help-add-entry (command help-file)
  (let ((hfbuf (find-file-noselect help-file))
	(dbuf (current-buffer)) beg end)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote YaTeX-help-synopsis)))
    (forward-line 1)	(setq beg (point))
    (end-of-line)	(setq end (point))
    (set-buffer hfbuf)
    (goto-char (point-min))
    (insert YaTeX-help-delimiter command "\n")
    (insert-buffer-substring dbuf beg end)
    (insert "\n")
    (set-buffer dbuf)
    (re-search-forward (concat "^" (regexp-quote YaTeX-help-description)))
    (forward-line 1)
    (setq beg (point))
    (setq end (point-max))
    (set-buffer hfbuf)
    (insert-buffer-substring dbuf beg end)
    (insert "\n\n")
    (forward-line -1)
    (delete-blank-lines)
    (let ((make-backup-files t))
      (basic-save-buffer))
    (bury-buffer hfbuf)
    (setq YaTeX-help-entries (cons (list command) YaTeX-help-entries))))

(defun YaTeX-help-prepare-entry (command help-file)
  "Read help description on COMMAND and add it to HELP-FILE."
  (let ((buf (get-buffer-create "**Description**"))
	(conf (current-window-configuration)))
    (YaTeX-showup-buffer
     buf (function (lambda (x) (nth 3 (window-edges x)))) t)
    (make-local-variable 'YaTeX-help-file-current)
    (make-local-variable 'YaTeX-help-command-current)
    (make-local-variable 'YaTeX-help-saved-config)
    (setq YaTeX-help-file-current help-file
	  YaTeX-help-command-current command
	  YaTeX-help-saved-config conf
	  mode-name "Text"
	  major-mode 'text)
    (erase-buffer)
    (insert YaTeX-help-synopsis "\n\n" YaTeX-help-description "\n\n")
    (define-key YaTeX-help-entry-map "\r" 'YaTeX-help-newline)
    (use-local-map YaTeX-help-entry-map)
    (message
     (cond (YaTeX-japan "ì¸óÕÇèIÇ¶ÇΩÇÁ . ÇÃÇ›ì¸óÕÇµÇƒRET")
	   (t "Type only `.' and RET to exit.")))))

(defun YaTeX-enrich-help (command)
  "Add the COMMAND's help to help file."
  (if (y-or-n-p (format "No help on `%s'. Create help?" command))
      (YaTeX-help-prepare-entry
       command
       (if (y-or-n-p "Add help to global documentation?")
	   YaTeX-help-file YaTeX-help-file-private))))

(defun YaTeX-help-sort (&optional help-file)
  "Sort help file HELP-FILE.
If HELP-FILE is nil or called interactively, sort current buffer
as a help file."
  (interactive)
  (if help-file (set-buffer (find-file-noselect help-file)))
  (sort-regexp-fields
   nil "\\(\\sw+\\)\\([^]+\\|\\s'\\)" "\\1" (point-min) (point-max)))

(defun YaTeX-apropos-file (keyword help-file &optional append)
  (let ((hb (find-file-noselect help-file))
	(ab (get-buffer-create YaTeX-help-buffer))
	(sw (selected-window))
	(head (concat "^" (regexp-quote YaTeX-help-delimiter)))
	pt command)
    (YaTeX-showup-buffer
     ab (function (lambda (x) (nth 3 (window-edges x)))))
    (select-window (get-buffer-window ab))
    (set-buffer ab)			;assertion
    (setq buffer-read-only nil)
    (or append (erase-buffer))
    (set-buffer hb)
    (goto-char (point-min))
    (while (re-search-forward keyword nil t)
      (setq pt (point))
      (re-search-backward head nil t)
      (setq command (buffer-substring (match-end 0) (point-end-of-line)))
      (switch-to-buffer ab)
      (goto-char (point-max))
      (insert-char ?- (1- (window-width)))
      (insert (format "\n<<%s>>\n" command))
      (YaTeX-refer-help command help-file t) ;append mode
      (setq buffer-read-only nil)
      (set-buffer hb)
      (goto-char pt)
      (if (re-search-forward head nil 1)
	  (goto-char (1- (match-beginning 0)))))
    (setq buffer-read-only t)
    (select-window sw)
    pt))

;;;###autoload
(defun YaTeX-apropos (key)
  (interactive "sLaTeX apropos (regexp): ")
  (if (string= "" key) (error "Nothing to show"))
  (setq YaTeX-help-saved-config (current-window-configuration))
  (or (YaTeX-apropos-file key YaTeX-help-file)
      (YaTeX-apropos-file key YaTeX-help-file-private t)
      (message "No matches found.")))

;;;###autoload
(defun YaTeX-help ()
  "Show help buffer of LaTeX/TeX commands or macros."
  (interactive)
  (let (p beg end command)
    (save-excursion
      (if (looking-at YaTeX-ec-regexp)
	  (goto-char (match-end 0)))
      (setq p (point))			;remember current position.
      (cond
       ((YaTeX-on-begin-end-p)
	;;if on \begin or \end, extract its environment.
	(setq command
	      (cond ((match-beginning 1)
		     (buffer-substring (match-beginning 1) (match-end 1)))
		    ((match-beginning 2)
		     (buffer-substring (match-beginning 2) (match-end 2))))))
       ((search-backward YaTeX-ec (point-beginning-of-line) t)
	(goto-char (setq beg (match-end 0)))
	(re-search-forward YaTeX-TeX-token-regexp (point-end-of-line) t)
	(setq end (point))
	(if (and (<= beg p) (<= p end))
	    (setq command (buffer-substring beg end)))))
      (if (or (string= command "begin") (string= command "end"))
	  (progn
	    (search-forward "{" (point-end-of-line))
	    (setq beg (point))
	    (search-forward "}" (point-end-of-line))
	    (setq command (buffer-substring beg (match-beginning 0)))))
      (setq command
	    (completing-read
	     "Describe (La)TeX command: "
	     YaTeX-help-entries nil nil command))
      );end excursion
    (setq YaTeX-help-saved-config (current-window-configuration))
    (or (YaTeX-refer-help command YaTeX-help-file)
	(YaTeX-refer-help command YaTeX-help-file-private)
	(YaTeX-enrich-help command))))
