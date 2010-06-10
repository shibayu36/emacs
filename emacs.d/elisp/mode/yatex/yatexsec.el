;;; -*- Emacs-Lisp -*-
;;; YaTeX sectioning browser.
;;; yatexsec.el
;;; (c) 1994,1998,1999,2003 by HIROSE Yuuji [yuuji@yatex.org]
;;; Last modified Fri Jun 27 12:10:34 2003 on firestorm
;;; $Id: yatexsec.el,v 1.72 2003/12/25 04:10:54 yuuji Rel $

(defvar YaTeX-sectioning-level
  '(("part" . 0)
    ("chapter" . 1)
    ("section" . 2)
    ("subsection" . 3)
    ("subsubsection" . 4)
    ("paragraph" . 5)
    ("subparagraph" . 6))
  "*Alist of LaTeX's sectioning command and its level.
This value must be written in numerically ascending order and consecutive.
Needn't define the level of `*' commands such as `section*'.")

(defvar YaTeX-sectioning-max-level
  (cdr (nth (1- (length YaTeX-sectioning-level)) YaTeX-sectioning-level))
  "*The heighest(numerically) level of sectioning command.
This must be the heighest number in YaTeX-sectioning-level.")

(defun YaTeX-sectioning-map-hide (map)
  (let ((ch ?0))
    (while (<= ch ?9)
      (define-key map (char-to-string ch) 'YaTeX-sectioning-hide)
      (setq ch (1+ ch))))
)

(defvar YaTeX-sectioning-minibuffer-map nil
  "Key map used in minibuffer for sectioning.")
(if YaTeX-sectioning-minibuffer-map nil
  (setq YaTeX-sectioning-minibuffer-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key YaTeX-sectioning-minibuffer-map "\C-p"
    'YaTeX-sectioning-up)
  (define-key YaTeX-sectioning-minibuffer-map "\C-e"
    'YaTeX-sectioning-up)
  (define-key YaTeX-sectioning-minibuffer-map "\C-i"
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-sectioning-minibuffer-map " "
    'YaTeX-minibuffer-complete)
  (define-key YaTeX-sectioning-minibuffer-map "\C-n"
    'YaTeX-sectioning-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-x"
    'YaTeX-sectioning-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-v"
    'YaTeX-sectioning-scroll-up)
  (define-key YaTeX-sectioning-minibuffer-map "\C-c"
    'YaTeX-sectioning-scroll-up)
  (define-key YaTeX-sectioning-minibuffer-map "\M-v"
    'YaTeX-sectioning-scroll-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-r"
    'YaTeX-sectioning-scroll-down)
  (define-key YaTeX-sectioning-minibuffer-map "\C-w"
    '(lambda () (interactive) (YaTeX-sectioning-scroll-down 1)))
  (define-key YaTeX-sectioning-minibuffer-map "\C-z"
    '(lambda () (interactive) (YaTeX-sectioning-scroll-up 1)))
  (define-key YaTeX-sectioning-minibuffer-map "\C-l"
    'YaTeX-sectioning-recenter)
  (define-key YaTeX-sectioning-minibuffer-map "?"
    'YaTeX-sectioning-help)
  (YaTeX-sectioning-map-hide YaTeX-sectioning-minibuffer-map)
)

(defvar YaTeX-sectioning-buffer-map nil
  "Key map used in YaTeX-sectioning-buffer.")
(if YaTeX-sectioning-buffer-map nil
  (setq YaTeX-sectioning-buffer-map (make-sparse-keymap))
  (define-key YaTeX-sectioning-buffer-map " "	'YaTeX-sectioning-buffer-jump)
  (define-key YaTeX-sectioning-buffer-map "."	'YaTeX-sectioning-buffer-show)
  (define-key YaTeX-sectioning-buffer-map (concat YaTeX-prefix "\C-c")
    'YaTeX-sectioning-buffer-jump)
  (define-key YaTeX-sectioning-buffer-map "u"	'YaTeX-shift-section-up)
  (define-key YaTeX-sectioning-buffer-map "d"	'YaTeX-shift-section-down)
  (define-key YaTeX-sectioning-buffer-map "U"   'YaTeX-shift-section-up-region)
  (define-key YaTeX-sectioning-buffer-map "D" 'YaTeX-shift-section-down-region)
  (define-key YaTeX-sectioning-buffer-map "s"	'YaTeX-sync-section-buffer)
  (define-key YaTeX-sectioning-buffer-map "n"
    'YaTeX-sectioning-buffer-next-line)
  (define-key YaTeX-sectioning-buffer-map "p"
    'YaTeX-sectioning-buffer-prev-line)
  (define-key YaTeX-sectioning-buffer-map "h"  'describe-mode)
  (define-key YaTeX-sectioning-buffer-map "o"  'other-window)
  (define-key YaTeX-sectioning-buffer-map "-"  'shrink-window)
  (define-key YaTeX-sectioning-buffer-map "+"  'enlarge-window)
  (define-key YaTeX-sectioning-buffer-map "q"  'delete-window)
  (define-key YaTeX-sectioning-buffer-map "\C-_" 'YaTeX-shift-section-undo)
  (and YaTeX-emacs-19 (boundp 'window-system) (eq window-system 'x)
       (define-key YaTeX-sectioning-buffer-map [?\C-/]
	 'YaTeX-shift-section-undo))
  (YaTeX-sectioning-map-hide YaTeX-sectioning-buffer-map)
)

(defun YaTeX-sectioning-mode ()
  "Mode for browsing document's sectioning structure.
\\[YaTeX-shift-section-up]	Shift up a sectioning command
\\[YaTeX-shift-section-down]	Shift down a sectioning command
\\[YaTeX-shift-section-up-region]	Shift up sectioning commands in region
\\[YaTeX-shift-section-down-region]	Shift down sectioning commands in region
\\[YaTeX-shift-section-undo]	Undo changes of shifting
\\[YaTeX-sync-section-buffer]	Synchronize sectioning buffer with source
\\[YaTeX-sectioning-buffer-next-line]	Next line
\\[YaTeX-sectioning-buffer-prev-line]	Previous line
\\[YaTeX-sectioning-buffer-jump]	Previous line
\\[YaTeX-sectioning-buffer-show]	Show curresponding source line
"
  (interactive)
  (setq major-mode 'YaTeX-sectioning-mode
	mode-name "sectioning")
  (use-local-map YaTeX-sectioning-buffer-map)
)

(defvar YaTeX-sectioning-buffer-parent nil)
(defun YaTeX-sectioning-buffer-jump-internal (&optional keep)
  (let ((p (point))		;save-excursion is NG because
	ptn ln)			;this function should switch buffer
    (beginning-of-line)
    (if (re-search-forward YaTeX-sectioning-regexp)
	(progn
	  (save-restriction
	    (narrow-to-region (point-beginning-of-line) (point-end-of-line))
	    (setq ptn (buffer-substring
		       (1- (match-beginning 0))
		       (progn (skip-chars-forward "^}") (1+ (point))))
		  ln (buffer-substring
		      (progn (search-forward "line:") (match-end 0))
		      (progn (skip-chars-forward "0-9") (point)))))
	  (goto-char p)
	  (YaTeX-showup-buffer YaTeX-sectioning-buffer-parent nil t)
	  (or
	   (and ln (string< "" ln)
		(progn
		  (goto-line (max 1 (1- (string-to-int ln))))
		  (and
		   (search-forward ptn nil t)
		   (goto-char (match-beginning 0)))))
	   (progn
	     (goto-char (point-max))
	     (search-backward ptn)))
	  (if keep (goto-buffer-window YaTeX-sectioning-buffer))
	  (current-buffer))
      nil)))

(defun YaTeX-sectioning-buffer-jump (&optional keep)
  "Goto corresponding sectioning unit with current line in the next window.
If optional argument KEEP is non-nil, only shows the line."
  (interactive)
  (if (and YaTeX-sectioning-buffer-parent
	   (get-buffer YaTeX-sectioning-buffer-parent))
      (YaTeX-sectioning-buffer-jump-internal keep)
    (message "No line number expression."))
)

(defun YaTeX-sectioning-buffer-show ()
  "Show corresponding sectioning unit with current line."
  (interactive)
  (YaTeX-sectioning-buffer-jump-internal t)
)

(defun YaTeX-sectioning-hide-under (n)
  "Hide sectioning commands under level N."
  (let ((cw (selected-window)))
    (YaTeX-showup-buffer YaTeX-sectioning-buffer nil t)
    (if (>= n YaTeX-sectioning-max-level)
	(progn
	  (set-selective-display nil)
	  (message "Show all."))
      (set-selective-display (1+ n))
      (if (rassq n YaTeX-sectioning-level)
	  (message "Hide lower than %s" (car (rassq n YaTeX-sectioning-level)))
	(message "")))
    (if (numberp selective-display)
	(setq mode-name (format "level %d" (1- selective-display)))
      (setq mode-name (format "all")))
    (select-window cw))
)
(defun YaTeX-sectioning-hide ()
  "Call YaTeX-sectioning-hide-under with argument according to pressed key."
  (interactive)
  (YaTeX-sectioning-hide-under (- last-command-char ?0)))

(defun YaTeX-sectioning-help ()
  "Show help of sectioning."
  (interactive)
  (let ((cw (selected-window)) sb (hb (get-buffer-create "*Help*")))
    (unwind-protect
	(progn
	  (other-window 1)
	  (setq sb (current-buffer))
	  (switch-to-buffer hb)
	  (erase-buffer)
	  (insert "===== View sectioning =====
C-p	Up sectioning level.			0	Show only \\part, 
C-n	Down sectioning level.			1	 and \\chapter,
C-v	Scroll up *Sectioning line* buffer.	2	 and \\section,
M-v	Scroll down *Sectioning line* buffer.	3	 and \\subsection,
C-z	Scroll up by 1 line.			4	 and \\subsubsection,
C-w	Scroll down by 1 line.			5	 and \\paragraph.
SPC	Complete word.				6	Show all.
TAB	Complete word.
C-l	Recenter recent line.
RET	Select.
==== End of HELP =====
")
	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (momentary-string-display "" (point-min)))
      (bury-buffer hb)
      (switch-to-buffer sb)
      (select-window cw)))
)

(defun YaTeX-sectioning-up (n)
  "Up section level.
Refers the YaTeX-read-section-in-minibuffer's local variable minibuffer-start."
  (interactive "p")
  (if (eq (selected-window) (minibuffer-window))
      (let*((command (YaTeX-minibuffer-string))
	    (aster (and (string< "" command)
			(equal (substring command -1) "*")))
	    (command (if aster (substring command 0 -1) command))
	    (alist YaTeX-sectioning-level)
	    (level 0))
	(or (assoc command alist) (error "No such sectioning command."))
	(while (not (string= (car (nth level alist)) command))
	  (setq level (1+ level)))	;I want to use `member'....
	(setq level (- level n))
	(if (or (< level 0) (>= level (length alist)))
	    (ding)
	  (YaTeX-minibuffer-erase)
	  (insert (concat (car (nth level alist)) (if aster "*" ""))))))
)

(defun YaTeX-sectioning-down (n)
  "Down section level."
  (interactive "p")
  (YaTeX-sectioning-up (- n))
)

(defun YaTeX-sectioning-scroll-up (n)
  (interactive "P")
  (let ((section-buffer YaTeX-sectioning-buffer)
	(cw (selected-window)))
    (YaTeX-showup-buffer section-buffer nil t)
    (unwind-protect
	(scroll-up (or n (- (window-height) 2)))
      (select-window cw)))
)

(defun YaTeX-sectioning-scroll-down (n)
  (interactive "P")
  (let ((section-buffer YaTeX-sectioning-buffer)
	(cw (selected-window)))
    (YaTeX-showup-buffer section-buffer nil t)
    (unwind-protect
	(scroll-down (or n (- (window-height) 2)))
      (select-window cw)))
)

(defun YaTeX-sectioning-recenter (arg)
  "Recenter `<<--' line"
  (interactive "P")
  (let ((cw (selected-window)))
    (unwind-protect
	(progn
	  (YaTeX-showup-buffer YaTeX-sectioning-buffer nil t)
	  (or (search-forward "<<--" nil t)
	      (search-backward "<<--" nil))
	  (recenter (or arg (/ (window-height) 2))))
      (select-window cw)))
)

(defvar YaTeX-sectioning-minibuffer " *sectioning*"
  "Miniuffer used for sectioning")
;;;###autoload
(defun YaTeX-read-section-in-minibuffer (prompt table &optional default delim)
  (interactive)
  (let ((minibuffer-completion-table table))
    (read-from-minibuffer
     prompt default YaTeX-sectioning-minibuffer-map))
)

(defun YaTeX-get-sectioning-level ()
  "Get section-level on the cursor."
   (cdr-safe (assoc (buffer-substring
		     (point)
		     (progn (skip-chars-forward "a-z") (point)))
		     YaTeX-sectioning-level))
)

(defvar YaTeX-sectioning-buffer "*Sectioning lines*")
(defvar YaTeX-sectioning-indent 1)
(defun YaTeX-collect-sections ()
  "Collect all the lines which contains sectioning command."
  (let ((cw (selected-window)) level indent begp (prevp 1) (prevl 1)
	(pattern (concat YaTeX-ec-regexp
			 "\\(" YaTeX-sectioning-regexp "\\)\\*?{"))
	(cb (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create YaTeX-sectioning-buffer))
      (setq buffer-read-only nil)
      (erase-buffer)
      (set-buffer cb)
      (YaTeX-showup-buffer YaTeX-sectioning-buffer) ;show buffer
      (goto-char (point-min))
      (let ((standard-output (get-buffer YaTeX-sectioning-buffer)))
	(while (re-search-forward pattern nil t)
	  (goto-char (1+ (match-beginning 0)))
	  (setq level (YaTeX-get-sectioning-level)
		begp (match-beginning 0))
	  ;;(beginning-of-line)
	  ;;(skip-chars-forward " \t")
	  (setq indent (format "%%%ds" (* level YaTeX-sectioning-indent)))
	  (princ (format indent ""))
	  (if (YaTeX-on-comment-p) (princ "%"))
	  (princ (buffer-substring begp (progn (forward-list 1) (point))))
	  (setq prevl (+ prevl (count-lines prevp (point)) -1)
		prevp (point))
	  (princ (format " (line:%d)" prevl))
	  (princ "\n")))
      (set-buffer YaTeX-sectioning-buffer)
      (make-local-variable 'YaTeX-sectioning-buffer-parent)
      (YaTeX-sectioning-mode)
      (use-local-map YaTeX-sectioning-buffer-map)
      (setq YaTeX-sectioning-buffer-parent cb)
      (if (numberp selective-display)
	  (setq mode-name (format "level %d" (1- selective-display))))
      YaTeX-sectioning-buffer))
)

(defvar YaTeX-pending-undo nil)
(defun YaTeX-section-overview ()
  "Show section overview.  Return the nearest sectioning command."
  (interactive)
  (let ((cw (selected-window)) (ln (count-lines (point-min) (point)))
	(pattern "(line:\\([0-9]+\\))")
	secbuf (command ""))
    (save-excursion
      (setq secbuf (YaTeX-collect-sections))
      (YaTeX-showup-buffer secbuf nil t)
      (set-buffer secbuf)
      (goto-char (point-max))
      (while (re-search-backward pattern nil t)
	(if (< ln (string-to-int (YaTeX-match-string 1))) nil
	  (beginning-of-line)
	  (search-forward YaTeX-ec)
	  (looking-at YaTeX-TeX-token-regexp)
	  (setq command (YaTeX-match-string 0))
	  (end-of-line)
	  (insert "  <<--")
	  (setq pattern (concat "HackyRegexp" "ForFailure"))))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t buffer-undo-list nil)
      (make-local-variable 'YaTeX-pending-undo)
      (forward-line 1)
      (if (eobp) (recenter -1) (recenter -3))
      (select-window cw)
      command))
)

;;;###autoload
(defun YaTeX-make-section-with-overview ()
  "Input sectining command with previous overview."
  (interactive)
  (insert
   YaTeX-ec
   (YaTeX-read-section-in-minibuffer
    "Sectioning(Up=C-p, Down=C-n, Help=?): "
    YaTeX-sectioning-level (YaTeX-section-overview))
   "{}")
  (forward-char -1)
)

(defun YaTeX-shifted-section (sc n)
  "Get SC's N-shifted sectioning command."
  (let (lv)
    (setq lv (- (cdr (assoc sc YaTeX-sectioning-level)) n)
	  lv (max (min YaTeX-sectioning-max-level lv) 0))
    (car (nth lv YaTeX-sectioning-level)))
)

(defun YaTeX-shift-section-up (n)
  "Shift sectioning command down by level N."
  (interactive "p")
  (let ((cb (current-buffer)) sc nsc lv)
    (if (and YaTeX-sectioning-buffer-parent
	     (get-buffer YaTeX-sectioning-buffer-parent)
	     (save-excursion
	       (beginning-of-line)
	       (skip-chars-forward "^\\\\" (point-end-of-line))
	       (YaTeX-on-section-command-p YaTeX-sectioning-regexp)))
	(save-excursion
	  (or (buffer-name (get-buffer YaTeX-sectioning-buffer-parent))
	      (error "This buffer is obsolete."))
	  (setq nsc (YaTeX-shifted-section (YaTeX-match-string 1) n))
	  (YaTeX-sectioning-buffer-jump-internal)
	  (undo-boundary)
	  (goto-char (match-beginning 0))
	  (skip-chars-forward "\\\\")
	  (delete-region
	   (point) (progn (skip-chars-forward "^*{") (point)))
	  (insert nsc)
	  (undo-boundary)
	  ;; Return to *Sectioning Lines* buffer
	  (select-window (get-buffer-window cb))
	  (beginning-of-line)
	  (let (buffer-read-only)
	    (delete-region
	     (point) (progn (skip-chars-forward " \t") (point)))
	    (indent-to-column (* (cdr (assoc nsc YaTeX-sectioning-level))
				 YaTeX-sectioning-indent))
	    (skip-chars-forward "^\\\\")
	    (delete-region
	     (1+ (point)) (progn (skip-chars-forward "^*{") (point)))
	    (insert nsc)
	    (undo-boundary))
	  (set-buffer-modified-p nil)
	  (setq YaTeX-pending-undo pending-undo-list)
	  )))
)
(defun YaTeX-shift-section-down (n)
  "Shift sectioning command down by level N."
  (interactive "p")
  (YaTeX-shift-section-up (- n))
)
(defun YaTeX-shift-section-undo (arg)
  "Undo YaTeX-shift-section-up/down."
  (interactive "p")
  (and YaTeX-sectioning-buffer-parent
       (get-buffer YaTeX-sectioning-buffer-parent)
       (equal (current-buffer) (get-buffer YaTeX-sectioning-buffer))
       (let ((cb (current-buffer))
	     (lc (if (eq last-command 'YaTeX-shift-section-undo) 'undo t)))
	 (let ((pending-undo-list YaTeX-pending-undo)
	       buffer-read-only (last-command lc))
	   (undo arg)
	   (setq YaTeX-pending-undo pending-undo-list))
	 (YaTeX-showup-buffer YaTeX-sectioning-buffer-parent)
	 (goto-buffer-window YaTeX-sectioning-buffer-parent)
	 (undo-boundary)
	 (let ((last-command lc)
	       (pending-undo-list
		(if (eq lc 'undo) YaTeX-pending-undo pending-undo-list)))
	   (undo arg)
	   (setq YaTeX-pending-undo pending-undo-list))
	 (goto-buffer-window cb)
	 (setq this-command 'YaTeX-shift-section-undo)))
)
(defun YaTeX-sync-section-buffer ()
  "Synchronize *Sectioning Lines* buffer with parent buffer."
  (interactive)
  (if (and YaTeX-sectioning-buffer-parent
	   (get-buffer YaTeX-sectioning-buffer-parent))
      (let ((cb (current-buffer)) (p (point)))
	(set-buffer (get-buffer YaTeX-sectioning-buffer-parent))
	(YaTeX-section-overview)
	(switch-to-buffer cb)
	(goto-char p)))
)
(defun YaTeX-shift-section-up-region (beg end n)
  "Shift sectioning commands in region down by level N."
  (interactive "r\np")
  (or YaTeX-sectioning-buffer-parent
      (get-buffer YaTeX-sectioning-buffer-parent)
      (error "Can't find corresponding LaTeX buffer"))
  (save-excursion
    (goto-char beg)
    (let ((cb (current-buffer)) nsc from to repllist (e (make-marker)))
      (set-marker e end)
      (while (progn (skip-chars-forward "^\\\\") (< (point) e))
	(YaTeX-on-section-command-p YaTeX-sectioning-regexp)
	(setq from (YaTeX-match-string 0)
	      nsc (YaTeX-shifted-section (YaTeX-match-string 1) n))
	(goto-char (match-beginning 0))
	(let (buffer-read-only)
	  ;(delete-region (point) (progn (beginning-of-line) (point)))
	  (delete-region (progn (beginning-of-line) (point))
			 (progn (skip-chars-forward " \t") (point)))
	  (indent-to-column (cdr (assoc nsc YaTeX-sectioning-level)))
	  (delete-region
	   (progn (skip-chars-forward "%\\\\") (point))
	   (progn (skip-chars-forward "^*{") (point)))
	  (insert nsc))
	(YaTeX-on-section-command-p YaTeX-sectioning-regexp)
	(setq to (YaTeX-match-string 0)
	      repllist (cons (cons from to) repllist))
	(forward-line 1))
      (YaTeX-showup-buffer YaTeX-sectioning-buffer-parent)
      (goto-buffer-window YaTeX-sectioning-buffer-parent)
      (save-excursion
	(goto-char (point-max))
	(undo-boundary)
	(while repllist
	  (if (search-backward (car (car repllist)) nil t)
	      (progn
		(goto-char (match-beginning 0))	;confirm
		(delete-region (point) (match-end 0))
		(insert (cdr (car repllist)))
		(goto-char (match-beginning 0))))
	  (setq repllist (cdr repllist))))
      (goto-buffer-window cb)))
)
(defun YaTeX-shift-section-down-region (beg end n)
  "Shift sectioning commands in region down by level N."
  (interactive "r\np")
  (YaTeX-shift-section-up-region beg end (- n))
)
(defun YaTeX-sectioning-buffer-next-line (n)
  "Move to next line in *Sectioning Lines* buffer."
  (interactive "p")
  (forward-line n)
  (skip-chars-forward " \t%")
)
(defun YaTeX-sectioning-buffer-prev-line (n)
  "Move to previous line in *Sectioning Lines* buffer."
  (interactive "p")
  (YaTeX-sectioning-buffer-next-line (- n))
)
(provide 'yatexsec)
