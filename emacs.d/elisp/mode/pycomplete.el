;;; Complete symbols at point using Pymacs.

;;; See pycomplete.py for the Python side of things and a short description
;;; of what to expect.

;;; On the net, I came across some correspondence on pycomplete including
;;; patch files that promised improved functionality. I took and applied those
;;; patches and then decided that it could be improved further.  In  this file
;;; I added the switch-to-buffer so the cursor would end up active in the 
;;; desired window.  I then tried to use delete-other-windows to make the 
;;; python buffer fill the whole frame. I could not get that to work. I would
;;; certainly appreciate any help in doing that.

(require 'pymacs)
(require 'python-mode)

(pymacs-load "pycomplete")

(defun py-complete ()
  (interactive)
  (let* ((pymacs-forget-mutability t)
         (symbol (py-symbol-near-point))
         (completions
          (pycomplete-pycomplete symbol
                                 (py-find-global-imports))))
    (cond ((null completions)           ; no matching symbol
           (message "Can't find completion for \"%s\"" symbol)
           (ding))
          ((null (cdr completions)) ; sole completion
           (insert (car completions)))
          (t
           (message "Making completion list...")
           (with-output-to-temp-buffer "*Python Completions*"
             (display-completion-list completions))
           (message "Making completion list...%s" "done")
		   (switch-to-buffer (other-buffer))   ; Rozen
   
))
))


; Rozen added the period in the regular expression below. Without it,
; it would now work with an import such as 'os.path'.
(defun py-find-global-imports ()
  (save-excursion
    (let 
		(
		 imports)
      (goto-char (point-min))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(import \\|from \\([A-Za-z_][A-Za-z_0-9.]*\\) import \\).*"
	      nil t)
		(setq imports 
			  (append imports
			      (list (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))))
      imports)))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(define-key py-mode-map "\M-\C-i"  'py-complete)

(provide 'pycomplete)
