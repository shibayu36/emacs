;;; ALPHA ALPHA ALPHA
;;;
;;; load and eval this buffer (i.e. in your .emacs) to enable
;;; html-helper-mode customization even if the mode is not loaded.
;;;
;;; For Emacs 20.5.1 or newer (better: for those Emacsen supporting
;;; configuration

(defgroup html-helper nil
  "Customizing html-helper-mode"
  :group 'languages
  :group 'hypermedia
  :group 'local)

(defgroup html-helper-faces nil
  "Customizing html-helper-mode custom faces"
  :group 'html-helper
  :group 'faces)

;; Default distribution doesn't include visual-basic-mode
(defcustom html-helper-mode-uses-visual-basic nil
  "Non nil to require visual-basic-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-JDE nil
  "No nil to use jde instead of java-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-bold-italic nil
  "Non nil to use the bold-italic font (if your font supports it)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-KG-style nil
  "Non nil to make Emacs consider PHP/ASP code blocks beginning in 
the first column"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-global-JSP-not-ASP t
  "Non nil to make Emacs consider <% %> blocks as JSP (global default behaviour)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(progn
  (defvar html-tag-face 
    (defface html-tag-face
      '((((class color)
	  (background dark))
	 (:foreground "deep sky blue" :bold t))
	(((class color)
	  (background light))
	 (:foreground "dodger blue" :bold t))
	(t
	 (:foreground "dodger blue" :bold t)))
      "Face to use for HTML tags."
      :group 'html-helper-faces))
  (defvar html-helper-bold-face 
    (defface html-helper-bold-face
      '((((class color)
	  (background dark))
	 (:foreground "wheat" :bold t))
	(((class color)
	  (background light))
	 (:foreground "peru" :bold t))
	(t
	 (:foreground "peru" :bold t)))
      "Custom bold face."
      :group 'html-helper-faces))
  (defvar html-helper-italic-face 
    (defface html-helper-italic-face
      '((((class color)
	  (background dark))
	 (:foreground "spring green" :italic t))
	(((class color)
	  (background light))
	 (:foreground "medium sea green" :italic t))
	(t
	 (:foreground "medium sea green" :italic t)))
      "Custom italic face."
      :group 'html-helper-faces))
  (cond (html-helper-mode-uses-bold-italic
	 (defvar html-helper-bold-italic-face 
	   (defface html-helper-bold-italic-face
	     '((((class color)
		 (background dark))
		(:foreground "peachpuff" :bold t:italic t))
	       (((class color)
		 (background light))
		(:foreground "orange" :bold t :italic t))
	       (t
		(:foreground "orange" :bold t :italic t)))
	     "Custom bold italic face."
	     :group 'html-helper-faces))))
  (defvar html-helper-underline-face 
    (defface html-helper-underline-face
      '((((class color)
	  (background dark))
	 (:foreground "cornsilk" :underline t))
	(((class color)
	  (background light))
	 (:foreground "goldenrod" :underline t))
	(t
	 (:foreground "goldenrod" :underline t)))
      "Custom underline face."
      :group 'html-helper-faces)))