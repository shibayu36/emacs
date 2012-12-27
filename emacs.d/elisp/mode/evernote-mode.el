;;
;;  Copyright 2010 Yusuke Kawakami
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;
;; evernote-mode home page is at:
;; Author: Yusuke Kawakami
;; Version: 0.14
;; Keywords: tools

;; This emacs lisp offers the interactive functions to open, edit, and update notes of Evernote.
;; The minor mode Evernote-mode is applied to the buffer editing a note of Evernote.
;;
;; Please copy this file into emacs lisp library directory or place it in
;; a directory (for example "~/lisp") and write $HOME/.emacs like this.
;;
;;      (add-to-list 'load-path "~/lisp")
;;      (require 'evernote-mode)
;;      (global-set-key "\C-cec" 'evernote-create-note)
;;      (global-set-key "\C-ceo" 'evernote-open-note)
;;      (global-set-key "\C-ces" 'evernote-search-notes)
;;      (global-set-key "\C-ceS" 'evernote-do-saved-search)
;;      (global-set-key "\C-cew" 'evernote-write-note)
;;      (global-set-key "\C-cep" 'evernote-post-region)
;;
;; There is one hooks, evernotes-mode-hook.
;; The usage of the hook is shown as follows.
;;
;; (setq evernote-mode-hook
;;   '(lambda ()
;;      (...)))

;;; Code

(defvar evernote-mode nil
  "Non-nil if Evernote mode is enabled.")
(make-variable-buffer-local 'evernote-mode)

(defvar evernote-note-guid nil
  "Note guid of the buffer")
(make-variable-buffer-local 'evernote-note-guid)

(defvar evernote-note-name nil
  "Note name of the buffer")
(make-variable-buffer-local 'evernote-note-name)

(defvar evernote-note-tags nil
  "Tags of the buffer")
(make-variable-buffer-local 'evernote-note-tags)

(defvar evernote-note-edit-mode nil
  "Edit mode of the buffer")
(make-variable-buffer-local 'evernote-note-edit-mode)

(defvar evernote-mode-info-for-changing-major-mode nil
  "Temporal values used when changing the major mode")

(defvar evernote-mode-map (make-sparse-keymap)
  "Keymap used in evernote mode.")
(define-key evernote-mode-map "\C-x\C-s" 'evernote-save-note)
(define-key evernote-mode-map "\C-cet"   'evernote-edit-tags)
(define-key evernote-mode-map "\C-cee"   'evernote-change-edit-mode)
(define-key evernote-mode-map "\C-cer"   'evernote-rename-note)
(define-key evernote-mode-map "\C-ced"   'evernote-delete-note)

(defvar evernote-read-note-map
  (copy-keymap minibuffer-local-completion-map))
(define-key evernote-read-note-map [tab] 'evernote-read-note-completion)
(define-key evernote-read-note-map "\C-i" 'evernote-read-note-completion)
(define-key evernote-read-note-map "\C-m" 'evernote-read-note-finish)
(define-key evernote-read-note-map  " "   'self-insert-command)


(defvar evernote-search-mode-map (copy-keymap global-map)
  "Keymap used in evernote search mode.")
(define-key evernote-search-mode-map "\C-m" 'evernote-select-note-in-search-mode)

(defvar evernote-search-mode-formatted-name-displayed-name-alist nil
  "Alist from formatted names to names used only in evernote-search-mode buffer")
(make-variable-buffer-local 'evernote-search-mode-formatted-name-displayed-name-alist)


(defconst evernote-command-output-buffer-name "*Evernote-Client-Output*")
(defconst evernote-error-ok                  0)
(defconst evernote-error-fail                100)
(defconst evernote-error-parse               101)
(defconst evernote-error-unknown             1)
(defconst evernote-error-bad-data-format     2)
(defconst evernote-error-permission-denied   3)
(defconst evernote-error-internal-error      4)
(defconst evernote-error-data-required       5)
(defconst evernote-error-limit-reached       6)
(defconst evernote-error-quota-reached       7)
(defconst evernote-error-invalid-auth        8)
(defconst evernote-error-auth-expired        9)
(defconst evernote-error-data-conflict      10)
(defconst evernote-error-enml-validation    11)
(defconst evernote-error-shared-unavailable 12)


;;
;; Interactive functions.
;;


(defun evernote-open-note ()
  "Open a note"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (evernote-open-note-common
      (evernote-command-get-note-list-from-tags
       (evernote-completing-read-multiple
        "Tags used for search (comma separated form. default search all tags):"
        (evernote-get-tag-alist
         (evernote-command-get-tag-list))
        nil t)))
     t)))


(defun evernote-search-notes ()
  "Search notes with query and open a note among them"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (evernote-open-note-common
      (evernote-command-get-note-list-from-query
       (read-string "Query:"))
      t)
     t)))


(defun evernote-do-saved-search ()
  "Do a saved search and open a note"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (evernote-open-note-common
      (evernote-command-get-note-list-from-query
       (let (search-alist)
         (setq search-alist
               (evernote-get-search-alist
                (evernote-command-get-search-list)))
         (evernote-assoc-cdr
          'query
          (evernote-assoc-cdr
           (completing-read
            "Saved search:"
            search-alist
            nil t)
           search-alist))))
      t)
     t)))


(defun evernote-open-note-common (note-command-output &optional display-completion)
  "Common procedure of opening a note"
  (let (note-attr note-guid note-name note-edit-mode note-tags opened-buf)
    (setq note-attr (evernote-read-note-attr note-command-output display-completion))
    (setq note-guid (evernote-assoc-cdr 'guid note-attr)
          note-name (evernote-assoc-cdr 'name note-attr)
          note-edit-mode (evernote-assoc-cdr 'edit-mode note-attr)
          note-tags (evernote-assoc-cdr 'tags note-attr))
    (setq opened-buf (evernote-find-opened-buffer note-guid))
    (if opened-buf
        (evernote-move-cursor-to-window opened-buf)
      (progn
        (evernote-create-note-buffer
         note-guid
         note-name
         note-tags
         note-edit-mode
         (evernote-command-get-note-content note-guid note-edit-mode))))))


(defun evernote-save-note ()
  "Save a note"
  (interactive)
  (if (and evernote-note-guid (buffer-modified-p))
      (evernote-command-with-auth
       (lambda ()
         (evernote-command-update-note
          (current-buffer)
          evernote-note-guid
          evernote-note-name
          evernote-note-tags
          evernote-note-edit-mode)
         (set-buffer-modified-p nil)
         t))
    (message "(No changes need to be saved)")))


(defun evernote-create-note ()
  "Create a note"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (let (tags name edit-mode)
       (setq tags
             (evernote-completing-read-multiple
              "Attached Tags (comma separated form):"
              (evernote-get-tag-alist
               (evernote-command-get-tag-list))))
       (setq name (read-string "Note name:"))
       (setq edit-mode "TEXT")
       (switch-to-buffer (generate-new-buffer name))
       (evernote-command-create-note (current-buffer)
                                     name
                                     tags
                                     edit-mode)
       (evernote-change-major-mode-from-note-name name)
       (evernote-mode (evernote-eval-command-result)
                      name
                      tags
                      edit-mode)
       (set-buffer-modified-p nil))
     t)))


(defun evernote-write-note ()
  "Write buffer to a note"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (let (tags name edit-mode)
       (setq tags
             (evernote-completing-read-multiple
              "Attached Tags (comma separated form):"
              (evernote-get-tag-alist
               (evernote-command-get-tag-list))))
       (setq name (read-string "Note name:" (buffer-name)))
       (setq edit-mode (evernote-read-edit-mode "TEXT"))
       (evernote-command-create-note (current-buffer)
                                     name
                                     tags
                                     edit-mode)
       (set-visited-file-name nil)
       (evernote-change-major-mode-from-note-name name)
       (setq evernote-note-guid (evernote-eval-command-result)
             evernote-note-name name
             evernote-note-tags tags
             evernote-note-edit-mode edit-mode)
       (if (not evernote-mode)
           (evernote-mode))
       (rename-buffer name t)
       (evernote-update-mode-line)
       (set-buffer-modified-p nil))
     t)))


(defun evernote-post-region (begin end arg)
  "Post the region as a note"
  (interactive "r\np")
  (evernote-command-with-auth
   (lambda ()
     (let (tags name edit-mode)
       (setq tags
             (evernote-completing-read-multiple
              "Attached Tags (comma separated form):"
              (evernote-get-tag-alist
               (evernote-command-get-tag-list))))
       (setq name (read-string "Note name:" (buffer-name)))
       (setq edit-mode (evernote-read-edit-mode "TEXT"))
       (save-excursion
         (save-restriction
           (narrow-to-region begin end)
           (evernote-command-create-note (current-buffer)
                                         name
                                         tags
                                         edit-mode)))
       (if (and (not (eq arg nil)) (not (eq arg 1)))
           (evernote-create-note-buffer
            (evernote-eval-command-result)
            name
            tags
            edit-mode
            (buffer-substring begin end))))
     t)))


(defun evernote-edit-tags ()
  "Add or remove tags from/to the note"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (if evernote-mode
         (progn
           (setq evernote-note-tags
                 (evernote-completing-read-multiple
                  "Change attached Tags (comma separated form):"
                  (evernote-get-tag-alist (evernote-command-get-tag-list))
                  nil nil (mapconcat #'identity evernote-note-tags ",")))
           (evernote-update-mode-line)
           (set-buffer-modified-p t)))
     t)))


(defun evernote-change-edit-mode ()
  "Change edit mode of the note"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (if evernote-mode
         (progn
           (setq evernote-note-edit-mode
                 (evernote-read-edit-mode evernote-note-edit-mode))
           (evernote-update-mode-line)
           (set-buffer-modified-p t)))
     t)))


(defun evernote-rename-note ()
  "Rename a note"
  (interactive)
  (if evernote-mode
      (progn
        (setq evernote-note-name
              (read-string "New note name:" evernote-note-name))
        (rename-buffer evernote-note-name t)
        (evernote-change-major-mode-from-note-name evernote-note-name)
        (set-buffer-modified-p t))))


(defun evernote-delete-note ()
  "Delete a note"
  (interactive)
  (if (and evernote-mode
           (y-or-n-p "Do you really want to remove this note? "))
      (evernote-command-with-auth
       (lambda ()
         (evernote-command-delete-note evernote-note-guid)
         (kill-buffer (current-buffer))
         t))))


(defun evernote-create-search ()
  "Create a saved search"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (evernote-command-create-search
      (read-string "Saved Search Name:")
      (read-string "Query:"))
     t)))


(defun evernote-edit-search ()
  "Create a saved search"
  (interactive)
  (evernote-command-with-auth
   (lambda ()
     (let (search-alist search-attr)
       (setq search-alist
               (evernote-get-search-alist
                (evernote-command-get-search-list)))
       (setq search-attr (evernote-assoc-cdr
                          (completing-read
                           "Saved search:"
                           search-alist
                           nil t)
                          search-alist))
       (evernote-command-update-search
        (evernote-assoc-cdr 'guid search-attr)
        (read-string "New Saved search name:"
                     (evernote-assoc-cdr 'name search-attr))
        (read-string "New Query:"
                     (evernote-assoc-cdr 'query search-attr))))
     t)))


;;
;; Helper functions.
;;

;;(defun evernote-command-with-auth (func &rest args)
;;  "Add or remove tags from/to the note"
;;  (let ((error-code (catch 'error (apply func args))))
;;    (if (or (eq error-code evernote-error-invalid-auth)
;;            (eq error-code evernote-error-auth-expired))
;;        (let ((error-code (catch 'error (evernote-command-login))))
;;          (if (eq error-code t)
;;              (apply func args)
;;            (message "%s (%s)"
;;                     (evernote-error-message error-code)
;;                     (evernote-get-first-line
;;                      (evernote-get-command-result)))))
;;      (message "%s (%s)"
;;               (evernote-error-message error-code)
;;               (evernote-get-first-line
;;                (evernote-get-command-result))))))


(defun evernote-command-with-auth (func &rest args)
  "Add or remove tags from/to the note"
  (let ((error-code (catch 'error (apply func args))))
    (cond
     ((eq error-code t)
      t)
     ((or (eq error-code evernote-error-invalid-auth)
          (eq error-code evernote-error-auth-expired))
      (let ((error-code (catch 'error (evernote-command-login))))
        (if (eq error-code t)
            (progn
              (let ((error-code (catch 'error (apply func args))))
                (if (not (eq error-code t))
                    (evernote-output-error-message error-code))))
          (evernote-output-error-message error-code))))
     (t
      (evernote-output-error-message error-code)))))


(defun evernote-output-error-message (error-code)
  (message "%s (%s)"
           (evernote-error-message error-code)
           (evernote-get-first-line
            (evernote-get-command-result))))


(defun evernote-get-tag-alist (tag-command-out)
  "Get the alist for completion from command output"
  (let (acc collect-tagname)
    (setq collect-tagname
          (lambda (node)
            (setq acc
                  (cons
                   (list (evernote-assoc-cdr 'name node))
                   acc))
            (mapc collect-tagname (evernote-assoc-cdr 'children node))))
    (mapcar collect-tagname tag-command-out)
    (nreverse acc)))


(defun evernote-get-search-alist (search-command-out)
  "Get the alist for completion from command output"
  (mapcar
   (lambda (elem)
     (cons
      (evernote-assoc-cdr 'name elem)
      elem))
   search-command-out))


(defun evernote-read-note-attr (note-command-out &optional display-completion)
  "Prompts a note name and returns a note attribute"
  (let ((name-num-hash (make-hash-table :test #'equal))
        evernote-note-displayed-name-attr-alist ; used in evernote-search-mode
        evernote-note-displayed-name-formatted-name-alist) ; used in evernote-search-mode
    (mapc
     (lambda (attr)
       (let (name displayed-name)
         (setq name (evernote-assoc-cdr 'name attr))
         (setq displayed-name
               (evernote-get-displayed-note-name name name-num-hash))
         (setq evernote-note-displayed-name-attr-alist
               (cons (cons displayed-name attr)
                     evernote-note-displayed-name-attr-alist))
         (setq evernote-note-displayed-name-formatted-name-alist
               (cons (cons displayed-name
                           (format "%-30s   %-15s   %s"
                                   (evernote-assoc-cdr 'updated attr)
                                   (evernote-get-tag-list-string
                                    (evernote-assoc-cdr 'tags attr)
                                    15)
                                   displayed-name))
                     evernote-note-displayed-name-formatted-name-alist))))
     note-command-out)
    (setq evernote-note-displayed-name-attr-alist
          (nreverse evernote-note-displayed-name-attr-alist))
    (setq evernote-note-displayed-name-formatted-name-alist
          (nreverse evernote-note-displayed-name-formatted-name-alist))
    (if display-completion
        (evernote-display-note-completion-buf
         evernote-note-displayed-name-formatted-name-alist))
    (evernote-assoc-cdr (read-from-minibuffer "Note:"
                                              nil evernote-read-note-map)
                        evernote-note-displayed-name-attr-alist)))


(defun evernote-get-displayed-note-name (name name-hash)
  "Get displayed note name from the read note name"
  (let ((num (gethash name name-num-hash))
        result)
    (if num
        (progn
          (setq num (+ num 1))
          (setq result (format "%s(%d)" name num))
          (puthash name num name-num-hash))
      (setq result (substring name 0))
      (puthash name 1 name-num-hash))
    result))


(defun evernote-read-note-completion ()
  "Complete note name and display completion list"
  (interactive)
  (let (word result start)
    (setq word (evernote-get-minibuffer-string))
    (setq result (try-completion word evernote-note-displayed-name-attr-alist))
    ;(evernote-minibuffer-tmp-message (concat "[" word "]"))
    (cond
     ((eq result t)
      (evernote-minibuffer-tmp-message "[Sole Completion]"))
     ((eq result nil)
      (ding)
      (evernote-minibuffer-tmp-message "[No Match]"))
     ((string= result word)
      (evernote-display-note-completion-buf
       evernote-note-displayed-name-formatted-name-alist
       word))
     (t (evernote-set-minibuffer-string result)
        (end-of-buffer)
        (if (eq t
                (try-completion result
                                evernote-note-displayed-name-attr-alist))
            nil
          (evernote-minibuffer-tmp-message "[Complete, but not unique]"))))))


(defun evernote-read-note-finish ()
  "Finish input note name"
  (interactive)
  (if (assoc
       (evernote-get-minibuffer-string)
       evernote-note-displayed-name-attr-alist)
      (progn
        (let ((completion-buf (get-buffer "*Evernote-Completions*")))
          (if completion-buf
              (kill-buffer completion-buf)))
        (exit-minibuffer))
    (evernote-minibuffer-tmp-message "[No Match]")))


(defun evernote-display-note-completion-buf (displayed-name-formatted-name-alist &optional word)
  (let (formatted-name-displayed-name-alist completion-buf)
    (setq formatted-name-displayed-name-alist
          (mapcar (lambda (displayed-name)
                    (cons
                     (evernote-assoc-cdr
                      displayed-name
                      evernote-note-displayed-name-formatted-name-alist)
                     displayed-name))
                  (all-completions
                   (if word
                       word
                     "")
                   evernote-note-displayed-name-formatted-name-alist)))
    (save-excursion
      (setq completion-buf (get-buffer-create "*Evernote-Completions*"))
      (set-buffer completion-buf)
      (evernote-display-note-completion-list
       formatted-name-displayed-name-alist)
      (setq evernote-search-mode-formatted-name-displayed-name-alist
            formatted-name-displayed-name-alist)
      (evernote-search-mode))
    (display-buffer completion-buf)))


(defun evernote-display-note-completion-list (formatted-name-displayed-name-alist)
  "Display formatted note names on this buffer"
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (format "total %d\n%-30s   %-15s   %s\n\n"
                  (length formatted-name-displayed-name-alist)
                  "Last Modified"
                  "Tags"
                  "Title"))
  (mapc (lambda (elem)
          (insert (car elem) "\n"))
        formatted-name-displayed-name-alist)
  (setq buffer-read-only t))


(defun evernote-select-note-in-search-mode ()
  "Select a note name on this buffer and input it into the mini buffer"
  (interactive)
  (if (active-minibuffer-window)
      (save-excursion
        (let (displayed-name)
          (setq displayed-name
                (evernote-assoc-cdr
                 (evernote-get-current-line-string)
                 evernote-search-mode-formatted-name-displayed-name-alist))
          (if displayed-name
              (progn
                (kill-buffer (current-buffer))
                (evernote-set-minibuffer-string displayed-name)
                (exit-minibuffer)))))
    (kill-buffer (current-buffer))))


(defun evernote-find-opened-buffer (guid)
  "Find a buffer associated with guid"
  (let ((found_buf nil))
    (save-excursion
      (mapc (lambda (buf)
              (set-buffer buf)
              (if (string= evernote-note-guid guid)
                  (setq found_buf buf)))
            (buffer-list)))
    found_buf))


(defun evernote-move-cursor-to-window (buf)
  "Move cursor to the window associated with the bufer"
  (let ((buf-window (get-buffer-window buf)))
    (if buf-window
        (select-window buf-window)
      (pop-to-buffer buf))))


(defun evernote-create-note-buffer (guid name tags edit-mode content)
  "Create new buffer for the note"
  (save-excursion
    (let ((buf (generate-new-buffer name)))
      (set-buffer buf)
      (insert content)
      (evernote-change-major-mode-from-note-name name)
      (evernote-mode guid name tags edit-mode)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (pop-to-buffer buf))))


(defun evernote-get-tag-list-string (tags maxlen)
  (truncate-string-to-width (mapconcat #'identity tags ",") maxlen))


(defun evernote-read-edit-mode (default)
  (completing-read "Edit mode (type \"TEXT\" or \"XHTML\"):"
                   '(("TEXT") ("XHTML"))
                   nil t default))


(defun evernote-string-to-oct (string)
  "Convert the string into quoted backslashed octal edit mode."
  (concat
   "\""
   (apply 'concat (mapcar (lambda (string)
                            (format "\\%03o" string))
                          (mapcar 'identity (encode-coding-string string 'utf-8))))
   "\""))


(defun evernote-assoc-cdr (key alist)
  (let ((result-cons (assoc key alist)))
    (if result-cons
        (cdr result-cons)
      nil)))


(defun evernote-get-current-line-string ()
  (save-excursion
    (buffer-substring
     (progn
       (beginning-of-line)
       (point))
     (progn
       (end-of-line)
       (point)))))


(defun evernote-get-minibuffer-string ()
  (save-excursion
    (evernote-set-buffer-to-minibuffer)
    (buffer-substring
     (progn
       (goto-char (+ 1 (minibuffer-prompt-width)))
       (point))
     (progn
       (end-of-line)
       (point)))))


(defun evernote-set-minibuffer-string (str)
  (save-excursion
    (evernote-set-buffer-to-minibuffer)
    (delete-region
     (progn
       (goto-char (+ 1 (minibuffer-prompt-width)))
       (point))
     (progn
       (end-of-line)
       (point)))
    (insert str)))


(defun evernote-set-buffer-to-minibuffer ()
  (set-buffer (window-buffer (active-minibuffer-window))))


(defun evernote-minibuffer-tmp-message (msg)
  (save-excursion
    (goto-char (point-max))
    (save-excursion (insert " " msg))
    (sit-for 1)
    (delete-region (point) (point-max))))


(defun evernote-update-mode-line ()
  "Update mode line"
  (setq vc-mode
        (concat "[Tag:" (mapconcat #'identity evernote-note-tags ",") "] "
                "[Edit mode:" evernote-note-edit-mode "]"))
  (force-mode-line-update))


(defun evernote-completing-read-multiple
  (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Read multiple strings with completion. and return nil if no input is given"
  (let (results)
    (setq results
          (completing-read-multiple prompt
                                    table
                                    predicate
                                    require-match
                                    initial-input
                                    hist
                                    def
                                    inherit-input-method))
    (delete "" results)))


(defun evernote-change-major-mode-from-note-name (note-name)
  (catch 'mode
    (mapc
     (lambda (pattern-mode-pair)
       (if (consp pattern-mode-pair)
           (let ((pattern (car pattern-mode-pair))
                 (mode (cdr pattern-mode-pair)))
             (if (consp mode)
                 (setq mode (car mode)))
             (if (and (stringp pattern)
                      (fboundp mode)
                      (string-match pattern note-name))
                 (progn
                   (funcall mode)
                   (throw 'mode mode))))))
     auto-mode-alist)
    (if (fboundp default-major-mode)
        (progn
          (funcall default-major-mode)
          (throw 'mode default-major-mode)))))


(defun evernote-get-first-line (str)
  "Get first line of the string"
  (let ((begin (string-match "^.*$" str)))
    (substring str begin (match-end 0))))


;;
;; Command interface.
;;

(defun evernote-command-login ()
  "Issue login command"
  (let* ((user (read-string "Evernote user name:"))
         (passwd (read-passwd "Passwd:")))
    (evernote-issue-command nil "login" user passwd)))


(defun evernote-command-get-tag-list ()
  "Issue listtags command"
  (evernote-issue-command nil "listtags")
  (evernote-eval-command-result))


(defun evernote-command-get-note-list-from-tags (tag-names)
  "Issue listnotes command from the tag name list."
  (if tag-names
      (let ((oct-tag-names
             (mapconcat #'identity (mapcar 'evernote-string-to-oct tag-names) ",")))
        (evernote-issue-command nil "listnotes" "-t" oct-tag-names))
    (evernote-issue-command nil "listnotes"))
  (evernote-eval-command-result))


(defun evernote-command-get-note-list-from-query (query)
  "Issue listnotes command from the query."
  (if query
      (let ((oct-query (evernote-string-to-oct query)))
        (evernote-issue-command nil "listnotes" "-q" oct-query))
    (evernote-issue-command nil "listnotes"))
  (evernote-eval-command-result))


(defun evernote-command-get-note-content (guid note-edit-mode)
  "Issue getnotecontent command specified by the guid and the edit mode."
  (let ((option (cond
                 ((string= note-edit-mode "XHTML") "-x")
                 ((string= note-edit-mode "TEXT") "--text"))))
    (evernote-issue-command nil "getnotecontent" guid option)
    (evernote-get-command-result)))


(defun evernote-command-create-note (inbuf name tags edit-mode)
  "Issue createnote command specified by the guid, tags and the edit-mode."
  (let (edit-mode-option)
    (cond
     ((string= edit-mode "XHTML")
      (setq edit-mode-option "-x"))
     ((string= edit-mode "TEXT")
      (setq edit-mode-option "--text")))
    (if tags
        (evernote-issue-command inbuf
                                "createnote" "-c"
                                "-t" (mapconcat #'identity (mapcar 'evernote-string-to-oct tags) ",")
                                (evernote-string-to-oct name)
                                edit-mode-option)
      (evernote-issue-command inbuf
                              "createnote" "-c"
                              (evernote-string-to-oct name)
                              edit-mode-option))))


(defun evernote-command-update-note (inbuf guid name tags edit-mode)
  "Issue updatenote command specified by the guid and the parameters for updating."
  (let (edit-mode-option)
    (cond
     ((string= edit-mode "XHTML")
      (setq edit-mode-option "-x"))
     ((string= edit-mode "TEXT")
      (setq edit-mode-option "--text")))
    (if tags
        (evernote-issue-command inbuf
                                "updatenote" "-c"
                                "-t" (mapconcat #'identity (mapcar 'evernote-string-to-oct tags) ",")
                                guid (evernote-string-to-oct name) edit-mode-option)
      (evernote-issue-command inbuf
                              "updatenote" "-c" "--delete-all-tags"
                              guid (evernote-string-to-oct name) edit-mode-option))))


(defun evernote-command-delete-note (guid)
  "Issue deletenote command specified by the guid, tags and the edit mode."
  (evernote-issue-command nil "deletenote" guid))


(defun evernote-command-get-search-list ()
  "Issue listsearch command"
  (evernote-issue-command nil "listsearch")
  (evernote-eval-command-result))


(defun evernote-command-create-search (name query)
  "Issue createsearch command"
  (evernote-issue-command nil
                          "createsearch"
                          (evernote-string-to-oct name)
                          (evernote-string-to-oct query)))


(defun evernote-command-update-search (guid name query)
  "Issue updatesearch command"
  (evernote-issue-command nil
                          "updatesearch"
                          guid
                          (evernote-string-to-oct name)
                          (evernote-string-to-oct query)))


(defun evernote-issue-command (inbuf &rest args)
  "Invoke external process to issue an evernote command."
  (let ((outbuf (get-buffer-create evernote-command-output-buffer-name))
        (infile nil)
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (if inbuf
        (save-excursion
          (set-buffer inbuf)
          (setq infile (make-temp-file "evernote"))
          (write-region (point-min) (point-max) infile)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (erase-buffer))
    (message "Waiting for the result...")
    (let ((result (apply 'call-process "ruby" infile outbuf nil "-S" "enclient.rb" args)))
      (message "")
      (cond
       ((eq result evernote-error-ok) t)
       (t (throw 'error result))))))


(defun evernote-get-command-result ()
  "Get the result of the result of the lately issued command as a string."
  (let ((outbuf (get-buffer-create evernote-command-output-buffer-name)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (buffer-substring (point-min) (point-max)))))


(defun evernote-eval-command-result ()
  "Get the result of the result of the lately issued command as a string and eval the string."
  (let ((outbuf (get-buffer-create evernote-command-output-buffer-name)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (car (read-from-string
            (buffer-substring (point-min) (point-max)))))))


(defun evernote-error-message (error-code)
  "Get the error message corresponding to  the integer command result."
  (cond
   ((eq error-code evernote-error-ok)                 "OK")
   ((eq error-code evernote-error-fail)               "System error")
   ((eq error-code evernote-error-parse)              "Parse error")
   ((eq error-code evernote-error-unknown)            "Unknown error")
   ((eq error-code evernote-error-bad-data-format)    "Bad data format")
   ((eq error-code evernote-error-permission-denied)  "Permission denied")
   ((eq error-code evernote-error-internal-error)     "Internal error")
   ((eq error-code evernote-error-data-required)      "Data required")
   ((eq error-code evernote-error-limit-reached)      "Limit reached")
   ((eq error-code evernote-error-quota-reached)      "Quota reached")
   ((eq error-code evernote-error-invalid-auth)       "Invalid auth")
   ((eq error-code evernote-error-auth-expired)       "Auth expired")
   ((eq error-code evernote-error-data-conflict)      "Data conflict. The data already exists.")
   ((eq error-code evernote-error-enml-validation)    "Enml validation. Tried to save a note of invalid format.")
   ((eq error-code evernote-error-shared-unavailable) "Shared unavailable")))


;;
;; Evernote mode
;;

(defun evernote-search-mode ()
  "Major mode for selecting a note."
  (interactive)
  (use-local-map evernote-search-mode-map)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'evernote-search-mode
        mode-name "Evernote-Search")
  (goto-char (point-min)))

;;
;; Evernote mode
;;

(defun evernote-mode-after-save-hook ()
  "After save hook for evernote mode. This invalid evernote-mode"
  (if evernote-mode
      (evernote-mode)))


(defun evernote-mode-change-major-mode-hook ()
  "Change major mode hook for evernote mode. This records the note info to the global variable to restore them after changing the major mode"
  (if evernote-mode
      (setq evernote-mode-info-for-changing-major-mode
            (list
             (cons 'guid  evernote-note-guid)
             (cons 'name  evernote-note-name)
             (cons 'tags  evernote-note-tags)
             (cons 'edit-mode  evernote-note-edit-mode)))))


(defun evernote-mode-after-change-major-mode-hook ()
  "After change major mode hook for evernote mode. This restore the note info after changing the major mode"
  (if evernote-mode-info-for-changing-major-mode
      (progn
        (evernote-mode
         (evernote-assoc-cdr 'guid evernote-mode-info-for-changing-major-mode)
         (evernote-assoc-cdr 'name evernote-mode-info-for-changing-major-mode)
         (evernote-assoc-cdr 'tags evernote-mode-info-for-changing-major-mode)
         (evernote-assoc-cdr 'edit-mode evernote-mode-info-for-changing-major-mode))
        (setq evernote-mode-info-for-changing-major-mode nil))))


(add-hook 'after-change-major-mode-hook
          'evernote-mode-after-change-major-mode-hook)


(defun evernote-mode (&optional guid name tags edit-mode)
  "Toggle Evernote mode, a minor mode for using evernote functions."
  (interactive)
  (or (assq 'evernote-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(evernote-mode " Evernote") minor-mode-alist)))
  (or (assq 'evernote-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'evernote-mode evernote-mode-map) minor-mode-map-alist)))
  (let ((modified (buffer-modified-p)))
    (set-buffer-file-coding-system 'utf-8)
    (set-buffer-modified-p modified))
  (setq evernote-mode (not evernote-mode))
  (if evernote-mode
      (progn
        (if guid (setq evernote-note-guid guid))
        (if name (setq evernote-note-name name))
        (if tags (setq evernote-note-tags tags))
        (if edit-mode (setq evernote-note-edit-mode edit-mode))
        (setq vc-mode
              (concat "[Tag:" (mapconcat #'identity evernote-note-tags ",") "] "
                      "[Edit mode:" evernote-note-edit-mode "]"))
        (make-local-hook 'after-save-hook)
        (make-local-hook 'change-major-mode-hook)
        (add-hook 'after-save-hook
                  'evernote-mode-after-save-hook
                  nil t)
        (add-hook 'change-major-mode-hook
                  'evernote-mode-change-major-mode-hook
                  nil t)
        (run-hooks 'evernote-mode-hook))
    (progn
      (setq evernote-note-guid nil
            evernote-note-name nil
            evernote-note-tags nil
            evernote-note-edit-mode nil)
      (setq vc-mode nil)
      (remove-hook 'after-save-hook
                   'evernote-mode-after-save-hook)
      (remove-hook 'change-major-mode-hook
                   'evernote-mode-change-major-mode-hook))))


(provide 'evernote-mode)

;;(setq debug-on-error t)

