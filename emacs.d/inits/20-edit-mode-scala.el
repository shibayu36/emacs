;;; scala mode
(require 'scala-mode2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ensime settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(require 'noflet)

;;; Use auto-complete for ensime
(setq ensime-completion-style 'auto-complete)

(defun scala/configure-ensime ()
  "Ensure the file exists before starting `ensime-mode'."
  (if (file-exists-p (buffer-file-name))
      (ensime-mode +1)
    (add-hook 'after-save-hook #'(lambda () (ensime-mode +1)) nil t)))

(defun scala/maybe-start-ensime ()
  (when (buffer-file-name)
    (let ((ensime-buffer (scala/ensime-buffer-for-file (buffer-file-name)))
          (file (ensime-config-find-file (buffer-file-name))))
      ;; ignore if there is no .ensime for the project
      (when (null ensime-buffer)
        (noflet ((ensime-config-find (&rest _) file))
                (save-window-excursion
                  (ensime)))))))

(defun scala/ensime-project-name-from-config (file)
  (let ((config (ensime-config-load file)))
    (plist-get config :name)))

(defun scala/ensime-buffer-for-file (file)
  "Find the Ensime server buffer corresponding to FILE."
  (let* ((config-file (ensime-config-find-file file))
         (name (and config-file
                    (scala/ensime-project-name-from-config config-file)))
         (default-directory (file-name-directory file)))
    (when name
      (--first (-when-let (bufname (buffer-name it))
                 (and (s-contains? "inferior-ensime-server" bufname)
                      (s-contains? name bufname)))
               (buffer-list)))))

(defun scala/enable-eldoc ()
  "Show error message or type name at point by Eldoc."
  (setq-local eldoc-documentation-function
              #'(lambda ()
                  (when (ensime-connected-p)
                    (let ((err (ensime-print-errors-at-point)))
                      (or (and err (not (string= err "")) err)
                          (ensime-print-type-at-point))))))
  (eldoc-mode +1))

(defun scala/completing-dot-company ()
  (cond (company-backend
         (company-complete-selection)
         (scala/completing-dot))
        (t
         (insert ".")
         (company-complete))))

(defun scala/completing-dot-ac ()
  (insert ".")
  (ac-trigger-key-command t))

;; Interactive commands

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 's))
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (cond ((not (and (ensime-connected-p) ensime-completion-style))
         (insert "."))
        ((eq ensime-completion-style 'company)
         (scala/completing-dot-company))
        ((eq ensime-completion-style 'auto-complete)
         (scala/completing-dot-ac))))

(defun ensime-cleanup ()
  "Shutdown and destroy connection buffer."
  (interactive)
  (ensime-shutdown)
  (let* ((buf (buffer-file-name))
         (ensime-buffer (scala/ensime-buffer-for-file buf)))
    (when ensime-buffer (kill-buffer ensime-buffer))))

(defun ensime-restart ()
  "Restart the ensime server."
  (interactive)
  (ensime-cleanup)
  (scala/maybe-start-ensime))

(defun ensime-gen-and-restart ()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (message "Regenerating .ensime ...")
    (when (= 0 (scala/call-sbt-command "gen-ensime"))
      (ensime-restart))))

;; Initialization
(defun shibayu36/configure-scala ()
  (scala/configure-ensime)
  ;; (scala/maybe-start-ensime)
  (unless (ensime-config-find-file (buffer-file-name))
    (flycheck-mode +1)))

(defadvice ensime (after ensime-disable-flycheck activate)
  (flycheck-mode -1))

(add-hook 'ensime-mode-hook #'scala/enable-eldoc)
(add-hook 'scala-mode-hook #'shibayu36/configure-scala)
