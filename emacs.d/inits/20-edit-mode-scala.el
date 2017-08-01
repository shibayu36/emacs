;;; scala mode
(require 'scala-mode)
(require 'sbt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ensime settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(setq ensime-startup-notification nil)
(setq ensime-eldoc-hints 'all)

;;; Use auto-complete for ensime
(setq ensime-completion-style 'auto-complete)
(defvar shibayu36/ensime-completion-style 'auto-complete)

(defun scala/configure-ensime ()
  "Ensure the file exists before starting `ensime-mode'."
  (eval-and-compile (require 'ensime))
  (if (file-exists-p (buffer-file-name))
      (scala/start-ensime-or-enable-flycheck)
    (add-hook 'after-save-hook
              #'(lambda ()
                  (scala/start-ensime-or-enable-flycheck)) nil t)))

(defun scala/maybe-start-ensime ()
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 'noflet))
  (and (buffer-file-name)
       (let* ((ensime-buffer (ignore-errors (scala/ensime-buffer)))
              (ensime-buffer (and (buffer-live-p ensime-buffer)
                                  ensime-buffer))
              (config-file (ensime-config-find-file (buffer-file-name))))
         (and config-file ; ignore if there is no .ensime for the project
              (prog1 t
                (unless ensime-buffer
                  (noflet ((ensime-config-find (&rest _) config-file))
                          (save-window-excursion
                            (ensime)))))))))

(defun scala/ensime-buffer ()
  "Find the Ensime server buffer corresponding to FILE."
  (eval-and-compile (require 'ensime))
  (let* ((config (ensime-config-for-buffer))
         (server-process (and config (ensime-process-for-config config))))
    (when server-process
      (process-buffer server-process))))

(defun scala/start-ensime-or-enable-flycheck ()
  (unless (scala/maybe-start-ensime)
    (flycheck-mode +1)))

(defun scala/enable-eldoc ()
  (eldoc-mode +1))

(defun scala/completing-dot-company ()
  (eval-and-compile (require 'company))
  (cond (company-backend
         (company-abort)
         (scala/completing-dot))
        (t
         (insert ".")
         (company-complete))))

;; (defun scala/ac-trigger-key-command (orig-fun &rest args)
;;   (if ensime-mode
;;       (let ((ac-sources '(ac-source-ensime-completions))
;;             (ac-use-comphist nil)
;;             (ac-auto-show-menu 0.5)
;;             (ac-candidates-cache nil)
;;             (ac-auto-start nil)
;;             (ac-expand-on-auto-complete t)
;;             (ac-use-fuzzy nil)
;;             (ac-dwim nil)
;;             (ac-use-quick-help t)
;;             (ac-delete-dups nil)
;;             (ac-ignore-case t))
;;         (apply orig-fun args))
;;     (apply orig-fun args)))
;; (advice-add 'ac-trigger-key-command :around #'scala/ac-trigger-key-command)

(defun scala/completing-dot-ac ()
  (eval-and-compile (require 'auto-complete))
  (insert ".")
  (ac-trigger-key-command t))

(defmacro scala/with-project-sbt (&rest form)
  `(progn
     (eval-and-compile (require 'projectile))
     (eval-and-compile (require 'sbt-mode))
     (let* ((file (or (buffer-file-name) (error "Visiting no file")))
            (dir (file-name-directory file))
            (dir (let ((default-directory dir)) (projectile-project-p))))
       (when dir
         (setq sbt:buffer-project-root dir)
         (condition-case err
             (progn ,@form)
           (error (error err)))))))

(defun scala/call-sbt-command (command &rest args)
  (scala/with-project-sbt
   (let (buf (get-buffer-create (sbt:buffer-name)))
     (apply 'call-process sbt:program-name nil buf t args))))


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

(defun sbt:send-buffer ()
  "Send buffer content to shell."
  (interactive)
  (eval-and-compile (require 'sbt-mode))
  (sbt:send-region (point-min) (point-max)))

(defun ensime-restart ()
  "Restart the ensime server."
  (interactive)
  (ensime-shutdown)
  (sit-for 2)
  (scala/maybe-start-ensime))

(defun ensime-gen-and-restart ()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (message "Regenerating .ensime ...")
    (when (= 0 (scala/call-sbt-command "ensimeCofig"))
      (ensime-restart))))

;; Initialization
(defun shibayu36/enable-eldoc ()
  (set (make-local-variable 'eldoc-idle-delay) 0.5)
  (scala/enable-eldoc))

(defun shibayu36/configure-scala ()
  (when (eq ensime-completion-style 'auto-complete)
    (eval-and-compile (require 'auto-complete))
    (make-local-variable 'ac-trigger-key)
    (ac-set-trigger-key "TAB"))
  (unless (string-match "\\.sbt$" (or (buffer-file-name) ""))
    (scala/configure-ensime)))

(defun shibayu36/ensime-disable-flycheck (&rest args)
  (flycheck-mode -1))
(advice-add 'ensime :after #'shibayu36/ensime-disable-flycheck)

(add-hook 'ensime-mode-hook #'shibayu36/enable-eldoc)
(add-hook 'scala-mode-hook #'shibayu36/configure-scala)
(add-hook 'scala-mode-hook #'shibayu36/enable-eldoc)
(add-hook 'java-mode-hook #'ensime-mode)

(with-eval-after-load-feature 'ensime-mode
                              ;; Prevent the default behavior; `ensime-mode' is invoked via
                              ;; `shibayu36/configure-scala'.
                              (remove-hook 'scala-mode-hook 'ensime-mode))

(unless (eq shibayu36/ensime-completion-style 'auto-complete)
  (with-eval-after-load-feature 'auto-complete
                                (setq ac-modes (remove 'scala-mode ac-modes))))

;; Configuration

(defun shibayu36/ensime-search-mode (&rest args)
  (with-current-buffer ensime-search-target-buffer-name
    (setq show-trailing-whitespace nil)))
(advice-add 'ensime-search-mode :after #'shibayu36/ensime-search-mode)

(with-eval-after-load-feature 'ensime
                              (set-face-attribute 'ensime-implicit-highlight nil
                                                  :underline '(:style wave :color "#7F9F7F")))

(add-hook 'scala-mode-hook
          '(lambda ()
             (progn
               (local-set-key (kbd "C-x C-j") 'open-by-intellij)
               (local-set-key (kbd "C-@") 'ensime-edit-definition)
               (local-set-key (kbd "M-@") 'ensime-pop-find-definition-stack))))
