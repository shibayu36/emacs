(require 'magit)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(set-face-foreground 'magit-diff-add "green")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup for mo-git-blame ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------------------------------------------------
;; git blame
;; ---------------------------------------------------
;; (require 'mo-git-blame)
;; (setq mo-git-blame-blame-window-width 80)
;; ;; format of blame
;; (defun mo-git-blame-process-filter-process-entry (entry)
;;   (with-current-buffer (plist-get mo-git-blame-vars :blame-buffer)
;;     (save-excursion
;;       (let ((inhibit-read-only t)
;;             (info (format "%s (%-8s %s %s) %s"
;;                           (substring (symbol-name (plist-get entry :hash)) 0 8)
;;                           (let ((author (plist-get entry :author))) (substring author 0 (min 8 (length author))))
;;                           (format-time-string "%y-%m-%d %T" (mo-git-blame-commit-info-to-time entry) t)
;;                           (plist-get entry :author-tz)
;;                           (plist-get entry :summary)))
;;             i)
;;         (mo-git-blame-goto-line-markless (plist-get entry :result-line))
;;         (dotimes (i (plist-get entry :num-lines))
;;           (insert info)
;;           (goto-char (line-beginning-position 2)))))))

;; ;; override
;; (defun mo-git-blame-process-filter (process string)
;;   (with-current-buffer (process-buffer process)
;;     (let ((inhibit-read-only t)
;;           done matched)
;;       (save-excursion
;;         (goto-char (process-mark process))
;;         (insert string)
;;         (set-marker (process-mark process) (point)))
;;       (while (not done)
;;         (goto-char (line-end-position))
;;         (setq done (= (point) (point-max)))
;;         (goto-char (line-beginning-position))
;;         (unless done
;;           (setq matched t)
;;           (cond ((and (not mo-git-blame-curr-entry)
;;                       (looking-at "^\\([a-fA-F0-9]\\{40\\}\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\)$"))
;;                  ;; SHA line, beginning of entry
;;                  (setq mo-git-blame-curr-entry (intern (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
;;                  (mo-git-blame-set-entry :source-line (string-to-number (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
;;                  (mo-git-blame-set-entry :result-line (string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3))))
;;                  (mo-git-blame-set-entry :num-lines (string-to-number (buffer-substring-no-properties (match-beginning 4) (match-end 4))))
;;                  )

;;                 ((and mo-git-blame-curr-entry
;;                       (looking-at "^filename +\\(.+\\)$"))
;;                  ;; filename line, end of entry
;;                  (mo-git-blame-set-entry :filename (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
;;                  (mo-git-blame-process-filter-process-entry (plist-get mo-git-blame-data mo-git-blame-curr-entry))
;;                  (setq mo-git-blame-curr-entry nil)
;;                  )
;;                 ((and mo-git-blame-curr-entry
;;                       (looking-at "^\\([a-zA-Z0-9-]+\\) +\\(.+\\)$"))
;;                  ;; property line
;;                  (mo-git-blame-set-entry (intern (concat ":" (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
;;                                          (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
;;                  )

;;                 (t (setq matched nil)))
;;           (forward-line) ;; bugfix (next-line)
;;           )))))


;; (defalias 'g 'mo-git-blame-current)


(defun git-blame-current-buffer () ;; shell-command を使うより、vc-annotete を利用した方がさらに過去にさかのぼれるので良いのでは？
  (interactive)
  (let ((result-buf "*git blame*")
        (popup-context (git-blame-current-line))
        (hoge-line (line-number-at-pos)))
    (shell-command (concat "git blame " (buffer-file-name (current-buffer))) result-buf)
    (view-buffer-other-window result-buf t
                              (lambda (buf)
                                (kill-buffer-and-window)))
  (switch-to-buffer result-buf)
  (goto-line hoge-line)
  (recenter)
  (popup-tip popup-context)))

(defalias 'g 'git-blame-current-buffer)

(defun git-blame-current-line ()
  (interactive)
  (let ((old-buf (current-buffer))
    (blame-buf (get-buffer-create "*blame*"))
    (line-num (number-to-string (line-number-at-pos))))
    (set-buffer blame-buf)
    (erase-buffer)
    (call-process "git-blame-oneline" nil "*blame*" t (buffer-file-name old-buf) line-num)
    (setq content (buffer-string))
    (set-buffer old-buf)
    (when (not (eq (length content) 0)) ;;この when に入らない場合は想定していないです
      content)))