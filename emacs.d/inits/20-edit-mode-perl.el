;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;perlモードの設定;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;perlモードではなくcperl-modeを使う
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi$" . cperl-mode) auto-mode-alist))
(require 'set-perl5lib)

(setq cperl-hairy nil)

(require 'perlbrew)
(perlbrew-switch "perl-5.14.2")

(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-label-offset -4
      cperl-tab-always-indent nil
      cperl-font-lock t)
(add-hook 'cperl-mode-hook 'flymake-perl-load)
(add-hook 'cperl-mode-hook
          (lambda ()
            (set-face-bold-p 'cperl-array-face nil)
            (set-face-background 'cperl-array-face "black")
            (set-face-bold-p 'cperl-hash-face nil)
            (set-face-italic-p 'cperl-hash-face nil)
            (set-face-background 'cperl-hash-face "black")
            ))
(add-hook 'cperl-mode-hook
          (lambda ()
            (yas/reload-all)))

;;flymake, perl-completionは重いので、やめた
(defvar ac-source-my-perl-completion
  '((candidates . plcmp-ac-make-cands)))
(add-hook 'cperl-mode-hook
          (lambda()
            (setq plcmp-use-keymap nil)
            (require 'perl-completion)
            (perl-completion-mode t)
            (add-to-list 'ac-sources 'ac-source-my-perl-completion)
            (local-set-key (kbd "M-RET") 'plcmp-cmd-smart-complete)
            (local-set-key (kbd "C-c d") 'plcmp-cmd-show-doc-at-point)
            ))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil)
               (local-set-key "\C-c\C-hm" 'perldoc-m)
               (local-set-key (kbd "C-c C-s") 'perl-syntax-check)
               (local-set-key (kbd "C-c C-d") 'perl5-data-dump)
               (local-set-key (kbd "C-c C-t") 'run-perl-test)
               (local-set-key "\C-ct" 'run-perl-method-test)
               (local-set-key (kbd "C-c a") 'align)
               (local-set-key (kbd "F") (smartchr '("F" "$")))
               (local-set-key (kbd "H") (smartchr '("H" " => ")))
               (local-set-key (kbd "J") (smartchr '("J" "->")))
               (local-set-key (kbd "M") (smartchr '("M" "my ")))
               (require 'hatena-translator)
               (local-set-key [(meta t)] 'hatena-translator:popup-msgid-at-point)
               (local-set-key [(meta T)] 'hatena-translator:open-msgid-at-point)
               (set-perl5lib)
               )))



;; モジュールソースバッファの場合はその場で、
;; その他のバッファの場合は別ウィンドウに開く。
(put 'perl-module-thing 'end-op
     (lambda ()
       (re-search-forward "\\=[a-zA-Z][a-zA-Z0-9_:]*" nil t)))
(put 'perl-module-thing 'beginning-op
     (lambda ()
       (if (re-search-backward "[^a-zA-Z0-9_:]" nil t)
           (forward-char)
         (goto-char (point-min)))))

(defun perldoc-m ()
  (interactive)
  (let ((module (thing-at-point 'perl-module-thing))
        (pop-up-windows t)
        (cperl-mode-hook nil))
    (when (string= module "")
      (setq module (read-string "Module Name: ")))
    (let ((result (substring (shell-command-to-string (concat "perldoc -m " module)) 0 -1))
          (buffer (get-buffer-create (concat "*Perl " module "*")))
          (pop-or-set-flag (string-match "*Perl " (buffer-name))))
      (if (string-match "No module found for" result)
          (message "%s" result)
        (progn
          (with-current-buffer buffer
            (toggle-read-only -1)
            (erase-buffer)
            (insert result)
            (goto-char (point-min))
            (cperl-mode)
            (toggle-read-only 1)
            )
          (if pop-or-set-flag
              (switch-to-buffer buffer)
            (display-buffer buffer)))))))

;; テスト実行用
(defun run-perl-method-test ()
  (interactive)
  (let (
        (command compile-command)
        (test-method nil))
    (save-excursion
      (when (or
             (re-search-backward "\\bsub\s+\\([_[:alpha:]]+\\)\s*:\s*Test" nil t)
             (re-search-forward "\\bsub\s+\\([_[:alpha:]]+\\)\s*:\s*Test" nil t))
        (setq test-method (match-string 1))))
    (if test-method
        (compile
         (format "cd %s; TEST_METHOD=%s %s -MProject::Libs %s" (replace-regexp-in-string "\n+$" "" (shell-command-to-string "git rev-parse --show-cdup")) test-method (perlbrew-get-current-perl-path) (buffer-file-name (current-buffer))))
        (compile
         (format "cd %s; %s -MProject::Libs %s" (replace-regexp-in-string "\n+$" "" (shell-command-to-string "git rev-parse --show-cdup")) (perlbrew-get-current-perl-path) (buffer-file-name (current-buffer)))))))

(defun run-perl-test ()
  "test実行します"
  (interactive)
  (compile
   (format "cd %s; %s -MProject::Libs %s"
           (replace-regexp-in-string "\n+$" "" (shell-command-to-string "git rev-parse --show-cdup"))
           (perlbrew-get-current-perl-path)
           (buffer-file-name))))

;;; perlスクリプト実行用

;;; perlのuse sort
(defun sort-perl-use (beg end)
  (interactive (list (region-beginning) (region-end)))
  (sort-regexp-fields nil "^.+$" "[^;]+" beg end))

;;perlインデント整形ツール
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

;;perlのsyntaxチェック
(defun perl-syntax-check()
  (interactive)
  (shell-command
   (concat "perl -wc " (file-name-nondirectory (buffer-file-name)))))


;; gitルートからPERL5LIBにPATH通す
(defun setup-perl5lib ()
  (interactive)
  (set-perl5lib-glob-from-git-root "lib")
  (set-perl5lib-glob-from-git-root "t/lib")
  (set-perl5lib-glob-from-git-root "modules/*/lib"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;podモード;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pod-mode)