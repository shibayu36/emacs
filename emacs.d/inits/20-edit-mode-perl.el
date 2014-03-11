;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;perlモードの設定;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;perlモードではなくcperl-modeを使う
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("cpanfile$" . cperl-mode) auto-mode-alist))
(require 'set-perl5lib)

(setq cperl-hairy nil)

(require 'plenv)
(plenv-global "5.14.2")

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
            (set-face-background 'cperl-hash-face "black")))

;;flymake, perl-completionは重いので、やめた
(defvar ac-source-my-perl-completion
  '((candidates . plcmp-ac-make-cands)))
(add-hook 'cperl-mode-hook
          (lambda()
            (setq plcmp-use-keymap nil)
            (require 'perl-completion)
            (perl-completion-mode t)
            (setq plcmp-default-lighter nil)
            (add-to-list 'ac-sources 'ac-source-my-perl-completion)))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil)
               (set-perl5lib))))

;; テスト実行用
(defun run-perl-method-test ()
  (interactive)
  (let (
        (command compile-command)
        (test-method nil))
    (save-excursion
      (when (or
             (re-search-backward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t)
             (re-search-forward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t))
        (setq test-method (match-string 1))))
    (if test-method
        (compile
         (format
          "cd %s; TEST_METHOD=%s perl -M'Project::Libs lib_dirs => [qw(modules/*/lib local/lib/perl5)]' %s"
          (replace-regexp-in-string
           "\n+$" ""
           (shell-command-to-string "git rev-parse --show-cdup"))
          test-method
          (buffer-file-name (current-buffer))))

      (compile
       (format
        "cd %s; perl -M'Project::Libs lib_dirs => [qw(modules/*/lib local/lib/perl5)]' %s"
        (replace-regexp-in-string
         "\n+$" "" (shell-command-to-string "git rev-parse --show-cdup"))
        (buffer-file-name (current-buffer)))))))

(defun run-perl-test ()
  (interactive)
  (let* ((cmd "git rev-parse --show-toplevel")
         (topdir (with-temp-buffer
                   (call-process-shell-command cmd nil t nil)
                   (goto-char (point-min))
                   (if (re-search-forward "^\\(.+\\)$" nil t)
                       (match-string 1)))))
    (quickrun :source `((:command . "prove")
                        (:default-directory . ,topdir)
                        (:exec . ("%c -l -Ilocal/lib/perl5 -It/lib -bv --color %s"))))))

;; gitルートからPERL5LIBにPATH通す
(defun setup-perl5lib ()
  (interactive)
  (set-perl5lib-glob-from-git-root "lib")
  (set-perl5lib-glob-from-git-root "t/lib")
  (set-perl5lib-glob-from-git-root "modules/*/lib"))

;; 現在の位置のmodule名のuseを書くためにpopupする
(defun popup-editor-perl-use ()
  (interactive)
  (let* ((module-name nil))
    (cond ((use-region-p)
           (setq module-name (buffer-substring (region-beginning) (region-end)))
           (keyboard-escape-quit))
          (t
           (setq module-name (thing-at-point 'symbol))))
    (kill-new (concat "use " module-name ";"))
    (popwin:popup-buffer (current-buffer) :height 0.4)
    (re-search-backward "^use " nil t)
    (next-line)))

;;; helm-perldocの設定
(require 'helm-perldoc)

(defun my-helm-perldoc-setup ()
  ;; add helm-perldoc:perl5lib automatically
  (let ((perl5libs (split-string (or helm-perldoc:perl5lib "") path-separator t))
        (local-lib (projectile-expand-root "local/lib/perl5")))
    (when (and (projectile-verify-file "cpanfile")
               (not (member local-lib perl5libs)))
      (setq helm-perldoc:perl5lib
            (if perl5libs
                (mapconcat 'identity (cons local-lib perl5libs) path-separator)
              local-lib))
      (message "helm-perldoc:perl5lib is updated. (%s)" helm-perldoc:perl5lib)
      (setq helm-perldoc:modules nil)))
  (message "hogehoge")
  (helm-perldoc:setup))

(add-hook 'cperl-mode-hook 'my-helm-perldoc-setup)

(require 'hatena-translator)

;;; perl用keybind
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (local-set-key (kbd "M-RET") 'plcmp-cmd-smart-complete)
               (local-set-key (kbd "C-c d") 'plcmp-cmd-show-doc-at-point)
               (local-set-key "\C-c\C-hm" 'perldoc-m)
               (local-set-key (kbd "C-c C-s") 'perl-syntax-check)
               (local-set-key (kbd "C-c C-t") 'run-perl-test)
               (local-set-key (kbd "C-c C-c C-u") 'popup-editor-perl-use)
               (local-set-key "\C-ct" 'run-perl-method-test)
               (local-set-key (kbd "C-c a") 'align)
               (local-set-key (kbd "F") (smartchr '("F" "$")))
               (local-set-key (kbd ",") (smartchr '("," " => ")))
               (local-set-key (kbd "J") (smartchr '("J" "->")))
               (local-set-key [(meta t)] 'hatena-translator:popup-msgid-at-point)
               (local-set-key [(meta T)] 'hatena-translator:open-msgid-at-point)
               (local-set-key (kbd "C-c d") 'helm-perldoc))))
