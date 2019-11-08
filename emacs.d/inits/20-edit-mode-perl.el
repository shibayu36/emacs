;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;perlモードの設定;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;perlモードではなくcperl-modeを使う
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("cpanfile$" . cperl-mode) auto-mode-alist))

(custom-set-faces
 '(cperl-array-face ((t (:foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:foreground "Red" :weight bold)))))

(require 'plenv)
;; (plenv-global "5.14.2")

(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-label-offset -4
      cperl-tab-always-indent nil
      cperl-font-lock t
      cperl-hairy nil
      cperl-electric-parens nil)

(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil))))

;;; flycheckによるsyntaxチェック
(flycheck-define-checker perl-project-libs
  "A perl syntax checker."
  :command (
            "perl"
            (option-list "-I" flycheck-perl-include-path)
            "-MProject::Libs lib_dirs => [qw(t/lib modules/*/lib local/lib/perl5)]"
            "-wc"
            source-inplace)
  :error-patterns ((error line-start
                          (minimal-match (message))
                          " at " (file-name) " line " line
                          (or "." (and ", " (zero-or-more not-newline)))
                          line-end))
  :modes (cperl-mode))

(defun shibayu36/cperl-mode-flycheck-hook ()
  (flycheck-mode t)
  (setq flycheck-checker 'perl-project-libs)
  (setq flycheck-perl-include-path `(,(magit-toplevel))))
(add-hook 'cperl-mode-hook 'shibayu36/cperl-mode-flycheck-hook)

;; テスト実行用
(defun run-perl-method-test ()
  (interactive)
  (let ((topdir (magit-toplevel))
        (test-method nil))
    (save-excursion
      (when (or
             (re-search-backward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t)
             (re-search-forward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t))
        (setq test-method (match-string 1))))
    (if test-method
        (quickrun
         :source
         `((:command . "prove")
           (:default-directory . ,topdir)
           (:exec . (,(concat "PERL5LIB=lib:local/lib/perl5:t/lib:$PERL5LIB TEST_METHOD=" test-method " %c -v %s")))))

      (quickrun
       :source
       `((:command . "prove")
         (:default-directory . ,topdir)
         (:exec . (("PERL5LIB=lib:local/lib/perl5:t/lib:$PERL5LIB %c -v %s"))))))))

(defun run-perl-test ()
  (interactive)
  (let* ((topdir (magit-toplevel)))
    (quickrun :source `((:command . "prove")
                        (:default-directory . ,topdir)
                        (:exec . ("PERL5LIB=lib:local/lib/perl5:t/lib:$PERL5LIB %c -bv --color %s"))))))

(defun check-perl-used-modules ()
  (interactive)
  (let* ((topdir (magit-toplevel)))
    (quickrun :source `((:command . "perl")
                        (:default-directory . ,topdir)
                        (:exec . ("PERL5LIB=lib:local/lib/perl5:$PERL5LIB %c -MTest::UsedModules -MTest::More -e 'used_modules_ok(\"%s\");done_testing()'"))))))

(defun run-perltidy ()
  (interactive)
  (let* ((topdir (magit-toplevel))
         (file (buffer-file-name (current-buffer))))
    (quickrun :source `((:command . "carton exec -- perltidy")
                        (:default-directory . ,topdir)
                        (:exec . (,(concat "%c -b -bext=/ " file)))))))

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
(add-hook 'cperl-mode-hook 'helm-perldoc:carton-setup)

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
               (local-set-key (kbd "C-c d") 'helm-perldoc)
               (local-set-key (kbd "C-c C-p") 'run-perltidy))))
