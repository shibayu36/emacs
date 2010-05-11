;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;html-helper-mode設定;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'html-helper-mode "html-helper-mode" "Ya HTML" t)
(setq auto-mode-alist 
      (append '(
                ("\\.\\(html\\|htm\\)\\'" . html-helper-mode)
                ) auto-mode-alist))
(add-hook 'html-helper-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq intelligent-tab nil)
             ) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;phpモード設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
;;abbrev対応
(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))
;;TAB幅指定
(add-hook 'php-mode-user-hook
          '(lambda ()
             (setq tab-width 2)
             (setq indent-tabs-mode nil))
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;cssモード設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'css-mode "css-mode" "CSS mode" t)
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;pythonモード設定;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;メジャーモードの自動起動設定
(autoload 'python-mode "python-mode" "Major mode for editing Python programs" t)
(autoload 'py-shell "python-mode" "Python shell" t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
;;インデントをスペースに
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil))))

;;ipython設定
;;(setq ipython-command "/usr/local/bin/ipython")
;;(require 'ipython)
             
;;pycompleteの有効化
;;なぜかうまくいかない
;; (add-hook 'python-mode-hook '(lambda ()
;;                                (require 'pycomplete)
;;                                ))

;;pysmellの有効化
;;うまくいかない
;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;perlモードの設定;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;perlモードではなくcperl-modeを使う
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))


(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-label-offset -4
      cperl-tab-always-indent nil
      cperl-font-lock t)
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil)
               (local-set-key "\C-c\C-hm" 'perldoc-m)
               (local-set-key "\C-cs" 'perl-syntax-check)
               (set-perl5lib)
               )))
(add-hook 'cperl-mode-hook
          (lambda ()
            (set-face-bold-p 'cperl-array-face nil)
            (set-face-background 'cperl-array-face "black")
            (set-face-bold-p 'cperl-hash-face nil)
            (set-face-italic-p 'cperl-hash-face nil)
            (set-face-background 'cperl-hash-face "black")
            ))

;; (add-hook 'cperl-mode-hook 'flymake-perl-load)

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;psgmlモードの設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dot emacs for psgml
;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;; (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.html$" . xml-mode)
;;          ("\\.xhtml$" . xml-mode))
;;        auto-mode-alist))

;; ;;; カタログファイルの指定
;; (setq sgml-catalog-files '("~/.emacs.d/DTD/xhtml11-20010531/DTD/xhtml11.cat"))

;; ;;; DOCTYPE 宣言の設定
;; (setq sgml-custom-dtd
;;       '(
;;         ("XHTML 1.1"
;;          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
;;                       \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
;;         ))

;; ;;; hookで変数をsetq
;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (setq tab-width                             2
;;                   sgml-indent-step                      2
;;                   indent-tabs-mode                      nil
;;                   sgml-xml-p                            t
;;                   sgml-always-quote-attributes          t
;;                   sgml-system-identifiers-are-preferred t
;;                   sgml-auto-activate-dtd                t
;;                   sgml-recompile-out-of-date-cdtd       t
;;                   sgml-auto-insert-required-elements    t
;;                   sgml-insert-missing-element-comment   t
;;                   sgml-balanced-tag-edit                t
;;                   sgml-default-doctype-name             "XHTML 1.1"
;;                   sgml-ecat-files                       nil
;;                   sgml-general-insert-case              'lower
;;                   sgml-entity-insert-case               'lower
;;                   sgml-normalize-trims                  t
;;                   sgml-insert-defaulted-attributes      nil
;;                   sgml-live-element-indicator           t
;;                   sgml-active-dtd-indicator             t
;;                   sgml-minimize-attributes              nil
;;                   sgml-omittag                          nil
;;                   sgml-omittag-transparent              nil
;;                   sgml-shorttag                         nil
;;                   sgml-tag-region-if-active             t
;;                   sgml-xml-validate-command             "xmllint --noout --valid %s %s"
;;                   )
;;             ))

;; ;; これ以下はお好みで

;; ;;; font-lock
;; (font-lock-mode 1)
;; (setq font-lock-support-mode   'jit-lock-mode
;;       jit-lock-stealth-verbose nil
;;       font-lock-verbose nil)

;; ;;;;; PSGML デフォルトのfont-lockを使う場合
;; ;;(setq sgml-set-face t
;; ;;    sgml-markup-faces '((start-tag  . font-lock-builtin-face)
;; ;;                          (end-tag    . font-lock-builtin-face)
;; ;;                          (ms-start   . font-lock-variable-name-face)
;; ;;                          (ms-end     . font-lock-variable-name-face)
;; ;;                          (comment    . font-lock-comment-face)
;; ;;                          (ignored    . font-lock-warning-face)
;; ;;                          (pi         . font-lock-preprocessor-face)
;; ;;                          (sgml       . font-lock-type-face)
;; ;;                          (doctype    . font-lock-constant-face)
;; ;;                          (entity     . font-lock-string-face)
;; ;;                          (shortref   . font-lock-reference-face)))

;; ;;; My original font-lock-keywords
;; (add-hook 'sgml-mode-hook
;;           '(lambda ()
;;              (make-local-variable 'font-lock-defaults)
;;              (setq sgml-set-face nil
;;                    font-lock-defaults '(xml-font-lock-keywords-2 nil))
;;              (turn-on-font-lock)
;;              ))

;; (defvar xml-font-lock-keywords-1
;;   (list
;;    ;; タグ開始区切子 & タグ終了区切子
;;    '("<\\|>" 0 font-lock-keyword-face t)
;;    ;; スラッシュ
;;    '("\\(/\\)>" 1 font-lock-keyword-face t)
;;    '("<\\(/\\)" 1 font-lock-keyword-face t)
;;    ;; 要素名
;;    '("\\(</?\\)\\([a-zA-Z]+[a-zA-Z0-9-_:]*\\)" 2  font-lock-builtin-face t)
;;    ;; コメント
;;    '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)" 1 font-lock-comment-face t)
;;    ;; 命令処理
;;    '("\\(<\\?[a-zA-Z]*\\>[^<>]*\\(<[^>]*>[^<>]*\\)*\\?>\\)" 1 font-lock-type-face t)
;;    ;; DOCTYPE, ENTITY, ATTLIST, NOTATION等々 マーク宣言
;;    '("\\(<![a-zA-Z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)" 1 font-lock-constant-face t)
;;    ;; °
;;    '("\\<\\([a-zA-Z]+[a-zA-Z-_:]*\\)=" 1 font-lock-variable-name-face t)
;;    ;; 属性値
;;    '("=?\\(\"[^\"]*\"\\|'[^\']*'\\)" 1 font-lock-string-face t)
;;    ;; 数値文字参照, 文字実体参照, パラメータ実体参照
;;    '("\\(&#[0-9]+;\\|&[a-zA-Z]+;\\|%[^'\";]+;\\)" 1 font-lock-reference-face t)
;;    ;; CDATA 等々 マーク区間 (マーク指定区域)
;;    '("\\(<!\\[[^\\[]+\\[[^]]+]]>\\)" 1 font-lock-warning-face t)
;;    ))

;; (defvar xml-font-lock-keywords-2
;;   (append
;;    xml-font-lock-keywords-1
;;    (list
;;     ;; SSI
;;     `(,(concat "\\(<!--#\\(fsize\\|flastmod\\|printenv\\|"
;;                "include\\|echo\\|config\\|exec\\|set\\|"
;;                "if\\|elif\\|else\\|endif\\)\\>[ \t\n]+"
;;                "\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)")
;;       1 'bold t)
;;     ;; php
;;     '("\\(<\\?\\(php\\|=\\)[^?>]+\\?>\\)" 1 font-lock-function-name-face t)
;;     ;; eRuby, JSP, ASP
;;     '("\\(<%\\(=\\)?\\>[^%>]+%>\\)" 1 font-lock-function-name-face t)
;;     )))



;;yamlモード
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq yaml-indent-offset 4)
          ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;javascriptモード;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; fixing indentation
; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)
